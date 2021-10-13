(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 TQ Tezos <contact@tqtezos.com>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open! Base
open Tezos_contract_metadata
open Tezos_contract_metadata.Import
open Cmdliner
open Stdio

type text_length = Full | Short
type output_format = Text of text_length | Json | Raw

let string_of_output_format fmt =
  match fmt with
  | Text Full -> "text:full"
  | Text Short -> "text:short"
  | Json -> "json"
  | Raw -> "raw"

let validate_address input_value =
  match B58_hashes.check_b58_kt1_hash input_value with
  | _ -> `KT1 input_value
  | exception _ when String.is_prefix input_value ~prefix:"KT" ->
      `Error
        ( input_value
        , [Tezos_error_monad.Error_monad.failure "Invalid KT1 address"] )
  | exception _ -> (
    match Metadata_uri.of_uri (Uri.of_string input_value) with
    | Ok uri -> `Uri (input_value, uri)
    | Error e -> `Error (input_value, e) )

let on_uri ctxt uri ~address =
  let open Lwt in
  let ( >>= ) = Lwt_result.bind in
  catch
    (fun () ->
      Contract_metadata.Uri.fetch ctxt uri ~prefix:"Fetching Metadata "
        ~current_contract:address )
    (fun e -> raise (Exn.reraise e "Failed to fetch metadata"))
  >>= fun json_code ->
  dbgf ctxt "before of-json" ;
  match Contract_metadata.Content.of_json json_code with
  | Ok (warnings, contents) ->
      (*
                Async_work.ok result
                  (uri_and_metadata_result ctxt ~full_input ~uri
                     ?token_metadata_big_map ~metadata:json_code ) ;
                Lwt.return ()
                *)
      return (Ok (warnings, contents))
  | Error trace ->
      (*
                raise
                  (mkexn
                     (uri_ok_but_metadata_failure ctxt ~uri ~full_input
                        ~metadata_json:json_code ~error ) )*)
      (*return None *)
      return
        (Fmt.kstr Http_client.failure "Failed to retrieve contract %s %a"
           (Metadata_uri.to_string_uri uri)
           Tezos_error_monad.Error_monad.pp_print_error trace )

let fetch_contract_metadata ctxt src =
  let full_input = validate_address src in
  let ( >>= ) = Lwt_result.bind in
  match full_input with
  | `KT1 address -> (
      Query_nodes.metadata_value
        (ctxt#with_log_context "Getting URI")
        ~address ~key:""
      >>= fun metadata_uri ->
      match Metadata_uri.of_uri (Uri.of_string metadata_uri) with
      | Ok uri -> on_uri ctxt uri ~address:(Some address)
      | Error errors ->
          Fmt.kstr Lwt.fail_with "Wrong url %s %a" metadata_uri
            Tezos_error_monad.Error_monad.pp_print_error errors )
  | `Uri (_, uri) ->
      if Contract_metadata.Uri.needs_context_address uri then
        dbgf ctxt "This URI requires a context KT1 address …" ;
      on_uri ctxt uri ~address:None
  | `Error (_address, trace) ->
      Fmt.kstr Lwt.fail_with "wrong type: %a"
        Tezos_error_monad.Error_monad.pp_print_error trace

let with_timeout ctxt ~f ~raise =
  let open Lwt.Infix in
  let timeout = ctxt#http_timeout in
  Lwt.pick [f (); (ctxt#sleep timeout >>= fun () -> raise timeout)]

let http_with_timeout ctxt ~method_name http_method uri =
  let open Lwt in
  dbgf ctxt "get uri %S" uri ;
  with_timeout ctxt
    ~raise:(fun timeout ->
      return
        (Fmt.kstr Http_client.failure "HTTP Call timed out: %.3f s" timeout) )
    ~f:(fun () ->
      http_method (Uri.of_string uri)
      >>= fun (resp, body) ->
      Cohttp_lwt.Body.to_string body
      >>= fun content ->
      match Cohttp.Response.status resp with
      | `OK ->
          dbgf ctxt "response ok %d"
            (resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status) ;
          return (Ok content)
      | _ ->
          let code =
            resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
          dbgf ctxt "response bad %d" code ;
          return
            (Fmt.kstr Http_client.failure
               "Wrong HTTP status from http %s of %s: %d" method_name uri code )
      )

let show_metadata src format debug =
  let ctxt =
    let nodes = Query_nodes.Node_list.nodes (Query_nodes.get_default_nodes ()) in
    let now () = Unix.gettimeofday () /. 1000. in
    let time_zero = now () in
    let null_formatter =
      Caml.Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
    let formatter = if debug then Fmt.stderr else null_formatter in
    object (self)
      val prefix = ""
      method nodes = nodes
      method formatter = formatter
      method sleep = Lwt_unix.sleep
      method http_timeout = 5.0
      method program_time = now () -. time_zero

      method http_get ?limit_bytes uri =
        let headers =
          Option.map limit_bytes ~f:(fun b ->
              Cohttp.Header.of_list [("Range", Fmt.str "bytes=0-%d" b)] ) in
        http_with_timeout self ~method_name:"get"
          (fun uri -> Cohttp_lwt_unix.Client.get ?headers uri)
          uri

      method http_post ~headers ~body uri =
        http_with_timeout self ~method_name:"post"
          (fun uri ->
            Cohttp_lwt_unix.Client.post ~body:(`String body) ~headers uri )
          uri

      method http_client : Http_client.t =
        { get= self#http_get; post= self#http_post }

      method with_log_context new_prefix = {<prefix = new_prefix ^ " " ^ prefix>}
      method log_context = prefix
    end in
  let ( >>= ) = Lwt_result.bind in
  let result =
    Lwt_main.run
      ( fetch_contract_metadata ctxt src
      >>= fun result ->
      (let _warnings, contents = result in
       match format with
       | Text Full ->
           Metadata_contents.pp Caml.Format.std_formatter contents ;
           print_endline ""
       | Text Short ->
           Metadata_contents.pp_short Caml.Format.std_formatter contents ;
           print_endline ""
       | Raw -> () (* fixme *)
       | Json -> print_endline (Metadata_contents.to_json contents) ) ;
      Lwt.return (Ok 0) ) in
  match result with
  | Ok num -> num
  | Error e ->
      Fmt.pr "%a\n" Http_client.pp_http_error e ;
      1

(* CLI *)
let metadata_format =
  let doc = "metadata format" in
  let format =
    Arg.enum
      [ ("text", Text Full)
      ; ("text:full", Text Full)
      ; ("text:short", Text Short)
      ; ("raw", Raw)
      ; ("json", Json) ] in
  Arg.(value & opt format (Text Full) & info ["format"] ~docv:"FORMAT" ~doc)

let debug =
  let doc = "Debugging output to stderr." in
  let yes = (true, Arg.info ["debug"] ~doc) in
  Arg.(last & vflag_all [false] [yes])

let src =
  let doc =
    "source: the URL of a contract, or a KT1 address available via mainnet or \
     another known network" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)

let show_metadata_t = Term.(const show_metadata $ src $ metadata_format $ debug)

let info =
  let doc = "Show TZIP-16 metadata" in
  let man =
    [ `S Manpage.s_bugs
    ; `P
        "File bug reports at \
         https://github.com/oxheadalpha/tezos-contract-metadata" ] in
  Term.info "tezos-contract-metadata" ~version:"%‌%VERSION%%" ~doc
    ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (show_metadata_t, info)
