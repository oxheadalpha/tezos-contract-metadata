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

exception Error of string

let%expect_test "Fetch url test" =
  let nodes = Query_nodes.Node_list.nodes (Query_nodes.get_default_nodes ()) in
  let ctxt =
    object (self)
      method formatter = Caml.Format.str_formatter
      method log_context = ""

      method http_client : Http_client.t =
        { get=
            (fun ?limit_bytes uri ->
              Lwt.return
                (Ok
                   ( "get uri: " ^ uri ^ " limit "
                   ^ Int.to_string (Option.value limit_bytes ~default:(-1)) ) )
              )
        ; post=
            (fun ~headers ~body uri ->
              let _ = headers in
              Lwt.return (Ok ("post uri: " ^ uri ^ " body " ^ body)) )
        }

      method nodes = nodes
      method program_time = 0.0
      method with_log_context _prefix = self (* don't care *)
    end in
  let uri_res = Metadata_uri.of_uri (Uri.of_string "https://example.com") in
  let uri =
    match uri_res with Ok u -> u | _ -> raise (Error "failed to parse uril")
  in
  let open Lwt.Infix in
  let test =
    Contract_metadata.Uri.fetch ctxt uri ~current_contract:None
    >>= fun fetch_result ->
    match fetch_result with
    | Ok s -> Stdio.print_endline s ; Lwt.return ()
    | _ -> Stdio.print_endline "fail" ; Lwt.return () in
  Lwt_main.run test ; [%expect {|
    get uri: https://example.com limit -1 |}]
