(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 TQ Tezos <contact@tqtezos.com>                         *)
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
open Import

module Uri = struct
  module Fetcher = struct
    type gateway = {main: string; alternate: string}
    type t = {gateway: gateway}

    let create () =
      let main = "https://gateway.ipfs.io/ipfs/" in
      let alternate = "https://dweb.link/ipfs/" in
      {gateway= {main; alternate}}

    let fetcher = create ()
    let gateway = fetcher.gateway
  end

  let rec needs_context_address =
    let open Metadata_uri in
    function
    | Storage {address= None; _} -> true
    | Web _ | Storage _ | Ipfs _ -> false
    | Hash {target; _} -> needs_context_address target

  let to_ipfs_gateway ~alt_gateway ~cid ~path =
    let gateway =
      if alt_gateway then Fetcher.gateway.alternate else Fetcher.gateway.main
    in
    Fmt.str "%s%s%s" gateway cid path

  let fetch ?limit_bytes ?prefix (ctxt : 'a Context.t) uri ~current_contract =
    let log =
      match prefix with
      | None -> dbgf ctxt "Uri.fetch.log: %s"
      | Some prefix -> dbgf ctxt "%s: %s" prefix in
    let open Lwt.Infix in
    let logf fmt = Fmt.kstr (fun s -> log s) fmt in
    let not_implemented s = Fmt.failwith "Not Implemented: %s" s in
    dbgf ctxt "Fetching ============== " ;
    let rec resolve =
      let open Metadata_uri in
      function
      | Web http_uri ->
          logf "HTTP %S" http_uri ;
          ctxt#http_client.get ?limit_bytes http_uri
      | Ipfs {cid; path} ->
          logf "IPFS CID %S path %S" cid path ;
          let gatewayed = to_ipfs_gateway ~alt_gateway:false ~cid ~path in
          (* resolve (Web gatewayed) *)
          Lwt.catch
            (fun () -> resolve (Web gatewayed))
            (fun e ->
              dbgf ctxt "Trying alternate IPFS gateway due to exception: %s"
                (Exn.to_string e) ;
              let gatewayed_alt = to_ipfs_gateway ~alt_gateway:true ~cid ~path in
              resolve (Web gatewayed_alt) )
      | Storage {network= None; address; key} ->
          let addr =
            match address with
            | Some s -> s
            | None -> (
              match current_contract with
              | None -> Fmt.failwith "Missing current contract"
              | Some s -> s ) in
          logf "Using address %S (key = %S)" addr key ;
          Query_nodes.metadata_value ctxt ~address:addr ~key
      | Storage {network= Some network; address; key} ->
          logf "storage %s %a %S" network Fmt.Dump.(option string) address key ;
          Fmt.kstr not_implemented "storage uri with network = %s" network
      | Hash {kind= `Sha256; value; target} ->
          let expected =
            match Digestif.of_raw_string_opt Digestif.sha256 value with
            | Some s -> s
            | None ->
                Fmt.failwith "%a is not a valid SHA256 hash" Hex.pp
                  (Hex.of_string value) in
          logf "sha256: %a" (Digestif.pp Digestif.sha256) expected ;
          resolve target
          >>= fun content ->
          Lwt.return
            (Result.map
               ~f:(fun content ->
                 let obtained = Digestif.digest_string Digestif.sha256 content in
                 logf "hash of content: %a"
                   (Digestif.pp Digestif.sha256)
                   obtained ;
                 match
                   Digestif.unsafe_compare Digestif.sha256 expected obtained
                 with
                 | 0 -> content
                 | _ ->
                     Fmt.failwith
                       "Hash of content %a is different from expected %a"
                       (Digestif.pp Digestif.sha256)
                       obtained
                       (Digestif.pp Digestif.sha256)
                       expected )
               content ) in
    resolve uri
end

module Content = struct
  let of_json s =
    try
      let warnings = ref [] in
      let jsonm =
        let j = Ezjsonm.value_from_string s in
        let rec fix = function
          | (`String _ | `Float _ | `Bool _ | `Null) as v -> v
          | `A l -> `A (List.map l ~f:fix)
          | `O kvl ->
              let f (k, v) =
                let fix_warn o k =
                  ( match
                      List.exists !warnings ~f:(function
                          | `Fixed_legacy (a, _) -> String.equal a o )
                    with
                  | true -> ()
                  | false -> warnings := `Fixed_legacy (o, k) :: !warnings ) ;
                  (k, fix v) in
                match k with
                | "michelson-storage-view" -> fix_warn k "michelsonStorageView"
                | "return-type" -> fix_warn k "returnType"
                | other -> (other, fix v) in
              `O (List.map kvl ~f) in
        fix j in
      let contents = Json_encoding.destruct Metadata_contents.encoding jsonm in
      Ok (!warnings, contents)
    with e -> Tezos_error_monad.Error_monad.error_exn e
end
