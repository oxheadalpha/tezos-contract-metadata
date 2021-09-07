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

type hash_kind = [`Sha256]

type t =
  | Web of string
  | Ipfs of {cid: string; path: string}
  | Storage of {network: string option; address: string option; key: string}
  | Hash of {kind: hash_kind; value: string; target: t}

module Parsing_error = struct
  type error_kind =
    | Wrong_scheme of string option
    | Missing_cid_for_ipfs
    | Wrong_tezos_storage_host of string
    | Forbidden_slash_in_tezos_storage_path of string
    | Missing_host_for_hash_uri of hash_kind
    | Wrong_hex_format_for_hash of
        {hash: hash_kind; host: string; message: string}

  type t = {input: string; error_kind: error_kind}

  let pp ppf {input; error_kind} =
    let open Fmt in
    let err_fmt = function
      | Wrong_scheme None -> const text "Missing URI scheme"
      | Wrong_scheme (Some s) -> kstr (const text) "Unknown URI scheme: %S" s
      | Missing_cid_for_ipfs -> const text "Missing CID in ipfs:// URI"
      | Wrong_tezos_storage_host s ->
          kstr (const text) "Wrong host for tezos-storage: %S" s
      | Forbidden_slash_in_tezos_storage_path s ->
          kstr (const text) "Forbidden slash in tezos-storage path: %S" s
      | Missing_host_for_hash_uri `Sha256 ->
          const text "Missing hash in sha256:// URI"
      | Wrong_hex_format_for_hash {hash= `Sha256; host; message} ->
          kstr (const text) "Wrong hex-format for sha256:// URI: %S -> %s" host
            message in
    pf ppf "%a"
      (box
         ( const text "Error while parsing"
         ++ const (quote string) input
         ++ const string ":" ++ sp ++ err_fmt error_kind ) )
      ()

  let encoding =
    let open Data_encoding in
    let err_kind =
      let cases = ref [] (* imperative ⇒ proper tag numbers *) in
      let special_case name enc proj find =
        let c =
          case ~title:name
            (Tag (List.length !cases))
            (obj1 (req name enc))
            proj find in
        cases := c :: !cases in
      let hash_kind =
        union
          [ case ~title:"sha256" (Tag 0) (constant "sha256")
              (function `Sha256 -> Some ())
              (function () -> `Sha256) ] in
      special_case "wrong-scheme" (option string)
        (function Wrong_scheme s -> Some s | _ -> None)
        (fun o -> Wrong_scheme o) ;
      special_case "missing-cid" unit
        (function Missing_cid_for_ipfs -> Some () | _ -> None)
        (fun () -> Missing_cid_for_ipfs) ;
      special_case "wrong-tezos-storage-host" string
        (function Wrong_tezos_storage_host s -> Some s | _ -> None)
        (fun o -> Wrong_tezos_storage_host o) ;
      special_case "slash-in-tezos-storage-path" string
        (function
          | Forbidden_slash_in_tezos_storage_path s -> Some s | _ -> None )
        (fun o -> Forbidden_slash_in_tezos_storage_path o) ;
      special_case "missing-host-for-hash-uri" hash_kind
        (function Missing_host_for_hash_uri s -> Some s | _ -> None)
        (fun o -> Missing_host_for_hash_uri o) ;
      special_case "wrong-hex-format-for-hash"
        (obj3 (req "hash" hash_kind) (req "host" string) (req "message" string))
        (function
          | Wrong_hex_format_for_hash {hash; host; message} ->
              Some (hash, host, message)
          | _ -> None )
        (fun (hash, host, message) ->
          Wrong_hex_format_for_hash {hash; host; message} ) ;
      union (List.rev !cases) in
    conv
      (fun {input; error_kind} -> (input, error_kind))
      (fun (input, error_kind) -> {input; error_kind})
      (obj2 (req "input" string) (req "kind" err_kind))
end

open Error_monad

type error += Contract_metadata_uri_parsing of Parsing_error.t

let () =
  register_error_kind `Permanent ~id:"contract_metadata.uri.parsing_error"
    ~title:"Contract Metadata Parsing Error"
    ~description:"An error occurred while parsing a contract metadata URI."
    ~pp:Parsing_error.pp Parsing_error.encoding
    (function Contract_metadata_uri_parsing err -> Some err | _ -> None)
    (fun err -> Contract_metadata_uri_parsing err)

type field_validation =
     string
  -> ( unit
     , Tezos_error_monad.Error_monad.error
       Tezos_error_monad.Error_monad.TzTrace.trace )
     result

let rec of_uri ?validate_network ?validate_kt1_address uri =
  let open Uri in
  let open Parsing_error in
  let fail error_kind =
    error (Contract_metadata_uri_parsing {input= Uri.to_string uri; error_kind})
  in
  let remove_first_slash s =
    match Tezos_stdlib.TzString.remove_prefix s ~prefix:"/" with
    | Some cleaned -> cleaned
    | None -> s in
  match scheme uri with
  | None -> fail (Wrong_scheme None)
  | Some "https" | Some "http" -> ok (Web (to_string uri))
  | Some "ipfs" -> (
      let path = path uri in
      match host uri with
      | None -> fail Missing_cid_for_ipfs
      | Some cid -> ok (Ipfs {cid; path}) )
  | Some "tezos-storage" -> (
      ( match host uri with
      | None -> ok (None, None)
      | Some s -> (
        match Tezos_stdlib.TzString.split '.' s with
        | [one] -> ok (None, Some one)
        | [one; two] -> ok (Some two, Some one)
        | _ -> fail (Wrong_tezos_storage_host s) ) )
      >>? fun (network, address) ->
      let validate_option f v =
        match (f, v) with
        | Some (validate : field_validation), Some value -> validate value
        | _, _ -> ok () in
      validate_option validate_network network
      >>? fun () ->
      validate_option validate_kt1_address address
      >>? fun () ->
      match remove_first_slash (path uri) with
      | k when String.contains k '/' ->
          fail (Forbidden_slash_in_tezos_storage_path k)
      | k ->
          let key = Uri.pct_decode k in
          ok (Storage {network; address; key}) )
  | Some "sha256" -> (
    match host uri with
    | None -> fail (Missing_host_for_hash_uri `Sha256)
    | Some host -> (
        let fail_m message =
          fail (Wrong_hex_format_for_hash {hash= `Sha256; host; message}) in
        match Tezos_stdlib.TzString.remove_prefix host ~prefix:"0x" with
        | Some hex -> (
          match Hex.to_string (`Hex hex) with
          | value ->
              of_uri
                ( Uri.path uri |> remove_first_slash |> Uri.pct_decode
                |> Uri.of_string )
              >>? fun target -> ok (Hash {kind= `Sha256; value; target})
          | exception exn ->
              Fmt.kstr fail_m "Hex-to-string conversion error: %s"
                ( match exn with
                | Invalid_argument ia -> ia
                | other -> Printexc.to_string other ) )
        | None -> Fmt.kstr fail_m "Host does not start with 0x" ) )
  | Some s -> fail (Wrong_scheme (Some s))

let rec to_string_uri = function
  | Web s -> s
  | Ipfs {cid; path} -> Fmt.str "ipfs://%s%s" cid path
  | Storage {network; address; key} ->
      Fmt.str "tezos-storage:%s%s"
        ( match (network, address) with
        | None, None -> ""
        | Some n, Some a -> Fmt.str "//%s.%s/" a n
        | Some x, None | None, Some x -> Fmt.str "//%s/" x )
        (Uri.pct_encode key)
  | Hash {kind= `Sha256; value; target} ->
      Fmt.str "sha256://0x%s/%s"
        (let (`Hex hx) = Hex.of_string value in
         hx )
        (Uri.pct_encode (to_string_uri target))

let rec pp ppf (t : t) =
  let open Fmt in
  match t with
  | Web s -> pf ppf "Web-URL: %s" s
  | Ipfs {cid; path} -> pf ppf "IPFS-URI: CID: %s PATH: %s" cid path
  | Storage {network; address; key} ->
      pf ppf "On-chain: %a.%a @%S"
        (option ~none:(const string "current-network") string)
        network
        (option ~none:(const string "current-contract") string)
        address key
  | Hash {kind; value; target} ->
      pf ppf "%s(%a) = %s"
        (match kind with `Sha256 -> "sha256")
        pp target value
