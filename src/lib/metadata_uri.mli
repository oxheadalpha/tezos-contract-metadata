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

(** Implementation of the TZIP-16 metadata URIs. *)

type hash_kind = [`Sha256]

(** The type for {i parsed} metadata URIs. *)
type t =
  | Web of string  (** A web-URI is an ["http://"] or ["https://"] URL. *)
  | Ipfs of {cid: string; path: string}  (** An IPFS URI. *)
  | Storage of {network: string option; address: string option; key: string}
      (** A URI pointing inside a contract's storage. *)
  | Hash of {kind: hash_kind; value: string; target: t}
      (** A ["sha256://0xdeadbeef/<target-uri>"] checked URI. *)

module Parsing_error : sig
  type error_kind =
    | Wrong_scheme of string option
    | Missing_cid_for_ipfs
    | Wrong_tezos_storage_host of string
    | Forbidden_slash_in_tezos_storage_path of string
    | Missing_host_for_hash_uri of hash_kind
    | Wrong_hex_format_for_hash of
        {hash: hash_kind; host: string; message: string}

  type t = {input: string; error_kind: error_kind}

  val pp : Format.formatter -> t -> unit
  val encoding : t Data_encoding.encoding
end

type Error_monad.error += Contract_metadata_uri_parsing of Parsing_error.t

type field_validation =
     string
  -> ( unit
     , Tezos_error_monad.Error_monad.error
       Tezos_error_monad.Error_monad.TzTrace.trace )
     result

val of_uri :
     ?validate_network:field_validation
  -> ?validate_kt1_address:field_validation
  -> Uri.t
  -> ( t
     , Tezos_error_monad.Error_monad.error
       Tezos_error_monad.Error_monad.TzTrace.trace )
     result
(** Parse a metadata URI, validation of the network and address fields is left
    optional. *)

val to_string_uri : t -> string
(** Make a parsable URI. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a URI. *)
