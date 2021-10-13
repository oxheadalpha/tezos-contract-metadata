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

open! Import
open Tezos_micheline

module Partial_type : sig
  module Structure : sig
    type type_kind =
      | Any
      | Nat
      | Mutez
      | Bytes
      | Address
      | Bool
      | String
      | List of type_kind
      | Map of type_kind * type_kind

    type leaf = string

    type t =
      | Leaf of
          { raw: string Tezos_micheline.Micheline.canonical
          ; kind: type_kind
          ; v: leaf
          ; description: (string * string) option }
      | Pair of {left: t; right: t}
  end

  type t =
    { original: string Tezos_micheline.Micheline.canonical
    ; structure: Structure.t }

  val of_type :
       ?annotations:(string * string) list
    -> Metadata_contents.Michelson_blob.t
    -> t

  val micheline_string_bytes_map_exn :
    ('a, string) Micheline.node -> (string * string) list
end

val micheline_of_json : string -> (int, string) Micheline.node
val micheline_to_ezjsonm : ('a, string) Micheline.node -> Data_encoding.json
val micheline_of_ezjsonm : Data_encoding.json -> (int, string) Micheline.node
val micheline_node_to_string : ('a, string) Micheline.node -> string

val parse_micheline :
     check_indentation:bool
  -> check_primitives:bool
  -> string
  -> (Micheline_parser.node, Tezos_error_monad.TzCore.error list) result

val parse_micheline_exn :
     check_indentation:bool
  -> check_primitives:bool
  -> string
  -> Micheline_parser.node
