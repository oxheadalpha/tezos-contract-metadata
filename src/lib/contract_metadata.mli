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
open! Import

module Uri : sig
  module Fetcher : sig
    type gateway = {main: string; alternate: string}
    type t = {gateway: gateway}

    val create : unit -> t
  end

  val fetch :
       ?limit_bytes:int
    -> ?prefix:string
    -> < nodes: Query_nodes.Node.t list ; .. > Context.t
    -> Metadata_uri.t
    -> current_contract:string option
    -> Http_client.result Lwt.t

  val needs_context_address : Metadata_uri.t -> bool
end

module Content : sig
  val of_json :
       string
    -> ( [`Fixed_legacy of string * string] list * Metadata_contents.t
       , Tezos_error_monad.Error_monad.tztrace )
       Result.t
  (** Create a contract from json. Return the contract and, if there are any
      legacy (kebab-case instead of CamelCase) keys, a list of warnings about
      those keys. The legacy keys are from an earlier version of TZIP-016 and
      should not be seen very often. *)
end
