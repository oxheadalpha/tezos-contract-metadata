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

(** Contract-storage-parsing helper functions for the implementation of TZIP-16. *)

val get_storage_type_exn :
     string Tezos_micheline.Micheline.canonical
  -> ( Tezos_micheline.Micheline.canonical_location
     , string )
     Tezos_micheline.Micheline.node
(** Find the ["storage"] section of a Micheline-encoded Michelson contract.

    @raises [Failure _\] if not found. *)

val get_parameter_type_exn :
     string Tezos_micheline.Micheline.canonical
  -> ( Tezos_micheline.Micheline.canonical_location
     , string )
     Tezos_micheline.Micheline.node
(** Find the ["parameter"] section of a Micheline-encoded Michelson contract.

    @raises [Failure _\] if not found. *)

val pp_arbitrary_micheline :
  Format.formatter -> ('a, string) Tezos_micheline.Micheline.node -> unit
(** Pretty-print a piece of Micheline regardless of the location type. *)

val find_metadata_big_maps :
     storage_node:('a, string) Tezos_micheline.Micheline.node
  -> type_node:('b, string) Tezos_micheline.Micheline.node
  -> Z.t list
(** Assuming that [storage_node] is the storage expression of a contract has
    type [type_node], find the identifier of metadata-big-map according to the
    TZIP-16 specification. *)

val build_off_chain_view_contract :
     Metadata_contents.View.Implementation.Michelson_storage.t
  -> contract_balance:Z.t
  -> contract_address:string
  -> contract_storage_type:(int, string) Tezos_micheline.Micheline.node
  -> contract_parameter_type:(int, string) Tezos_micheline.Micheline.node
  -> view_parameters:(int, string) Tezos_micheline.Micheline.node
  -> contract_storage:(int, string) Tezos_micheline.Micheline.node
  -> [`Contract of (int, string) Tezos_micheline.Micheline.node]
     * [`Input of (int, string) Tezos_micheline.Micheline.node]
     * [`Storage of (int, string) Tezos_micheline.Micheline.node]
(** Build a contract for the [".../run_script"] RPC of the node. *)
