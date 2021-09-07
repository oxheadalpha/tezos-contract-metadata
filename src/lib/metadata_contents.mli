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
open Tezos_error_monad

(** Implementation of the TZIP-16 metadata "content", a.k.a. JSON blob. *)

(** This module defines types, their corresponding {!Json_encoding.t} values,
    pretty-printers and some generative examples.

    The type [t] corresponds to the top-level fields of the metadata JSON
    (section "Reserved Fields" in the specification), it's the root for all the
    other submodules and it's encoding is used to generate the JSON-Schema. *)

module License : sig
  type t = {name: string; details: string option}

  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
end

module Michelson_blob : sig
  type t = Micheline of string Tezos_micheline.Micheline.canonical

  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
end

module View : sig
  module Implementation : sig
    module Michelson_storage : sig
      type t =
        { parameter: Michelson_blob.t option
        ; return_type: Michelson_blob.t
        ; code: Michelson_blob.t
        ; human_annotations: (string * string) list
        ; version: string option }
    end

    module Rest_api_query : sig
      type t =
        { specification_uri: string
        ; base_uri: string option
        ; path: string
        ; meth: Cohttp.Code.meth }
    end

    type t =
      | Michelson_storage of Michelson_storage.t
      | Rest_api_query of Rest_api_query.t

    val michelson_storage :
         ?parameter:Michelson_blob.t
      -> return_type:Michelson_blob.t
      -> ?annotations:(string * string) list
      -> ?version:string
      -> Michelson_blob.t
      -> t

    val rest_api_query :
      ?base_uri:string -> ?meth:Cohttp.Code.meth -> string -> string -> t

    val pp : ?with_code:bool -> Format.formatter -> t -> unit
    val encoding : t Json_encoding.encoding

    module Example : sig
      val build : int -> t
    end
  end

  type t =
    { name: string
    ; description: string option
    ; implementations: Implementation.t list
    ; is_pure: bool }

  val make :
    ?description:string -> ?is_pure:bool -> string -> Implementation.t list -> t

  val pp : ?with_code:bool -> Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding

  module Example : sig
    val build : int -> t
  end
end

module Source : sig
  type t = {tools: string list; location: string option}

  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
end

module Errors : sig
  module Translation : sig
    type t =
      | Static of
          { error: Michelson_blob.t
          ; expansion: Michelson_blob.t
          ; languages: string list option }
      | Dynamic of {view_name: string; languages: string list option}

    val pp : Format.formatter -> t -> unit
    val encoding : t Json_encoding.encoding
  end

  type t = Translation.t list

  val pp : Format.formatter -> Translation.t list -> unit
  val encoding : Translation.t list Json_encoding.encoding
end

type t =
  { name: string option
  ; description: string option
  ; version: string option
  ; license: License.t option
  ; authors: string list
  ; homepage: string option
  ; source: Source.t option
  ; interfaces: string list
  ; errors: Errors.t option
  ; views: View.t list
  ; unknown: (string * Ezjsonm.value) list }

val make :
     ?name:string
  -> ?description:string
  -> ?version:string
  -> ?license:License.t
  -> ?authors:string list
  -> ?homepage:string
  -> ?source:Source.t
  -> ?interfaces:string list
  -> ?errors:Errors.t
  -> ?extras:(string * Ezjsonm.value) list
  -> View.t list
  -> t

val pp : Format.formatter -> t -> unit
val pp_short : Format.formatter -> t -> unit
val encoding : t Json_encoding.encoding
val of_json : string -> t Error_monad.tzresult
val to_json : t -> string

module Validation : sig
  module Error : sig
    type t =
      | Forbidden_michelson_instruction of {view: string; instruction: string}
      | Michelson_version_not_a_protocol_hash of {view: string; value: string}

    val pp : Format.formatter -> t -> unit
  end

  module Warning : sig
    type t =
      | Wrong_author_format of string
      | Unexpected_whitespace of {field: string; value: string}
      | Self_unaddressed of {view: string; instruction: string option}

    val pp : Format.formatter -> t -> unit
  end

  module Data : sig
    val author_re : Re.re lazy_t
    val forbidden_michelson_instructions : string list
  end

  val validate :
       ?protocol_hash_is_valid:(string -> bool)
    -> t
    -> Error.t list * Warning.t list
  (** Run the validation on a metadata instance. The default
      [protocol_hash_is_valid] is [(fun _ -> true)], so by default the error
      [Michelson_version_not_a_protocol_hash _] is not reported (for library
      dependency reasons). *)

  val pp : Format.formatter -> Error.t list * Warning.t list -> unit
end

module Example : sig
  val build : int -> t val all : unit -> t list
end
