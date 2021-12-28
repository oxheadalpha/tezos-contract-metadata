(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type CRYPTO = sig
  val sha256 : string -> string
end

module Alphabet : sig
  type t

  val bitcoin : t
  val ripple : t
  val flickr : t

  val default : t
  (** [default] is [bitcoin]. *)

  val all_characters : t -> string
  (** Return a string containing all the characters of that alphabet. *)
end

type t = [`Base58 of string]

(** Type of Base58Check encoded data. *)
type base58 = t

val compare : t -> t -> int
val equal : t -> t -> bool
val ( = ) : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val of_bytes : ?alphabet:Alphabet.t -> (module CRYPTO) -> string -> t
(** [of_bytes ?alphabet bytes] is the Base58Check encoding of [bytes] using
    alphabet [?alphabet]. *)

val to_bytes : ?alphabet:Alphabet.t -> (module CRYPTO) -> t -> string option
(** [to_bytes ?alphabet t] is [Some data] if [t] is a valid Base58Check encoding
    of [data] (with correct checksum), or [None] otherwise. *)

val to_bytes_exn : ?alphabet:Alphabet.t -> (module CRYPTO) -> t -> string
(** See [to_bytes].

    @raises [Invalid_argument] on checksum failure. *)

val of_string : ?alphabet:Alphabet.t -> (module CRYPTO) -> string -> t option
(** [of_string b58] is [`Base58 b58] if b58 is a valid Base58Check encoding. *)

val of_string_exn : ?alphabet:Alphabet.t -> (module CRYPTO) -> string -> t
(** See [of_string].

    @raises [Invalid_argument] if the first argument is not a valid
    Base58Check encoding. *)

val to_string : t -> string
(** [to_string [`Base58 b58] is [b58]. *)

(** {1 Tezos prefixes} *)

module Tezos : sig
  type version =
    | Block
    | Operation
    | Protocol
    | Address
    | Peer
    | Public_key
    | Secret_key
    | Signature

  type t = private {version: version; payload: string}

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : (module CRYPTO) -> Format.formatter -> t -> unit
  val show : (module CRYPTO) -> t -> string
  val create : version:version -> payload:string -> t
  val of_base58 : (module CRYPTO) -> base58 -> t option
  val of_base58_exn : (module CRYPTO) -> base58 -> t
  val to_base58 : (module CRYPTO) -> t -> base58
  val of_string : (module CRYPTO) -> string -> t option
  val of_string_exn : (module CRYPTO) -> string -> t
  val to_string : (module CRYPTO) -> t -> string

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t
end

(** {1 Bitcoin, or one-byte prefixes only} *)

module Bitcoin : sig
  type version =
    | P2PKH
    | P2SH
    | Namecoin_P2PKH
    | Privkey
    | BIP32_priv
    | BIP32_pub
    | Testnet_P2PKH
    | Testnet_P2SH
    | Testnet_privkey
    | Testnet_BIP32_priv
    | Testnet_BIP32_pub
    | Unknown of string

  type t = private {version: version; payload: string}

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : (module CRYPTO) -> Format.formatter -> t -> unit
  val show : (module CRYPTO) -> t -> string
  val create : version:version -> payload:string -> t
  val of_base58 : (module CRYPTO) -> base58 -> t option
  val of_base58_exn : (module CRYPTO) -> base58 -> t
  val to_base58 : (module CRYPTO) -> t -> base58
  val of_string : (module CRYPTO) -> string -> t option
  val of_string_exn : (module CRYPTO) -> string -> t
  val to_string : (module CRYPTO) -> t -> string

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t
end

module Komodo : sig
  type version = P2PKH | P2SH | WIF
  type t = private {version: version; payload: string}

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : (module CRYPTO) -> Format.formatter -> t -> unit
  val show : (module CRYPTO) -> t -> string
  val create : version:version -> payload:string -> t
  val of_base58 : (module CRYPTO) -> base58 -> t option
  val of_base58_exn : (module CRYPTO) -> base58 -> t
  val to_base58 : (module CRYPTO) -> t -> base58
  val of_string : (module CRYPTO) -> string -> t option
  val of_string_exn : (module CRYPTO) -> string -> t
  val to_string : (module CRYPTO) -> t -> string

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t
end

module Set : Set.S with type elt := t
module Map : Map.S with type key := t

val raw_encode : ?alphabet:Alphabet.t -> string -> string
