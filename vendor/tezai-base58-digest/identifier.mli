(** Tezos common identifiers. *)

module type Base58_identifier = sig
  val prefix : string
  val encode : string -> Raw.base58
  val decode : Raw.base58 -> string
end

module type Base58_hash_identifier = sig
  include Base58_identifier

  val size : int
  val hash_string : string -> string

  val check : Raw.base58 -> unit
  (** Decode the base58-hash in various steps while trying to fail
      with the most precise error message. *)
end

module Block_hash : Base58_hash_identifier

module Chain_id : sig
  include Base58_hash_identifier

  val of_base58_block_hash : Raw.base58 -> Raw.base58
end

module Operation_hash : Base58_hash_identifier
module Script_expr_hash : Base58_hash_identifier

module Kt1_address : sig
  include Base58_hash_identifier

  val of_base58_operation_hash : ?index:int32 -> Raw.base58 -> Raw.base58
end

module Ed25519 : sig
  module Secret_key : Base58_identifier
  module Public_key : Base58_identifier

  module Public_key_hash : sig
    include Base58_hash_identifier
  end

  module Signature : Base58_identifier
end
