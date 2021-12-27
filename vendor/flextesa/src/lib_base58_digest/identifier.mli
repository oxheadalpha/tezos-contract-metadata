(** Tezos common identifiers. *)

module Block_hash : sig
  val prefix : string
  val size : int
  val hash_string : string -> string
  val encode : string -> Raw.base58
  val decode : Raw.base58 -> string
end

module Chain_id : sig
  val prefix : string
  val size : int
  val hash_string : string -> string
  val encode : string -> Raw.base58
  val decode : Raw.base58 -> string
  val of_base58_block_hash : Raw.base58 -> Raw.base58
end

module Operation_hash : sig
  val prefix : string
  val size : int
  val hash_string : string -> string
  val encode : string -> Raw.base58
  val decode : Raw.base58 -> string
end

module Kt1_address : sig
  val prefix : string
  val size : int
  val hash_string : string -> string
  val encode : string -> Raw.base58
  val decode : Raw.base58 -> string
  val of_base58_operation_hash : ?index:int32 -> Raw.base58 -> Raw.base58
end

module Ed25519 : sig
  module Secret_key : sig
    val prefix : string
    val encode : string -> Tezai_base58_digest__Raw.base58
    val decode : Tezai_base58_digest__Raw.base58 -> string
  end

  module Public_key : sig
    val prefix : string
    val encode : string -> Tezai_base58_digest__Raw.base58
    val decode : Tezai_base58_digest__Raw.base58 -> string
  end

  module Public_key_hash : sig
    val size : int
    val hash_string : string -> string
    val prefix : string
    val encode : string -> Tezai_base58_digest__Raw.base58
    val decode : Tezai_base58_digest__Raw.base58 -> string
  end

  module Signature : sig
    val prefix : string
    val encode : string -> Tezai_base58_digest__Raw.base58
    val decode : Tezai_base58_digest__Raw.base58 -> string
  end
end
