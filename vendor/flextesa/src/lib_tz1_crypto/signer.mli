module Secret_key : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val to_base58 : t -> Tezai_base58_digest__Raw.base58
  val of_base58 : Tezai_base58_digest__Raw.base58 -> t
  val pp : Format.formatter -> t -> unit
  val of_seed : string -> t
end

module Public_key : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val to_base58 : t -> Tezai_base58_digest__Raw.base58
  val of_base58 : Tezai_base58_digest__Raw.base58 -> t
  val pp : Format.formatter -> t -> unit
  val of_secret_key : Secret_key.t -> t
end

module Public_key_hash : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val to_base58 : t -> Tezai_base58_digest__Raw.base58
  val of_base58 : Tezai_base58_digest__Raw.base58 -> t
  val pp : Format.formatter -> t -> unit
  val of_public_key : Public_key.t -> t
end
