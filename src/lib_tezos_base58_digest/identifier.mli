module Block_hash : sig
  val prefix : string
  val size : int
  val hash_string : string -> string
  val encode : string -> string
  val decode : string -> string
end

module Chain_id : sig
  val prefix : string
  val size : int
  val hash_string : string -> string
  val encode : string -> string
  val decode : string -> string
  val of_base58_block_hash : string -> string
end
