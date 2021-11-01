(** Convert string from/to base58-encoded strings. *)
module String : sig
  val to_base58 : string -> string val of_base58 : string -> string
end
