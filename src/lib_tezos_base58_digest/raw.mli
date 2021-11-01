(** Raw base58check conversions (using Digestif's SHA256 implementation). *)

(** Convert strings from/to base58-encoded strings. *)
module String : sig
  val to_base58 : string -> string val of_base58 : string -> string
end
