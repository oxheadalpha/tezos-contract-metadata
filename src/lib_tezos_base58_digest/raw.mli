(** Raw base58check conversions (using Digestif's SHA256 implementation). *)

(** The type base58 encoded strings is an alias for [string]. *)
type base58 = string

(** Convert strings from/to base58-encoded strings. *)
module String : sig
  val to_base58 : string -> base58 val of_base58 : base58 -> string
end
