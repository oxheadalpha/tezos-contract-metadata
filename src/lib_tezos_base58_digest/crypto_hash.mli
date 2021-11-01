(** Tezos' common hash functions as [string -> string] OCaml functions.  *)
module String : sig
  val sha256 : string -> string
  val sha512 : string -> string
  val blake2b : string -> string
end
