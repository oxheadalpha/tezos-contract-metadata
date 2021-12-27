module String = StringLabels
module List = ListLabels

module Make_with_id (Id : sig
  val encode : string -> Tezai_base58_digest__Raw.base58
  val decode : Tezai_base58_digest__Raw.base58 -> string
end) =
struct
  type t = Bytes of string

  let to_string (Bytes s) = s
  let of_string s = Bytes s
  let to_base58 (Bytes s) = Id.encode s
  let of_base58 s = Bytes (Id.decode s)
  let pp ppf k = Format.fprintf ppf "%s" (to_base58 k)
end

module Secret_key = struct
  include Make_with_id (Tezai_base58_digest.Identifier.Ed25519.Secret_key)

  (* type t = Bytes of string *)

  let of_seed = function
    | "" -> invalid_arg "Secret_key.of_seed: empty string"
    | seed ->
        let alices =
          List.init ~len:32 ~f:(fun _ -> seed)
          |> String.concat ~sep:"" |> String.sub ~pos:0 ~len:32 in
        (* let pub =
             Mirage_crypto_ec.Ed25519.(
               pub_of_priv
                 (match priv_of_cstruct (Cstruct.of_string alices) with
                 | Ok priv -> priv
                 | Error err ->
                     Format.kasprintf failwith "Error: %a"
                       Mirage_crypto_ec.pp_error err)
               |> pub_to_cstruct |> Cstruct.to_string)
           in *)
        Bytes alices

  let%test _ =
    let () = Mirage_crypto_rng_unix.initialize () in
    let should_be_id s =
      Mirage_crypto_ec.Ed25519.(
        match priv_of_cstruct (Cstruct.of_string s) with
        | Ok priv -> priv_to_cstruct priv |> Cstruct.to_string
        | Error err ->
            Format.kasprintf failwith "Error: %a" Mirage_crypto_ec.pp_error err)
    in
    let test_of_seed seed =
      of_seed seed = of_string (should_be_id (to_string (of_seed seed))) in
    List.for_all ~f:test_of_seed
      ["alice"; "dsieljdsliejde"; String.init 50 ~f:Char.chr]
end

module Public_key = struct
  include Make_with_id (Tezai_base58_digest.Identifier.Ed25519.Public_key)

  let of_secret_key (Secret_key.Bytes s) =
    (* Bytes (String.sub s ~pos:0 ~len:32) *)
    Bytes
      Mirage_crypto_ec.Ed25519.(
        pub_of_priv
          ( match priv_of_cstruct (Cstruct.of_string s) with
          | Ok priv -> priv
          | Error err ->
              Format.kasprintf failwith "Error: %a" Mirage_crypto_ec.pp_error
                err )
        |> pub_to_cstruct |> Cstruct.to_string)
end

module Public_key_hash = struct
  module Tz1 = Tezai_base58_digest.Identifier.Ed25519.Public_key_hash
  include Make_with_id (Tz1)

  let of_public_key pk = Bytes (Public_key.to_string pk |> Tz1.hash_string)
end

let%expect_test _ =
  let () = Mirage_crypto_rng_unix.initialize () in
  let show_of_seed seed =
    let sk = Secret_key.of_seed seed in
    let pk = Public_key.of_secret_key sk in
    let pkh = Public_key_hash.of_public_key pk in
    Format.printf "Seed: %S\nSK: %a\nPK: %a\nPKH: %a\n" seed Secret_key.pp sk
      Public_key.pp pk Public_key_hash.pp pkh in
  (* The following were checked against ["flextesa key-of-name"] using
     ["tezos-crypto"]. *)
  show_of_seed "alice" ;
  [%expect
    {|
    Seed: "alice"
    SK: edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq
    PK: edpkvGfYw3LyB1UcCahKQk4rF2tvbMUk8GFiTuMjL75uGXrpvKXhjn
    PKH: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
    |}] ;
  show_of_seed "bob" ;
  [%expect
    {|
    Seed: "bob"
    SK: edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt
    PK: edpkurPsQ8eUApnLUJ9ZPDvu98E8VNj4KtJa1aZr16Cr5ow5VHKnz4
    PKH: tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6
    |}] ;
  show_of_seed "chuck" ;
  [%expect
    {|
    Seed: "chuck"
    SK: edsk3RgXNjpM6MhBJjxjAcm4AVNp6B2jr9KKdLdTce63bgnAr9jmWS
    PK: edpktn8R91vDHirgNYz6DbK5TPHqTvJmtE3WqQoYitdmCSL94EKwQs
    PKH: tz1U6gfR9YAkJe8H2UNeo5zudU2mojsdkKYb
    |}] ;
  ()
