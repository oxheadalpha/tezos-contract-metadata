module List = ListLabels
module String = StringLabels

module type Base58_identifier = sig
  val prefix : string
  val encode : string -> Raw.base58
  val decode : Raw.base58 -> string
end

module type Base58_hash_identifier = sig
  include Base58_identifier

  val size : int
  val hash_string : string -> string
  val check : string -> unit
end

module Base58_prefixed (Parameters : sig
  val prefix : string
end) =
struct
  include Parameters

  let encode s = Raw.String.to_base58 (prefix ^ s)

  let decode s =
    let whole = Raw.String.of_base58 s in
    let prelen = String.length prefix in
    String.sub whole ~pos:prelen ~len:(String.length whole - prelen)
end

module Base58_hash (Parameters : sig
  val prefix : string val size : int
end) =
struct
  include Parameters

  module Sized_blacke2b = Digestif.Make_BLAKE2B (struct
    let digest_size = size
  end)

  let hash_string x = Sized_blacke2b.(to_raw_string (digest_string x))

  include Base58_prefixed (Parameters)

  let check hashed_string =
    let optry ~f k =
      Format.kasprintf
        (fun message ->
          match f () with
          | Some _ -> ()
          | None -> Format.kasprintf failwith "%s" message
          | exception Failure msg ->
              Format.kasprintf failwith "%s (Failure: %s)" message msg
          | exception e ->
              Format.kasprintf failwith "%s (Exception: %s)" message
                (Printexc.to_string e) )
        k in
    let exntry ~f k = optry ~f:(fun () -> Some (f ())) k in
    let bitcoin_alphabet = Vbmithr_base58.Alphabet.(all_characters default) in
    String.iteri hashed_string ~f:(fun idx c ->
        optry
          ~f:(fun () -> String.index_opt bitcoin_alphabet c)
          "Character %c (0x%x) at index %d is not part the Base58 alphabet." c
          (Char.code c) idx ) ;
    exntry
      ~f:(fun () ->
        let whole = Raw.String.of_base58 hashed_string in
        let prelen = String.length prefix in
        let pref = String.sub whole ~pos:0 ~len:prelen in
        if String.equal pref prefix then ()
        else Format.kasprintf failwith "Wrong prefix: %S" pref ;
        let decoded = decode hashed_string in
        let _ = Digestif.of_raw_string (Digestif.blake2b size) decoded in
        () )
      "Cannot decode base58 from %S" hashed_string ;
    ()
end

module Block_hash = struct
  include Base58_hash (struct let prefix = Prefix.block_hash let size = 32 end)
end

module Chain_id = struct
  include Base58_hash (struct let prefix = Prefix.chain_id let size = 4 end)

  let of_base58_block_hash hash =
    let of_block_hash block_hash =
      String.sub (Crypto_hash.String.blake2b ~size:32 block_hash) ~pos:0 ~len:4
    in
    encode (of_block_hash (Block_hash.decode hash))

  let%test _ =
    let flextesas_ones =
      (* Cf. Flextesa's vanity chain-ids in src/lib/interactive_mini_network.ml *)
      [ ( "BLmtDwmAm1FS1Ak5E2UN5Qu7MGnbpzonCqDUfSj4iC8AT5fteWa"
        , "NetXyJVJ3mkBox6" )
      ; ( "BLkENGLbHJ6ZL9vX7Kabb33yHsWL2z8bKzFFS3ntwTzz91YiTYb"
        , "NetXMFJWfpUBox7" )
      ; ( "BKverc3LnaRdiXUe9ruHrKqejFB3t9ZXxrqeH1Cwtfnbf9HhJtk"
        , "NetXnuwTfg9Box8" )
      ; ( "BMJqwuTLa3aSi3KAg4XtvSdVe5r7RuoXh5n15DwEoivx2Ve3Wfk"
        , "NetXfpUfwJdBox9" )
      ; ( "BLCRemfAUthe9XSXuJmuH5PmwvQk55aZUwtCbGZdjLh2niWZSJZ"
        , "NetXzcB5DmnBoxG" )
      ; ( "BLzMUYbk7sD6QG2H7tzLaJyU6dcN6ySE6dkVms49pY72DPN4Tfa"
        , "NetXgbFy27eBoxH" ) ] in
    let test_flextesa (block, chain_id) =
      Printf.eprintf "Trying of_base58_block_hash %s = %S Vs %s\n" block
        (of_base58_block_hash block)
        chain_id ;
      of_base58_block_hash block = chain_id in
    List.for_all ~f:test_flextesa flextesas_ones

  let%expect_test _ =
    let open Printf in
    let print_check s =
      try check s ; printf "OK\n%!" with
      | Failure msg -> printf "KO: %s\n%!" msg
      | e -> printf "KO: %s\n%!" (Printexc.to_string e) in
    print_check "PsIthaca" ;
    [%expect
      {| KO: Character I (0x49) at index 2 is not part the Base58 alphabet. |}] ;
    print_check "Psithaca" ;
    [%expect
      {|
        KO: Cannot decode base58 from "Psithaca" (Exception: (Invalid_argument Base58.of_string_exn)) |}] ;
    print_check "PsiThaCaT47Zboaw71QWScM8sXeMM7bbQFncK9FLqYc6EKdpjVP" ;
    [%expect
      {|
        KO: Cannot decode base58 from "PsiThaCaT47Zboaw71QWScM8sXeMM7bbQFncK9FLqYc6EKdpjVP" (Failure: Wrong prefix: "\002\170\131") |}] ;
    print_check "NetXgbFy27eBoxH" ;
    [%expect {| OK |}] ;
    ()
end

module Operation_hash = struct
  include Base58_hash (struct
    let prefix = Prefix.operation_hash let size = 32
  end)
end

module Kt1_address = struct
  include Base58_hash (struct
    let prefix = Prefix.contract_hash let size = 20
  end)

  let of_base58_operation_hash ?(index = 0l) op_hash =
    let operation_hash_bytes = Operation_hash.decode op_hash in
    let to_hash =
      let buf = Buffer.create (String.length operation_hash_bytes + 4) in
      Buffer.add_string buf operation_hash_bytes ;
      Buffer.add_int32_be buf index ;
      Buffer.contents buf in
    encode (Crypto_hash.String.blake2b ~size to_hash)

  let%test _ =
    let expected =
      (* See https://gist.github.com/smondet/006fa072e8d7afb32ba1adec106b09d8 *)
      [ ( "ooLC2QykF5NGiugdTyXJwsn2MEPMdRYJ4TeUoPMx8s38NzFximC"
        , 0x0l
        , "KT1B94zMXzBr5nLuGXxx1GF7r6uSK6Sfz7pn" )
      ; ( "opCCqC2xtPgXHdoYynQF4WajE3BvAJwr2uJN4jS54ziweKCiUgw"
        , 0x0l
        , "KT1BUd1YiPewuhYtbRpJqjrQj2yW55u1vEQB" )
      ; ( "opCCqC2xtPgXHdoYynQF4WajE3BvAJwr2uJN4jS54ziweKCiUgw"
        , 0x1l
        , "KT1VRpdD6jhMDdtR2QSLbRYDyFMkZpudVMAP" )
      ; ( "opCCqC2xtPgXHdoYynQF4WajE3BvAJwr2uJN4jS54ziweKCiUgw"
        , 0x3ffl
        , "KT1B49o4pF1wfv562fHcZ1wCzGFp8JdFkHeN" )
      ; ( "opCCqC2xtPgXHdoYynQF4WajE3BvAJwr2uJN4jS54ziweKCiUgw"
        , 0xdeadbeefl
        , "KT1EiKdPhuxRXrk9WtGEPVNp8aLYXFzJ1FGZ" ) ] in
    List.for_all expected ~f:(fun (op, index, expected_kt1) ->
        let computed = of_base58_operation_hash op ~index in
        Printf.eprintf
          "Trying of_base58_operation_hash %s ~index:%ld = %S Vs %s\n" op index
          computed expected_kt1 ;
        computed = expected_kt1 )
end

module Script_expr_hash = struct
  include Base58_hash (struct
    let prefix = Prefix.script_expr_hash let size = 32
  end)
end

module Ed25519 = struct
  module Secret_key = struct
    include Base58_prefixed (struct let prefix = Prefix.ed25519_seed end)
  end

  module Public_key = struct
    include Base58_prefixed (struct let prefix = Prefix.ed25519_public_key end)
  end

  module Public_key_hash = struct
    include Base58_hash (struct
      let prefix = Prefix.ed25519_public_key_hash let size = 20
    end)
  end

  module Signature = struct
    include Base58_prefixed (struct let prefix = Prefix.ed25519_signature end)
  end
end
