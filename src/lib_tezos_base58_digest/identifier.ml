module Base58_hash (Parameters : sig
  val prefix : string val size : int
end) =
struct
  include Parameters

  let hash_string = Crypto_hash.String.blake2b ~size
  let encode s = Raw.String.to_base58 (prefix ^ s)

  let decode s =
    let whole = Raw.String.of_base58 s in
    let prelen = String.length prefix in
    String.sub whole prelen (String.length whole - prelen)
end

module Block_hash = struct
  include Base58_hash (struct let prefix = Prefix.block_hash let size = 32 end)
end

module Chain_id = struct
  include Base58_hash (struct let prefix = Prefix.chain_id let size = 4 end)

  let of_base58_block_hash hash =
    let of_block_hash block_hash =
      String.sub (Crypto_hash.String.blake2b ~size:32 block_hash) 0 4 in
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
    List.for_all test_flextesa flextesas_ones
end
