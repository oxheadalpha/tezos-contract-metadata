type base58 = string

module String = struct
  let to_base58 s : base58 =
    Base58.of_bytes (module Crypto_hash.String) s |> Base58.to_string

  let of_base58 (s : base58) =
    Base58.of_string_exn (module Crypto_hash.String) s
    |> Base58.to_bytes_exn (module Crypto_hash.String)

  let%test "base58-involutions" =
    let check v = String.equal v (to_base58 v |> of_base58) in
    List.for_all check [""; "hello"; "\xde\xad\xbe\xef"; "\x00\xFF"]

  let%test "unbase58-involutions" =
    let check v = String.equal v (of_base58 v |> to_base58) in
    List.for_all check
      [ "tz1SebmhV9P6pKfx7otPpdECdqY2JPZYB5gM"
      ; "KT1QjUt6TyeV4EMYdouBMyvUiK4JjQJyY1EK"
      ; "oogrVcyHWq5YzaM4nfZbsJcqJGEMcbAqsRrvKt9onvmuJ8Aj71y"
      ; "exprv7Y3jmbfDJXZJimPNvHGoFThweNRYSoxqeS1HzGmSHXkhX6hhK"
      ; "BLQLKQxVtrQFDem6M2xgiPtM5EtuXdnmfj9V3TvPevyn4e7rzb1" ]
end
