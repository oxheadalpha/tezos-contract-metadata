open Tezos_contract_metadata

let of_uri uri_str =
  let open Metadata_uri in
  let fail error_kind =
    Tezos_error_monad.Error_monad.error
      (Contract_metadata_uri_parsing {input= uri_str; error_kind}) in
  let validate_network network =
    match network with
    | "mainnet" | "edonet" | "florencenet" | "granadanet" | "hangzhounet"
     |"mondaynet" ->
        Ok ()
    | network -> (
      try Ok (ignore (B58_hashes.check_b58_chain_id_hash network)) with
      | Failure f -> fail (Wrong_network (network, f))
      | e ->
          Fmt.kstr
            (fun x -> fail (Wrong_network (network, x)))
            "%a" Base.Exn.pp e ) in
  let validate_kt1_address address =
    try Ok (ignore (B58_hashes.check_b58_kt1_hash address)) with
    | Failure f -> fail (Bad_b58 (address, f))
    | e -> Fmt.kstr (fun x -> fail (Bad_b58 (address, x))) "%a" Base.Exn.pp e
  in
  Metadata_uri.of_uri ~validate_kt1_address ~validate_network
    (Uri.of_string uri_str)

let%test "validate_address tezos-storage bogus address" =
  Result.is_error (of_uri "tezos-storage://nope")

let%test "validate_address tezos-storage bogus network" =
  Result.is_error
    (of_uri "tezos-storage://KT1QDFEu8JijYbsJqzoXq7mKvfaQQamHD1kX.harappanet")

let%test "validate_address tezos-storage with path and network" =
  Result.is_ok
    (of_uri "tezos-storage://KT1QDFEu8JijYbsJqzoXq7mKvfaQQamHD1kX.mainnet/foo")

let%test "validate_address tezos-storage with path" =
  Result.is_ok
    (of_uri "tezos-storage://KT1QDFEu8JijYbsJqzoXq7mKvfaQQamHD1kX/foo")
