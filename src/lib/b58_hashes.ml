(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 TQ Tezos <contact@tqtezos.com>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open! Base
open! Import

module B58_crypto = struct
  let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
  let sha512 s = Digestif.SHA512.(to_raw_string (digest_string s))
end

let script_expr_hash =
  (* Taken from src/proto_006_PsCARTHA/lib_protocol/script_expr_hash.ml *)
  (* expr(54) *)
  "\013\044\064\027"

let contract_hash =
  (* src/proto_006_PsCARTHA/lib_protocol/contract_hash.ml KT1(36) *)
  "\002\090\121"

let chain_id = (* src/lib_crypto/base58.ml *) "\087\082\000"
let protocol_hash = "\002\170" (* P(51) *)

let blake2b32 = Digestif.blake2b 32

let blake2b x =
  Digestif.digest_string blake2b32 x |> Digestif.to_raw_string blake2b32

let b58 s = Base58.of_bytes (module B58_crypto) s |> Base58.to_string

let unb58 s =
  Base58.of_string_exn (module B58_crypto) s
  |> Base58.to_bytes (module B58_crypto)

let b58_script_id_hash s = b58 (script_expr_hash ^ blake2b s)

let check_b58_hash ~prefix ~size s =
  let ppstring ppf s =
    let open Fmt in
    pf ppf "[%d???0x%a]" (String.length s) Hex.pp (Hex.of_string s) in
  let optry o k =
    Fmt.kstr
      (fun message ->
        match o () with
        | Some s -> s
        | None -> Fmt.failwith "%s" message
        | exception e -> Fmt.failwith "%s (%a)" message Exn.pp e )
      k in
  String.mapi s ~f:(fun idx c ->
      let bitcoin =
        "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
      if String.mem bitcoin c then '\x00'
      else
        Fmt.failwith
          "Invalid character '%c' at position %d in supposedly base-58 %S" c idx
          s )
  |> ignore ;
  let b58 =
    optry
      (fun () -> Base58.of_string (module B58_crypto) s)
      "Cannot decode base58 from %S" s in
  let data =
    optry
      (fun () -> Base58.to_bytes (module B58_crypto) b58)
      "Cannot get data from base58 %a" Base58.pp b58 in
  if not (String.is_prefix data ~prefix) then
    Fmt.failwith "Wrong prefix for data 0x%a, expecting 0x%a" Hex.pp
      (Hex.of_string data) Hex.pp (Hex.of_string prefix) ;
  let hashpart =
    optry
      (fun () -> String.chop_prefix data ~prefix)
      "Wrong refix AGAIN??? %S %S" data prefix in
  optry
    (fun () -> Digestif.of_raw_string_opt (Digestif.blake2b size) hashpart)
    "This is not a blake2b hash: %a" ppstring hashpart

let check_b58_kt1_hash s = check_b58_hash ~prefix:contract_hash ~size:20 s
let check_b58_chain_id_hash s = check_b58_hash ~prefix:chain_id ~size:4 s
let check_b58_protocol_hash s = check_b58_hash ~prefix:protocol_hash ~size:32 s

let b58_script_id_hash_of_michelson_string s =
  b58_script_id_hash ("\x05" ^ Michelson_bytes.encode_michelson_string s)

let b58_script_id_hash_of_michelson_int s =
  b58_script_id_hash ("\x05" ^ Michelson_bytes.encode_michelson_int s)

let%test "Blake2B" =
  let expect =
    "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8" in
  let dgst = Digestif.digest_string (Digestif.blake2b 32) "" in
  let actual = Digestif.to_hex (Digestif.blake2b 32) dgst in
  String.equal expect actual

let%test "Base58" =
  let expect =
    "0d2c401b4a1cf11667fa0165eac9963333b883a80bcfdfebde09b79bfc740680e986bab6"
  in
  let actual =
    Base58.of_string_exn
      (module B58_crypto)
      "expru5X1yxJG6ezR2uHMotwMLNmSzQyh5t1vUnhjx4cS6Pv9qE1Sdo"
    |> Base58.to_bytes (module B58_crypto)
    |> Option.value_map ~default:"EEEERRRROR" ~f:(fun x ->
           let (`Hex h) = Hex.of_string x in
           h ) in
  String.equal expect actual

let%test "Crypto test" =
  let dbg fmt = Fmt.pf Fmt.stderr "@[tzcomet-debug: %a@]%!\n" fmt () in
  let dbgf fmt = Fmt.(kstr (fun s -> dbg (const string s))) fmt in
  let michelson_string_expr_hash s expect_bytes_hex expect_digest_raw_hex
      expect_digest_b58 expect_digest_b58_raw_encode expect_digest_with05_hex
      expect_digest_with05_b58 expect_digest_with05_b58_raw_encode
      expect_digest_pfx_b58 =
    dbgf "mseh: %S" s ;
    let bytes = Michelson_bytes.encode_michelson_string s in
    let expect_hex expected bytes =
      let (`Hex hx) = Hex.of_string bytes in
      dbgf "actual 0x%s expect %s\n" hx expected ;
      assert (String.equal expected ("0x" ^ hx)) in
    let ppb ppf b =
      let (`Hex hx) = Hex.of_string b in
      Fmt.pf ppf "0x%s\n" hx in
    expect_hex expect_bytes_hex bytes ;
    let dgst x =
      Digestif.to_raw_string blake2b32 (Digestif.digest_string blake2b32 x)
    in
    let b58 s = Base58.to_string (Base58.of_bytes (module B58_crypto) s) in
    dbgf "digest raw: %a -> %s (%s)" ppb (dgst bytes)
      (b58 (dgst bytes))
      (Base58.raw_encode (dgst bytes)) ;
    expect_hex expect_digest_raw_hex (dgst bytes) ;
    assert (String.equal expect_digest_b58 (b58 (dgst bytes))) ;
    assert (
      String.equal expect_digest_b58_raw_encode (Base58.raw_encode (dgst bytes)) ) ;
    let with05 = "\x05" ^ bytes in
    dbgf "digest-05: %a ??? %s [%s]" ppb (dgst with05)
      (b58 (dgst with05))
      (Base58.raw_encode (dgst with05)) ;
    expect_hex expect_digest_with05_hex (dgst with05) ;
    assert (String.equal expect_digest_with05_b58 (b58 (dgst with05))) ;
    assert (
      String.equal expect_digest_with05_b58_raw_encode
        (Base58.raw_encode (dgst with05)) ) ;
    dbgf "digest-pfx: %a ??? %s" ppb (dgst with05)
      (b58 (script_expr_hash ^ dgst with05)) ;
    assert (
      String.equal expect_digest_pfx_b58 (b58 (script_expr_hash ^ dgst with05)) )
  in
  michelson_string_expr_hash "" "0x0100000000"
    "0x3d31a0f322b4ce036466dc45f4ca74d28cbc7bd89427b1b61620aea87c384491"
    "Tx7g83eUVQUGxWb3Lzxep4MxkCc4hJq3a643QgwJNVKVa3aQz"
    "57smAWGpXmgT3sKEj6aN26ra6xeyAsbp7Jm1AeAUkYG8"
    "0x4a1cf11667fa0165eac9963333b883a80bcfdfebde09b79bfc740680e986bab6"
    "Ze7sbAPDYUELUCEjVNLAJp4tx5Eij6HLKKYV3TmcFrBV9NnhD"
    "5zJmRN5mEKQUQKYcfDwfSmMPjRZ1RE3PvsKDjhN4TDMX"
    "expru5X1yxJG6ezR2uHMotwMLNmSzQyh5t1vUnhjx4cS6Pv9qE1Sdo" ;
  michelson_string_expr_hash "foo" "0x0100000003666f6f"
    "0xa14995177d1226fb240c04c37f47ca239aee9ee6da9778effaff054b8ef9849a"
    "2E2sWug5KYw7AbkAbUJABfkdFJ9ysvkTaZNA6LaoNMwtY5LXs3"
    "BrbiXYu4uo9q8Hd6nAZJncPKQXGAHwscMxYQBatutaWD"
    "0x7b754bee2b13dd9f9486e6f933e27afc2c31765a8414fc98422255138b04a366"
    "wNaH8jxsngu2WFEEWRnoCUs4itVyeBZRPJaxzULPsSd48e5vL"
    "9JvtJaNHvYP5bpF7tjbbtpr8o7Jtptz445dZ8pemDnvH"
    "expruTFUPVsqkuD5iwLMJuzoyGSFABnxLo7CZrgnS1czt1WbTwpVrJ" ;
  true
(* testing is handled in assertions *)
