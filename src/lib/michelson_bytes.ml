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

open Import

(** See src/proto_alpha/lib_protocol/michelson_v1_primitives.ml *)
let primitives =
  [ (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("parameter", "K_parameter")
  ; ("storage", "K_storage")
  ; ("code", "K_code")
  ; ("False", "False")
  ; ("Elt", "Elt")
  ; ("Left", "Left")
  ; ("None", "None")
  ; ("Pair", "Pair")
  ; ("Right", "Right")
  ; ("Some", "Some")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("True", "True")
  ; ("Unit", "Unit")
  ; ("PACK", "PACK")
  ; ("UNPACK", "UNPACK")
  ; ("BLAKE2B", "BLAKE2B")
  ; ("SHA256", "SHA256")
  ; ("SHA512", "SHA512")
  ; ("ABS", "ABS")
  ; ("ADD", "ADD")
  ; ("AMOUNT", "AMOUNT")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("AND", "AND")
  ; ("BALANCE", "BALANCE")
  ; ("CAR", "CAR")
  ; ("CDR", "CDR")
  ; ("CHECK_SIGNATURE", "CHECK_SIGNATURE")
  ; ("COMPARE", "COMPARE")
  ; ("CONCAT", "CONCAT")
  ; ("CONS", "CONS")
  ; ("CREATE_ACCOUNT", "CREATE_ACCOUNT")
  ; ("CREATE_CONTRACT", "CREATE_CONTRACT")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("IMPLICIT_ACCOUNT", "IMPLICIT_ACCOUNT")
  ; ("DIP", "DIP")
  ; ("DROP", "DROP")
  ; ("DUP", "DUP")
  ; ("EDIV", "EDIV")
  ; ("EMPTY_MAP", "EMPTY_MAP")
  ; ("EMPTY_SET", "EMPTY_SET")
  ; ("EQ", "EQ")
  ; ("EXEC", "EXEC")
  ; ("FAILWITH", "FAILWITH")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("GE", "GE")
  ; ("GET", "GET")
  ; ("GT", "GT")
  ; ("HASH_KEY", "HASH_KEY")
  ; ("IF", "IF")
  ; ("IF_CONS", "IF_CONS")
  ; ("IF_LEFT", "IF_LEFT")
  ; ("IF_NONE", "IF_NONE")
  ; ("INT", "INT")
  ; ("LAMBDA", "LAMBDA")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("LE", "LE")
  ; ("LEFT", "LEFT")
  ; ("LOOP", "LOOP")
  ; ("LSL", "LSL")
  ; ("LSR", "LSR")
  ; ("LT", "LT")
  ; ("MAP", "MAP")
  ; ("MEM", "MEM")
  ; ("MUL", "MUL")
  ; ("NEG", "NEG")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("NEQ", "NEQ")
  ; ("NIL", "NIL")
  ; ("NONE", "NONE")
  ; ("NOT", "NOT")
  ; ("NOW", "NOW")
  ; ("OR", "OR")
  ; ("PAIR", "PAIR")
  ; ("PUSH", "PUSH")
  ; ("RIGHT", "RIGHT")
  ; ("SIZE", "SIZE")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("SOME", "SOME")
  ; ("SOURCE", "SOURCE")
  ; ("SENDER", "SENDER")
  ; ("SELF", "SELF")
  ; ("STEPS_TO_QUOTA", "STEPS_TO_QUOTA")
  ; ("SUB", "SUB")
  ; ("SWAP", "SWAP")
  ; ("TRANSFER_TOKENS", "TRANSFER_TOKENS")
  ; ("SET_DELEGATE", "SET_DELEGATE")
  ; ("UNIT", "UNIT")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("UPDATE", "UPDATE")
  ; ("XOR", "XOR")
  ; ("ITER", "ITER")
  ; ("LOOP_LEFT", "LOOP_LEFT")
  ; ("ADDRESS", "ADDRESS")
  ; ("CONTRACT", "CONTRACT")
  ; ("ISNAT", "ISNAT")
  ; ("CAST", "CAST")
  ; ("RENAME", "RENAME")
  ; ("bool", "bool")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("contract", "contract")
  ; ("int", "int")
  ; ("key", "key")
  ; ("key_hash", "key_hash")
  ; ("lambda", "lambda")
  ; ("list", "list")
  ; ("map", "map")
  ; ("big_map", "big_map")
  ; ("nat", "nat")
  ; ("option", "option")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("or", "or")
  ; ("pair", "pair")
  ; ("set", "set")
  ; ("signature", "signature")
  ; ("string", "string")
  ; ("bytes", "bytes")
  ; ("mutez", "mutez")
  ; ("timestamp", "timestamp")
  ; ("unit", "unit")
  ; ("operation", "operation")
  ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
    ("address", "address")
  ; (* Alpha_002 addition *)
    ("SLICE", "SLICE")
  ; (* Alpha_005 addition *)
    ("DIG", "DIG")
  ; ("DUG", "DUG")
  ; ("EMPTY_BIG_MAP", "EMPTY_BIG_MAP")
  ; ("APPLY", "APPLY")
  ; ("chain_id", "chain_id")
  ; ("CHAIN_ID", "CHAIN_ID")
    (* New instructions must be added here, "for" backward compatibility of the encoding. *)
  ]

let expr_encoding =
  Tezos_micheline.Micheline.canonical_encoding_v1 ~variant:"michelson_v1"
    (* Data_encoding.Encoding.string *)
    (let open Data_encoding in
    def "michelson.v1.primitives" @@ string_enum primitives)

module Hex_reimplementation = struct
  (** We rewrite some of `hex.ml` to improve error messages. *)
  open Caml

  let to_char ~position x y =
    let code pos c =
      match c with
      | '0' .. '9' -> Char.code c - 48 (* Char.code '0' *)
      | 'A' .. 'F' -> Char.code c - 55 (* Char.code 'A' + 10 *)
      | 'a' .. 'f' -> Char.code c - 87 (* Char.code 'a' + 10 *)
      | _ ->
          Decorate_error.raise
            Message.(
              text "Character ???"
              % inline_code (Char.escaped c)
              % text "???"
              %% parens
                   ( int inline_code (Char.code c)
                   % text ", "
                   %% Fmt.kstr inline_code "0x%02x" (Char.code c) )
              %% text "at position" %% int inline_code pos
              %% text "is not valid Hexadecimal encoding.") in
    Char.chr ((code position x lsl 4) + code (position + 1) y)

  let to_helper ~empty_return ~create ~set (`Hex s) =
    if s = "" then empty_return
    else
      let n = String.length s in
      let buf = create (n / 2) in
      let rec aux i j =
        if i >= n then ()
        else if j >= n then
          Decorate_error.raise
            Message.(
              text "Invalid hexadecimal string: length should be even, not"
              %% int inline_code n % text ".")
        else (
          set buf (i / 2) (to_char ~position:j s.[i] s.[j]) ;
          aux (j + 1) (j + 2) ) in
      aux 0 1 ; buf

  let to_bytes hex =
    to_helper ~empty_return:Bytes.empty ~create:Bytes.create ~set:Bytes.set hex
end

let parse_hex_bytes bytes =
  try
    let mich =
      Data_encoding.Binary.of_bytes_exn
        (* Tezos_micheline.Micheline.canonical_location_encoding *)
        expr_encoding
        (Hex_reimplementation.to_bytes (`Hex bytes)) in
    let json =
      Data_encoding.Json.construct expr_encoding
        (* Tezos_micheline.Micheline.canonical_location_encoding *)
        mich in
    Ok
      ( json
      , let open Tezos_micheline in
        Fmt.str "%a" Micheline_printer.print_expr
          (Micheline_printer.printable Base.Fn.id mich) )
  with e ->
    let open Tezos_error_monad.Error_monad in
    Error [Exn e]

let pack_node_expression e =
  Data_encoding.Binary.to_bytes_exn expr_encoding
    (Tezos_micheline.Micheline.strip_locations e)
  |> Bytes.to_string

let encode_michelson_string s =
  Data_encoding.Binary.to_bytes_exn expr_encoding
    Tezos_micheline.Micheline.(String (0, s) |> strip_locations)
  |> Bytes.to_string

let encode_michelson_int i =
  Data_encoding.Binary.to_bytes_exn expr_encoding
    Tezos_micheline.Micheline.(Int (0, Z.of_int i) |> strip_locations)
  |> Bytes.to_string

let example () =
  let bytes = "0707002a002a" in
  let to_display =
    try
      let mich =
        Data_encoding.Binary.of_bytes_exn
          (* Tezos_micheline.Micheline.canonical_location_encoding *)
          expr_encoding
          (Hex.to_bytes (`Hex bytes)) in
      let json =
        Data_encoding.Json.construct expr_encoding
          (* Tezos_micheline.Micheline.canonical_location_encoding *)
          mich in
      Ezjsonm.value_to_string ~minify:false json
    with
    | Data_encoding.Binary.Read_error e ->
        Fmt.str "readerror: %a" Data_encoding.Binary.pp_read_error e
    | e -> Fmt.str "exn: %a" Base.Exn.pp e in
  to_display
