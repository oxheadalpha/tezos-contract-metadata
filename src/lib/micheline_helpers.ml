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

open Tezos_micheline

let get_script_field_exn string_micheline field =
  let open Micheline in
  let type_opt =
    match root string_micheline with
    | Seq (_, l) ->
        Base.List.find_map l ~f:(function
          | Prim (_, f, [t], _) when f = field -> Some t
          | _ -> None )
    | _ -> None in
  match type_opt with
  | None -> Fmt.failwith "Cannot find the %S field for the contract" field
  | Some s -> s

let get_storage_type_exn string_micheline =
  get_script_field_exn string_micheline "storage"

let get_parameter_type_exn string_micheline =
  get_script_field_exn string_micheline "parameter"

let pp_arbitrary_micheline ppf e =
  let module P = Micheline_printer in
  P.print_expr ppf
    (Micheline.map_node (fun _ -> {P.comment= None}) (fun x -> x) e)

let rec find_metadata_big_maps ~storage_node ~type_node =
  let open Micheline in
  let go (storage_node, type_node) =
    find_metadata_big_maps ~storage_node ~type_node in
  match (storage_node, type_node) with
  | Prim (_, "Pair", [l; r], _), Prim (_, "pair", [lt; rt], _) ->
      go (l, lt) @ go (r, rt)
  | ( Int (_, z)
    , Prim
        ( _
        , "big_map"
        , [Prim (_, "string", [], _); Prim (_, "bytes", [], _)]
        , ["%metadata"] ) ) ->
      [z]
  | Int (_, _z), _ -> []
  | String (_, _s), _ -> []
  | Bytes (_, _b), _ -> []
  | Prim (_, _prim, _args, _annot), _t -> []
  | Seq (_, _l), _t -> []

let build_off_chain_view_contract view ~contract_balance ~contract_address
    ~contract_storage_type ~contract_parameter_type ~view_parameters
    ~contract_storage =
  let open Metadata_contents.View in
  let open Metadata_contents.Michelson_blob in
  let open Implementation.Michelson_storage in
  let open Micheline in
  let getm = function Micheline m -> root m in
  let seq l = Seq (0, l) in
  let prim p l = Prim (0, p, l, []) in
  let parameter, input =
    match Option.map getm view.parameter with
    | Some m ->
        ( prim "pair" [m; contract_storage_type]
        , prim "Pair" [view_parameters; contract_storage] )
    | None -> (contract_storage_type, contract_storage) in
  let storage = getm view.return_type in
  let code = getm view.code in
  let rec fix_code c =
    let continue = List.map fix_code in
    match c with
    | (Int _ | String _ | Bytes _) as lit -> lit
    | Prim (loc, "SELF", [], annots) ->
        seq
          [ prim "PUSH" [prim "address" []; String (loc, contract_address)]
          ; Prim (loc, "CONTRACT", [contract_parameter_type], annots)
          ; prim "IF_NONE" [seq [prim "UNIT" []; prim "FAILWITH" []]; seq []] ]
    | Prim (loc, "BALANCE", [], annots) ->
        Prim (loc, "PUSH", [prim "mutez" []; Int (0, contract_balance)], annots)
    | Prim (loc, name, args, annots) -> Prim (loc, name, continue args, annots)
    | Seq (loc, l) -> Seq (loc, continue l) in
  ( `Contract
      (seq
         [ prim "parameter" [parameter]
         ; prim "storage" [prim "option" [storage]]
         ; prim "code"
             [ seq
                 [ prim "CAR" [] (* We drop the storage (= None). *)
                 ; fix_code code
                 ; prim "SOME" []
                 ; prim "NIL" [prim "operation" []]
                 ; prim "PAIR" [] ] ] ] )
  , `Input input
  , `Storage (prim "None" []) )
