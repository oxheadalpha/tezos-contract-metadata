(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 TQ Tezos <contact@tqtezos.com>                         *)
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

module Node_status = struct
  type t = Uninitialized | Non_responsive of exn | Ready of string
end

module Rpc_cache = struct
  module Hashtbl = Caml.Hashtbl
  include Query_node.Rpc_cache

  let create () : t = Hashtbl.create 42

  let add ctxt (t : t) ~rpc ~response =
    let now = ctxt#program_time in
    dbgf ctxt "CACHE-ADD: %s (%.0f)" rpc now ;
    Hashtbl.add t rpc (now, response)

  let get ctxt (t : t) ~rpc =
    let now = ctxt#program_time in
    let best_ts = ref 0. in
    let best = ref None in
    let filter r (ts, v) =
      if Float.(!best_ts < ts) && String.equal rpc r then (
        best_ts := ts ;
        best := Some v ) ;
      if Float.(ts + 120. < now) then None else Some (ts, v) in
    Hashtbl.filter_map_inplace filter t ;
    let age = now -. !best_ts in
    dbgf ctxt "CACHE-GET:\n %s\n → now: %.0f\n → age: %.0f\n → %s" rpc now age
      (if Option.is_none !best then "MISS" else "HIT") ;
    (age, !best)
end

module Node = struct
  include Query_node.Node

  let create ~network ?info_url name prefix =
    {name; prefix; rpc_cache= Rpc_cache.create (); network; info_url}

  let rpc_get (ctxt : 'a Context.t) node path =
    let uri = node.prefix ^ path in
    let open Lwt in
    let actually_get uri =
      let content = ctxt#http_client.get ?limit_bytes:None uri in
      content
      >>= fun content ->
      return
        (Result.map
           ~f:(fun c ->
             Rpc_cache.add ctxt node.rpc_cache ~rpc:path ~response:c ;
             c )
           content ) in
    match Rpc_cache.get ctxt node.rpc_cache ~rpc:path with
    | _, None -> actually_get uri
    | age, Some _ when Float.(age > 120.) -> actually_get uri
    | _, Some s -> return (Ok s)

  let rpc_post (ctxt : 'a Context.t) ~node ~body path =
    let uri = node.prefix ^ path in
    let ctxt =
      ctxt#with_log_context ("Calling HTTP-POST on node " ^ node.name ^ ": ")
    in
    let headers = Cohttp.Header.of_list [("content_type", "application/json")] in
    ctxt#http_client.post ~headers ~body uri

  let get_storage ctxt node ~address =
    Lwt.catch
      (fun () ->
        let body =
          Ezjsonm.(
            dict [("unparsing_mode", string "Optimized_legacy")]
            |> value_to_string) in
        Fmt.kstr
          (rpc_post ctxt ~node ~body)
          "/chains/main/blocks/head/context/contracts/%s/storage/normalized"
          address )
      (fun _ ->
        dbgf ctxt "Node does not handle /normalized" ;
        Fmt.kstr (rpc_get ctxt node)
          "/chains/main/blocks/head/context/contracts/%s/storage" address )

  module Contract = struct
    type t =
      { storage_node: (int, string) Tezos_micheline.Micheline.node
      ; type_node: (int, string) Tezos_micheline.Micheline.node
      ; metadata_big_map: Z.t }

    let make ~storage_node ~type_node ~metadata_big_map =
      {storage_node; type_node; metadata_big_map}
  end

  let metadata_big_map ctxt node ~address =
    let open Lwt in
    let ( >>= ) = Lwt_result.bind in
    get_storage ctxt node ~address
    >>= fun storage_string ->
    let get = rpc_get ctxt node in
    dbgf ctxt "Got raw storage: %s" storage_string ;
    let mich_storage = Michelson.micheline_of_json storage_string in
    dbgf ctxt "As concrete: %a" Micheline_helpers.pp_arbitrary_micheline
      mich_storage ;
    return (Ok ())
    >>= fun () ->
    Fmt.kstr get "/chains/main/blocks/head/context/contracts/%s/script" address
    >>= fun script_string ->
    dbgf ctxt "Got raw script: %s…"
      (ellipsize_string script_string ~max_length:30) ;
    let mich_storage_type =
      Michelson.micheline_of_json script_string
      |> Tezos_micheline.Micheline.strip_locations
      |> Micheline_helpers.get_storage_type_exn in
    dbgf ctxt "Storage type: %a" Micheline_helpers.pp_arbitrary_micheline
      mich_storage_type ;
    return (Ok ())
    >>= fun () ->
    let bgs =
      Micheline_helpers.find_metadata_big_maps ~storage_node:mich_storage
        ~type_node:mich_storage_type in
    match bgs with
    | [] -> Fmt.failwith "Contract has no valid %%metadata big-map!"
    | _ :: _ :: _ ->
        Fmt.failwith "Contract has too many %%metadata big-maps: %s"
          ( oxfordize_list bgs ~map:Z.to_string
              ~sep:(fun () -> ",")
              ~last_sep:(fun () -> ", and ")
          |> String.concat ~sep:"" )
    | [metadata_big_map] ->
        return
          (Ok
             Contract.(
               make ~metadata_big_map ~storage_node:mich_storage
                 ~type_node:mich_storage_type) )

  let bytes_value_of_big_map_at_string ctxt node ~big_map_id ~key =
    let open Lwt in
    let ( >>= ) = Lwt_result.bind in
    let hash_string =
      Michelson_bytes.b58_script_id_hash_of_michelson_string key in
    Decorate_error.(
      reraise
        Message.(
          text "Cannot find any value in the big-map"
          %% inline_code (Z.to_string big_map_id)
          %% text "at the key" %% inline_code key %% text "(hash: "
          % inline_code hash_string % text ").")
        ~f:(fun () ->
          Fmt.kstr (rpc_get ctxt node)
            "/chains/main/blocks/head/context/big_maps/%s/%s"
            (Z.to_string big_map_id) hash_string ))
    >>= fun bytes_raw_value ->
    dbgf ctxt "bytes raw value: %s"
      (ellipsize_string bytes_raw_value ~max_length:30) ;
    let content =
      match Ezjsonm.value_from_string bytes_raw_value with
      | `O [("bytes", `String b)] -> Hex.to_string (`Hex b)
      | _ -> Fmt.failwith "Cannot find bytes in %s" bytes_raw_value in
    return (Ok content)

  let micheline_value_of_big_map_at_nat ctxt node ~big_map_id ~key =
    let hash_string = Michelson_bytes.b58_script_id_hash_of_michelson_int key in
    Lwt_result.bind_result
      Decorate_error.(
        reraise
          Message.(
            text "Cannot find any value in the big-map"
            %% inline_code (Z.to_string big_map_id)
            %% text "at the key" %% int inline_code key %% text "(hash: "
            % inline_code hash_string % text ").")
          ~f:(fun () ->
            Fmt.kstr (rpc_get ctxt node)
              "/chains/main/blocks/head/context/big_maps/%s/%s"
              (Z.to_string big_map_id) hash_string ))
      (fun raw_value ->
        dbgf ctxt "JSON raw value: %s"
          (ellipsize_string raw_value ~max_length:60) ;
        Ok (Michelson.micheline_of_json raw_value) )
end

module Node_list = struct
  type t = (string, Node.t * bool) List.Assoc.t

  let empty : t = []

  let add ?(dev = false) t n =
    List.Assoc.add ~equal:String.equal t n.Node.name (n, dev)

  let map t ~f = List.map t ~f:(fun (_, (n, _)) -> f n)
  let nodes t = map t ~f:(fun x -> x)
end

(* TODO: move to network.ml *)
let default_nodes : Node.t list =
  let smartpy = "https://smartpy.io/nodes" in
  let giga = "https://giganode.io" in
  List.rev
    [ Node.create "Mainnet-SmartPy" "https://mainnet.smartpy.io"
        ~network:`Mainnet ~info_url:smartpy
    ; Node.create "Edonet-SmartPy" "https://edonet.smartpy.io" ~network:`Edonet
        ~info_url:smartpy
    ; Node.create "Granadanet-SmartPy" "https://granadanet.smartpy.io"
        ~network:`Granadanet ~info_url:smartpy
    ; Node.create "Hangzhounet-SmartPy" "https://hangzhounet.smartpy.io"
        ~network:`Hangzhounet ~info_url:smartpy
    ; Node.create "Florencenet-SmartPy" ~network:`Florencenet
        "https://florencenet.smartpy.io" ~info_url:smartpy
    ; Node.create "Mainnet-GigaNode" "https://mainnet-tezos.giganode.io"
        ~network:`Mainnet ~info_url:giga
    ; Node.create "Edonet-GigaNode" "https://edonet-tezos.giganode.io"
        ~network:`Edonet ~info_url:giga
    ; Node.create "Florence-NoBA-net-GigaNode"
        "https://florence-tezos.giganode.io" ~network:`Florencenet
        ~info_url:giga
    ; Node.create "Flextesabox-node" "http://127.0.0.1:20000" ~network:`Sandbox
        ~info_url:"https://assets.tqtezos.com/docs/setup/2-sandbox/" ]

let dev_nodes =
  List.rev
    [Node.create "Dev:Wrong-node" "http://example.com/nothing" ~network:`Sandbox]

let get_default_nodes () =
  let dev =
    List.fold_left dev_nodes ~init:Node_list.empty ~f:(Node_list.add ~dev:true)
  in
  List.fold_left default_nodes ~init:dev ~f:(Node_list.add ~dev:false)

let find_node_with_contract ctxt addr =
  let open Lwt in
  let trace = ref [] in
  catch
    (fun () ->
      Lwt_list.find_s
        (fun node ->
          catch
            (fun () ->
              Fmt.kstr (Node.rpc_get ctxt node)
                "/chains/main/blocks/head/context/contracts/%s/storage" addr
              >>= fun res -> return (Result.is_ok res) )
            (fun exn ->
              dbgf ctxt "exn %S" (Exn.to_string exn) ;
              trace := exn :: !trace ;
              return_false ) )
        ctxt#nodes
      >>= fun node -> Lwt.return (Ok node) )
    (fun exn ->
      Lwt.return
        (Fmt.kstr Http_client.failure
           "Cannot find a node that knows about address %s: %a" addr
           Tezos_error_monad.Error_monad.pp_exn exn ) )

let metadata_value ctxt ~address ~key =
  let ( >>= ) = Lwt_result.bind in
  find_node_with_contract ctxt address
  >>= fun node ->
  dbgf ctxt "Found contract with node %S" node.Node.name ;
  Node.metadata_big_map ctxt node ~address
  >>= fun metacontract ->
  let big_map_id = metacontract.Node.Contract.metadata_big_map in
  dbgf ctxt "Metadata big-map: %s" (Z.to_string big_map_id) ;
  Node.bytes_value_of_big_map_at_string ctxt node ~big_map_id ~key

let call_off_chain_view ctxt ~address ~view ~parameter =
  let ( >>= ) = Lwt_result.bind in
  let open Metadata_contents.View.Implementation.Michelson_storage in
  let logf f = Fmt.kstr (fun s -> dbgf ctxt "call_off_chain_view: %s" s) f in
  logf "Calling %s(%a)" address Micheline_helpers.pp_arbitrary_micheline
    parameter ;
  find_node_with_contract ctxt address
  >>= fun node ->
  logf "Found contract with node %S" node.name ;
  Fmt.kstr (Node.rpc_get ctxt node) "/chains/main/blocks/head/protocols"
  >>= fun protocols ->
  let protocol_kind, protocol_hash =
    let hash =
      match Ezjsonm.value_from_string protocols with
      | `O l ->
          List.find_map l ~f:(function
            | "protocol", `String p -> Some p
            | _ -> None )
      | _ | (exception _) -> None in
    match hash with
    | None ->
        Decorate_error.raise
          Message.(
            text "Cannot understand answer from “protocols” RPC:"
            %% code_block protocols)
    | Some p when String.is_prefix p ~prefix:"PsCARTHA" -> (`Carthage, p)
    | Some p when String.is_prefix p ~prefix:"PsDELPH1" -> (`Delphi, p)
    | Some p when String.is_prefix p ~prefix:"PtEdoTez" -> (`Edo, p)
    | Some p when String.is_prefix p ~prefix:"PtEdo2Zk" -> (`Edo, p)
    | Some p when String.is_prefix p ~prefix:"PsFLorena" -> (`Florence, p)
    | Some p when String.is_prefix p ~prefix:"PtGRANAD" -> (`Granada, p)
    | Some p when String.is_prefix p ~prefix:"PtHangzH" -> (`Hangzhou, p)
    | Some p ->
        logf "Can't recognize protocol: `%s` assuming Edo-like." p ;
        (`Granada, p) in
  logf "Protocol is `%s`" protocol_hash ;
  Node.get_storage ctxt node ~address
  >>= fun storage ->
  logf "Got the storage: %s" storage ;
  Fmt.kstr (Node.rpc_get ctxt node)
    "/chains/main/blocks/head/context/contracts/%s/script" address
  >>= fun script ->
  Fmt.kstr (Node.rpc_get ctxt node)
    "/chains/main/blocks/head/context/contracts/%s/balance" address
  >>= fun balance ->
  let balance = Ezjsonm.(value_from_string balance |> get_string) in
  Fmt.kstr (Node.rpc_get ctxt node) "/chains/main/chain_id"
  >>= fun chain_id ->
  let chain_id = Ezjsonm.(value_from_string chain_id |> get_string) in
  logf "Got the script: %s" (ellipsize_string script ~max_length:30) ;
  let contract_storage = Michelson.micheline_of_json storage in
  let `Contract view_contract, `Input view_input, `Storage view_storage =
    let code_mich = Michelson.micheline_of_json script in
    let open Micheline_helpers in
    let contract_storage_type =
      get_storage_type_exn (Tezos_micheline.Micheline.strip_locations code_mich)
    in
    let contract_parameter_type =
      get_parameter_type_exn
        (Tezos_micheline.Micheline.strip_locations code_mich) in
    let view_parameters =
      Tezos_micheline.Micheline.(strip_locations parameter |> root) in
    let view =
      (* TEMPORARY: this is one macro expansion for the test that is on
         carthagenet *)
      let code =
        match view.code with
        | Micheline mich ->
            let open Tezos_micheline.Micheline in
            let node = root mich in
            let rec go = function
              | (Int _ | String _ | Bytes _) as ok -> ok
              | Prim (_loc, "CDAR", [], _annot) ->
                  Seq
                    ( _loc
                    , [Prim (_loc, "CDR", [], []); Prim (_loc, "CAR", [], [])]
                    )
              | Prim (_loc, _prim, args, _annot) ->
                  Prim (_loc, _prim, List.map ~f:go args, _annot)
              | Seq (loc, args) -> Seq (loc, List.map ~f:go args) in
            go node |> strip_locations in
      {view with code= Micheline code} in
    build_off_chain_view_contract view ~contract_balance:(Z.of_string balance)
      ~contract_address:address ~contract_storage ~view_parameters
      ~contract_storage_type ~contract_parameter_type in
  logf "Made the view-script: %a" Micheline_helpers.pp_arbitrary_micheline
    view_contract ;
  logf "Made the view-input: %a" Micheline_helpers.pp_arbitrary_micheline
    view_input ;
  logf "Made the view-storage: %a" Micheline_helpers.pp_arbitrary_micheline
    view_storage ;
  let constructed =
    let michjson which mich =
      try Michelson.micheline_to_ezjsonm mich
      with e -> Fmt.failwith "micheline_to_ezjsonm '%s' → %a" which Exn.pp e
    in
    let open Ezjsonm in
    let normal_fields =
      [ ("script", michjson "script" view_contract)
      ; ("storage", michjson "storage" view_storage)
      ; ("input", michjson "input" view_input)
      ; ("amount", string "0")
      ; ("chain_id", string chain_id) ] in
    let fields =
      match protocol_kind with
      | `Edo | `Florence | `Granada | `Hangzhou ->
          normal_fields
          @ [ ("balance", string "0")
            ; ("unparsing_mode", string "Optimized_legacy") ]
      | `Carthage | `Delphi -> normal_fields in
    dict fields in
  logf "Calling `/run_code`: %s"
    ( try Ezjsonm.value_to_string constructed
      with e -> Fmt.failwith "JSON too deep for JS backend: %a" Exn.pp e ) ;
  Node.rpc_post ctxt ~node
    ~body:(Ezjsonm.value_to_string constructed)
    ( match protocol_kind with
    | `Edo -> "/chains/main/blocks/head/helpers/scripts/run_code/normalized"
    | _ -> "/chains/main/blocks/head/helpers/scripts/run_code" )
  >>= fun result ->
  logf "RESULT: %s" result ;
  let actual_result =
    let open Ezjsonm in
    let d = value_from_string result |> get_dict in
    let mich =
      match List.Assoc.find ~equal:String.equal d "storage" with
      | None -> Fmt.failwith "Result has not storage: %S" result
      | Some json -> Michelson.micheline_of_ezjsonm json in
    let open Tezos_micheline.Micheline in
    match mich with
    | Prim (_, "Some", [s], _) -> s
    | other ->
        Fmt.failwith "Result is not (Some _): %a"
          Micheline_helpers.pp_arbitrary_micheline other in
  Lwt.return (Ok (actual_result, contract_storage))
