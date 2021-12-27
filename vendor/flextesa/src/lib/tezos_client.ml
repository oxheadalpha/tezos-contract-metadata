open Internal_pervasives

type t = {id: string; port: int; exec: Tezos_executable.t}
type client = t

let no_node_client ~exec = {id= "C-null"; port= 0; exec}

let of_node ~exec n =
  let id = sprintf "C-%s" n.Tezos_node.id in
  let port = n.Tezos_node.rpc_port in
  {id; port; exec}

let base_dir t ~state = Paths.root state // sprintf "Client-base-%s" t.id

open Tezos_executable.Make_cli

let client_call ?(wait = "none") state t args =
  ("--wait" :: wait :: optf "port" "%d" t.port)
  @ opt "base-dir" (base_dir ~state t)
  @ args

let client_command ?wait state t args =
  Tezos_executable.call state t.exec
    ~path:(base_dir t ~state // "exec-client")
    (client_call ?wait state t args)

module Command_error = struct
  let failf ?result ?client ?args fmt =
    let attach =
      Option.value_map ~default:[] args ~f:(fun l ->
          [("arguments", `String_list l)] )
      @ Option.value_map ~default:[] client ~f:(fun c ->
            [("client-id", `String_value c.id)] )
      @ Option.value_map ~default:[] result ~f:(fun res ->
            [("stdout", `Verbatim res#out); ("stderr", `Verbatim res#err)] )
    in
    Process_result.Error.wrong_behavior ~attach fmt
end

open Command_error
open Console

let run_client_cmd ?id_prefix ?wait state client args =
  Running_processes.run_cmdf ?id_prefix state "sh -c %s"
    ( client_command ?wait state client args
    |> Genspio.Compile.to_one_liner |> Caml.Filename.quote )

let client_cmd ?id_prefix ?(verbose = true) ?wait state ~client args =
  Running_processes.run_cmdf ?id_prefix state "sh -c %s"
    ( client_command ?wait state client args
    |> Genspio.Compile.to_one_liner |> Caml.Filename.quote )
  >>= fun res ->
  let unix_success = Poly.equal res#status (Lwt_unix.WEXITED 0) in
  ( if verbose then
    Console.display_errors_of_command state res >>= fun _ -> return ()
  else return () )
  >>= fun () -> return (unix_success, res)

let successful_client_cmd ?id_prefix ?(verbose = true) ?wait state ~client args
    =
  client_cmd ?id_prefix ~verbose state ?wait ~client args
  >>= fun (success, result) ->
  match success with
  | true -> return result
  | false ->
      failf ~result ~client ~args "Client-command failure: %s"
        (String.concat ~sep:" " args)

let wait_for_node_bootstrap state client =
  let try_once () =
    run_client_cmd
      ~id_prefix:(client.id ^ "-bootstrapped")
      state client ["bootstrapped"]
    >>= fun res -> return Poly.(res#status = Unix.WEXITED 0) in
  let attempts = 20 in
  let rec loop nth =
    if nth >= attempts then failf "Bootstrapping failed %d times." nth
    else
      try_once ()
      >>= function
      | true -> return ()
      | false ->
          System.sleep Float.(0.3 + (of_int nth * 0.6))
          >>= fun () -> loop (nth + 1) in
  loop 1

let import_secret_key state client ~name ~key =
  successful_client_cmd state ~client
    ["import"; "secret"; "key"; name; key; "--force"]
  >>= fun _ -> return ()

let register_as_delegate state client ~key_name =
  successful_client_cmd state ~client
    ["register"; "key"; key_name; "as"; "delegate"]
  >>= fun _ -> return ()

let rpc state ~client meth ~path =
  let args =
    match meth with
    | `Get -> ["rpc"; "get"; path]
    | `Post s -> ["rpc"; "post"; path; "with"; s] in
  successful_client_cmd state ~client args
  >>= fun res ->
  let output = String.concat ~sep:"\n" res#out in
  try
    let json = Jqo.of_string output in
    return json
  with e -> (
    try
      Ezjsonm.from_string (sprintf "[ %s ]" output)
      |> function `A [one] -> return one | _ -> raise e
    with e ->
      say state
        EF.(
          list
            [ desc (shout "Output:") (markdown_verbatim output)
            ; desc (shout "Error:")
                (markdown_verbatim (String.concat ~sep:"\n" res#err)) ])
      >>= fun () ->
      failf ~args "RPC failure cannot parse json: %s" Exn.(to_string e) )

let activate_protocol state client protocol =
  let timestamp =
    match protocol.Tezos_protocol.timestamp_delay with
    | None -> []
    | Some delay -> (
        let now = Ptime_clock.now () in
        match Ptime.add_span now (Ptime.Span.of_int_s delay) with
        | None ->
            invalid_arg "activate_protocol_script: protocol.timestamp_delay"
        | Some x -> ["--timestamp"; Ptime.to_rfc3339 x] ) in
  Console.say state
    EF.(wf "Activating protocol %s" protocol.Tezos_protocol.hash)
  >>= fun () ->
  import_secret_key state client
    ~name:(Tezos_protocol.dictator_name protocol)
    ~key:(Tezos_protocol.dictator_secret_key protocol)
  >>= fun () ->
  successful_client_cmd state ~client
    ( opt "block" "genesis"
    @ [ "activate"; "protocol"; protocol.Tezos_protocol.hash; "with"; "fitness"
      ; sprintf "%d" protocol.Tezos_protocol.expected_pow; "and"; "key"
      ; Tezos_protocol.dictator_name protocol; "and"; "parameters"
      ; Tezos_protocol.protocol_parameters_path state protocol ]
    @ timestamp )
  >>= fun _ ->
  rpc state ~client `Get ~path:"/chains/main/blocks/head/metadata"
  >>= fun metadata_json ->
  ( match Jqo.field metadata_json ~k:"next_protocol" with
  | `String hash when String.equal hash protocol.Tezos_protocol.hash ->
      return ()
  | exception e ->
      System_error.fail_fatalf "Error getting protocol metadata: %a" Exn.pp e
  | other_value ->
      System_error.fail_fatalf "Error activating protocol: %s Vs %s"
        (Ezjsonm.value_to_string other_value)
        protocol.Tezos_protocol.hash )
  >>= fun () -> return ()

let find_applied_in_mempool state ~client ~f =
  successful_client_cmd state ~client
    ["rpc"; "get"; "/chains/main/mempool/pending_operations"]
  >>= fun res ->
  try
    let json = Jqo.of_string (String.concat ~sep:"\n" res#out) in
    let found = Jqo.field ~k:"applied" json |> Jqo.list_find ~f in
    say state
      EF.(
        desc
          (af "piece of mempool found (client %s):" client.id)
          (markdown_verbatim (Ezjsonm.to_string json)))
    >>= fun () -> return (Some found)
  with e ->
    say state
      EF.(desc (shout "not found in mempool") (af "%s" (Exn.to_string e)))
    >>= fun () -> return None

let mempool_has_operation state ~client ~kind =
  find_applied_in_mempool state ~client ~f:(fun o ->
      Jqo.field o ~k:"contents"
      |> Jqo.list_exists
           ~f:Poly.(fun op -> Jqo.field op ~k:"kind" = `String kind) )
  >>= fun found_or_not -> return Poly.(found_or_not <> None)

let block_has_operation state ~client ~level ~kind =
  successful_client_cmd state ~client
    ["rpc"; "get"; sprintf "/chains/main/blocks/%d/operations" level]
  >>= fun res ->
  try
    let json = Jqo.of_string (String.concat ~sep:"\n" res#out) in
    let found =
      Jqo.list_exists json ~f:(fun olist ->
          Jqo.list_exists olist ~f:(fun o ->
              Jqo.field o ~k:"contents"
              |> Jqo.list_exists
                   ~f:Poly.(fun op -> Jqo.field op ~k:"kind" = `String kind) ) )
    in
    say state
      EF.(
        desc
          (af "looking for %S in block %d: %sfound" kind level
             (if found then "" else "not ") )
          (af "%s" (Ezjsonm.to_string json)))
    >>= fun () -> return found
  with e ->
    say state
      EF.(
        desc
          (ksprintf shout "Operation %S not found in block" kind)
          (af "%s" (Exn.to_string e)))
    >>= fun () -> return false

let get_block_header state ~client block =
  let path =
    sprintf "/chains/main/blocks/%s/header"
      (match block with `Head -> "head" | `Level i -> Int.to_string i) in
  rpc state ~client `Get ~path

let list_known_addresses state ~client =
  successful_client_cmd state ~client ["list"; "known"; "addresses"]
  >>= fun res ->
  let re =
    Re.(
      compile
        (seq
           [ group (rep1 (alt [alnum; char '_'])); str ": "; group (rep1 alnum)
           ; alt [space; eol; eos] ] )) in
  return
    (List.filter_map res#out
       ~f:
         Re.(
           fun line ->
             match exec_opt re line with
             | None -> None
             | Some matches -> Some (Group.get matches 1, Group.get matches 2)) )

let rec prefix_from_list ~prefix = function
  | [] -> None
  | x :: xs ->
      if not (String.is_prefix x ~prefix) then prefix_from_list ~prefix xs
      else
        Some
          (String.lstrip
             (String.chop_prefix x ~prefix |> Option.value ~default:x) )

let parse_account ~name ~lines =
  Option.(
    prefix_from_list ~prefix:"Hash:" lines
    >>= fun pubkey_hash ->
    prefix_from_list ~prefix:"Public Key:" lines
    >>= fun pubkey ->
    prefix_from_list ~prefix:"Secret Key:" lines
    >>= fun private_key ->
    return
      (Tezos_protocol.Account.key_pair name ~pubkey ~pubkey_hash ~private_key))

let get_account state ~client ~name =
  successful_client_cmd state ~client ["show"; "address"; name; "--show-secret"]
  >>= fun res -> return (parse_account ~name ~lines:res#out)

let show_known_contract state client ~name =
  successful_client_cmd state ~client ["show"; "known"; "contract"; name]
  >>= fun res -> return (String.concat res#out)

let deploy_multisig ?counter state client ~name ~amt ~from_acct ~threshold
    ~signer_names ~burn_cap =
  let counter_args =
    match counter with Some c -> ["--counter"; Int.to_string c] | None -> []
  in
  client_cmd state ~client
    (List.concat
       [ [ "deploy"; "multisig"; name; "transferring"; sprintf "%f" amt; "from"
         ; from_acct; "with"; "threshold"; Int.to_string threshold; "on"
         ; "public"; "keys" ]; signer_names
       ; ["--burn-cap"; sprintf "%f" burn_cap; "--force"]; counter_args ] )
  >>= fun _ -> return ()

let sign_multisig state client ~contract ~amt ~to_acct ~signer_name =
  let params =
    [ "sign"; "multisig"; "transaction"; "on"; contract; "transferring"
    ; sprintf "%f" amt; "to"; to_acct; "using"; "secret"; "key"; signer_name ]
  in
  client_cmd state ~client params
  >>= fun (_, sign_res) -> return (String.concat ~sep:"" sign_res#out)

let transfer_from_multisig ?counter state client ~name ~amt ~to_acct
    ~on_behalf_acct ~signatures ~burn_cap =
  let counter_args =
    match counter with Some c -> ["--counter"; Int.to_string c] | None -> []
  in
  client_cmd state ~client
    (List.concat
       [ [ "from"; "multisig"; "contract"; name; "transfer"; sprintf "%f" amt
         ; "to"; to_acct; "on"; "behalf"; "of"; on_behalf_acct; "with"
         ; "signatures" ]; signatures; ["--burn-cap"; sprintf "%f" burn_cap]
       ; counter_args ] )
  >>= fun _ -> return ()

let hash_data state ?gas client ~data_to_hash ~data_type =
  let the_list = ["hash"; "data"; data_to_hash; "of"; "type"; data_type] in
  let the_list' =
    match gas with
    | None -> the_list
    | Some g -> the_list @ ["--gas"; Int.to_string g] in
  successful_client_cmd state ~client the_list'
  >>= fun res ->
  let res_out = List.hd_exn res#out in
  let cleaned =
    match String.chop_prefix res_out ~prefix:"Raw packed data: " with
    | Some s -> s
    | None -> res_out in
  return cleaned

let multisig_storage_counter state client contract_id =
  let path =
    sprintf "/chains/main/blocks/head/context/contracts/%s/storage" contract_id
  in
  rpc state ~client `Get ~path
  >>= fun sto ->
  let args_array = Jqo.field ~k:"args" sto in
  let fst_arg = Jqo.get_list_element args_array 0 in
  let counter_val = Jqo.field ~k:"int" fst_arg in
  try return (Int.of_string (Jqo.get_string counter_val))
  with e -> System_error.fail_fatalf "Exception getting counter: %a" Exn.pp e

module Ledger = struct
  type hwm = {main: int; test: int; chain: string option}

  let set_hwm state ~client ~uri ~level =
    successful_client_cmd state ~client
      [ "set"; "ledger"; "high"; "watermark"; "for"; uri; "to"
      ; Int.to_string level ]
    >>= fun _ -> return ()

  let get_hwm state ~client ~uri =
    successful_client_cmd state ~client
      [ "get"; "ledger"; "high"; "watermark"; "for"; uri
      ; "--no-legacy-instructions" ]
    (* TODO: Use --for-script when available *)
    >>= fun res ->
    (* e.g. The high water mark values for married-bison-ill-burmese/P-256 are
            0 for the main-chain (NetXH12Aer3be93) and
            0 for the test-chain. *)
    let re =
      Re.(
        let num = rep1 digit in
        compile
          (seq
             [ group num; str " for the main-chain ("; group (rep1 alnum)
             ; str ") and "; group num; str " for the test-chain." ] )) in
    let matches = Re.exec re (String.concat ~sep:" " res#out) in
    try
      return
        { main= Int.of_string (Re.Group.get matches 1)
        ; chain=
            (let v = Re.Group.get matches 2 in
             if String.equal v "'Unspecified'" then None
             else Some (Tezai_base58_digest.Identifier.Chain_id.decode v) )
        ; test= Int.of_string (Re.Group.get matches 3) }
    with e ->
      failf
        "Couldn't understand result of 'get high watermark for %S': error %S: \
         from %S"
        uri (Exn.to_string e)
        (String.concat ~sep:"\n" res#out)

  let show_ledger state ~client ~uri =
    successful_client_cmd state ~client ["show"; "ledger"; uri]
    (* TODO: Use --for-script when available *)
    >>= fun res ->
    list_known_addresses state ~client
    >>= fun known_addresses ->
    let pk = Re.(rep1 alnum) in
    let addr_re = Re.(compile (seq [str "* Public Key Hash: "; group pk])) in
    let pubkey_re = Re.(compile (seq [str "* Public Key: "; group pk])) in
    let out = String.concat ~sep:" " res#out in
    try
      let pubkey = Re.(Group.get (exec pubkey_re out) 1) in
      let pubkey_hash = Re.(Group.get (exec addr_re out) 1) in
      let name =
        match
          List.find known_addresses ~f:(fun (_, pkh) ->
              String.equal pkh pubkey_hash )
        with
        | None -> ""
        | Some (alias, _) -> alias in
      return
        (Tezos_protocol.Account.key_pair name ~pubkey ~pubkey_hash
           ~private_key:uri )
    with e ->
      failf "Couldn't understand result of 'show ledger %S': error %S: from %S"
        uri (Exn.to_string e)
        (String.concat ~sep:"\n" res#out)

  let deauthorize_baking state ~client ~uri =
    successful_client_cmd state ~client
      ["deauthorize"; "ledger"; "baking"; "for"; uri]
    >>= fun _ -> return ()

  let get_authorized_key state ~client ~uri =
    successful_client_cmd state ~client
      ["get"; "ledger"; "authorized"; "path"; "for"; uri]
    >>= fun res ->
    let re_uri =
      Re.(compile (seq [str "Authorized baking URI: "; group (rep1 any); eol]))
    in
    let re_none = Re.(compile (str "No baking key authorized")) in
    let out = String.concat ~sep:" " res#out in
    return
      Re.(
        match exec_opt re_none out with
        | Some _ -> None
        | None -> Some (Group.get (exec re_uri out) 1))
end

module Keyed = struct
  type t = {client: client; key_name: string; secret_key: string}

  let make client ~key_name ~secret_key = {client; key_name; secret_key}

  let initialize state {client; key_name; secret_key} =
    successful_client_cmd state ~client
      ["import"; "secret"; "key"; key_name; secret_key; "--force"]

  let bake ?chain state baker msg =
    let chain_arg =
      Option.value_map chain ~default:[] ~f:(fun c -> ["--chain"; c]) in
    successful_client_cmd state ~client:baker.client
      ( chain_arg
      @ ["bake"; "for"; baker.key_name; "--force"; "--minimal-timestamp"] )
    >>= fun res ->
    Log_recorder.Operations.bake state ~client:baker.client.id ~output:res#out
      msg ;
    say state
      EF.(
        desc
          (af "Successful bake (%s: %s):" baker.client.id msg)
          (ocaml_string_list res#out))

  let endorse state baker msg =
    successful_client_cmd state ~client:baker.client
      ["endorse"; "for"; baker.key_name]
    >>= fun res ->
    Log_recorder.Operations.endorse state ~client:baker.client.id
      ~output:res#out msg ;
    say state
      EF.(
        desc
          (af "Successful bake (%s: %s):" baker.client.id msg)
          (ocaml_string_list res#out))

  let generate_nonce state {client; key_name; _} data =
    successful_client_cmd state ~client
      ["generate"; "nonce"; "hash"; "for"; key_name; "from"; data]
    >>= fun res -> return (List.hd_exn res#out)

  let sign_bytes state client ~bytes ~key_name =
    successful_client_cmd state ~client:client.client
      ["sign"; "bytes"; bytes; "for"; key_name]
    >>= fun sign_res -> return (List.hd_exn sign_res#out)

  let forge_and_inject state keyed_client ~json =
    rpc state ~client:keyed_client.client
      ~path:"/chains/main/blocks/head/helpers/forge/operations"
      (`Post (Ezjsonm.value_to_string json))
    >>= fun res ->
    let operation_bytes = match res with `String s -> s | _ -> assert false in
    let bytes_to_sign = "0x03" ^ operation_bytes in
    sign_bytes state keyed_client ~bytes:bytes_to_sign (*operation_bytes*)
      ~key_name:keyed_client.key_name
    >>= fun sign_res ->
    let to_decode =
      String.chop_prefix_exn ~prefix:"Signature:" sign_res |> String.strip in
    Dbg.e EF.(af "To Decode: %s" to_decode) ;
    let decoded =
      Tezai_base58_digest.Identifier.Ed25519.Signature.decode to_decode
      |> Hex.of_string ?ignore:None |> Hex.show in
    say state EF.(desc (shout "DECODED:") (af "%S" decoded))
    >>= fun () ->
    say state
      EF.(
        desc_list (af "Injecting Operation")
          [ ef_json "Injecting" (json :> Ezjsonm.value)
          ; desc (haf "op:")
              (af "%d: %S" (String.length operation_bytes) operation_bytes)
          ; desc (haf "sign:") (af "%d: %S" (String.length decoded) decoded) ])
    >>= fun () ->
    rpc state ~client:keyed_client.client
      ~path:"/injection/operation?chain=main"
      (`Post (sprintf "\"%s%s\"" operation_bytes decoded))

  let find_mempool_counter_exn (json : Ezjsonm.value) hash_key : int =
    match json with
    | `O _ -> (
        let z = Jqo.field ~k:"applied" json in
        match z with
        | `A trans_list ->
            let foldf acc x =
              let contents_list = Jqo.field ~k:"contents" x in
              let more_counters =
                Jqo.match_in_array "source" hash_key "counter" contents_list
              in
              more_counters @ acc in
            let to_ints (strs : string list) : int list =
              List.map strs ~f:(fun s -> Int.of_string s) in
            let to_max_int (ints : int list) : int =
              List.fold ints ~init:0 ~f:(fun acc x -> Int.max acc x) in
            let counters = List.fold trans_list ~init:[] ~f:foldf in
            let counter_strs =
              List.map ~f:(fun v -> Jqo.get_string v) counters in
            let max_int = to_max_int (to_ints counter_strs) in
            max_int
        | _ -> 0 )
    | _ -> 0

  let operations_from_chain state keyed_client =
    rpc state ~client:keyed_client.client `Get
      ~path:(Fmt.str "/chains/main/blocks/head/operations")
    >>= fun ops_json -> return ops_json

  let find_contract_id_exn (json : Ezjsonm.value) (orig_hash : string) =
    let ops = Jqo.get_list_element json 3 in
    let op = Jqo.match_in_array_first "hash" orig_hash "contents" ops in
    let meta = Jqo.match_in_array_first "kind" "origination" "metadata" op in
    let res = Jqo.field ~k:"operation_result" meta in
    let orig_list = Jqo.field ~k:"originated_contracts" res in
    Jqo.get_string (Jqo.get_list_element orig_list 0)

  let get_contract_id state client origination_hash =
    operations_from_chain state client
    >>= fun ops_json ->
    try return (find_contract_id_exn ops_json origination_hash)
    with e ->
      System_error.fail_fatalf "Exception getting contract_id: %a" Exn.pp e

  let counter_from_chain state keyed_client =
    get_account state ~client:keyed_client.client ~name:keyed_client.key_name
    >>= fun acct ->
    match acct with
    | None ->
        System_error.fail_fatalf "counter_from_chain - failed to parse account."
    | Some a ->
        let src = Tezos_protocol.Account.pubkey_hash a in
        rpc state ~client:keyed_client.client `Get
          ~path:
            (Fmt.str "/chains/main/blocks/head/context/contracts/%s/counter" src)
        >>= fun counter_json ->
        return (Jqo.get_string counter_json |> Int.of_string)

  let update_counter ?current_counter_override state client _dbg_str =
    let the_match =
      match current_counter_override with
      | None -> counter_from_chain state client
      | Some c -> return (Int.max (c - 1) 0) in
    the_match
    >>= fun current_counter ->
    rpc state ~client:client.client `Get
      ~path:"/chains/main/mempool/pending_operations"
    >>= fun json ->
    let pubkey_hash = Tezos_protocol.Key.Of_name.pubkey_hash client.key_name in
    let new_counter =
      try find_mempool_counter_exn json pubkey_hash with _ -> current_counter
    in
    let max = Int.max current_counter new_counter + 1 in
    return max
end
