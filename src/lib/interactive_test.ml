open Internal_pervasives
open Traffic_generation.Commands

module Commands = struct
  let no_args = function
    | [] -> return ()
    | _more -> cmdline_fail "this command expects no arguments"

  let flag f sexps = List.mem sexps (Base.Sexp.Atom f) ~equal:Base.Sexp.equal

  let unit_loop_no_args ~description opts f =
    Console.Prompt.unit_and_loop ~description opts (fun sexps ->
        no_args sexps >>= fun () -> f () )

  let du_sh_root state =
    unit_loop_no_args
      ~description:(Fmt.str "Run du -sh on %s" (Paths.root state))
      ["d"; "du-root"]
      (fun () ->
        Running_processes.run_cmdf state "du -sh %s" (Paths.root state)
        >>= fun du ->
        Console.display_errors_of_command state du
        >>= function
        | true ->
            Console.say state
              EF.(
                desc (haf "Disk-Usage:")
                  (af "%s" (String.concat ~sep:" " du#out)))
        | false -> return () )

  let processes state =
    Console.Prompt.unit_and_loop
      ~description:
        "Display status of processes-manager ('all' to include non-running)"
      ["p"; "processes"] (fun sxp ->
        let all = flag "all" sxp in
        Console.say state (Running_processes.ef ~all state) )

  let do_jq _state ~msg ~f = function
    | None -> cmdline_fail "%s: No JSON" msg
    | Some json -> (
      try return (f json)
      with e ->
        cmdline_fail "%s: failed to analyze JSON: %s from %s" msg
          (Exn.to_string e)
          (Ezjsonm.value_to_string ~minify:false json) )

  let curl_unit_display ?(jq = fun e -> e) ?pp_json state cmd ~default_port
      ~path ~doc =
    let pp_option =
      Option.value_map pp_json ~default:[] ~f:(fun _ ->
          [ Sexp_options.make_option "raw"
              "Do not try to pretty print the output." ] ) in
    let get_pp_json _state sexps =
      match pp_json with
      | None -> return More_fmt.json
      | Some _
        when List.exists sexps
               ~f:Sexp.(function List [Atom "raw"] -> true | _ -> false) ->
          return More_fmt.json
      | Some default -> return default in
    Console.Prompt.unit_and_loop ~description:doc
      ~details:
        (Sexp_options.pp_options
           (pp_option @ [Sexp_options.port_number_doc state ~default_port]) )
      cmd
      (fun sexps ->
        Sexp_options.port_number state sexps ~default_port
        >>= fun port ->
        get_pp_json state sexps
        >>= fun pp_json ->
        Helpers.curl_rpc_cmd state ~port ~path
        >>= fun json_opt ->
        do_jq ~msg:doc state json_opt ~f:jq
        >>= fun processed_json ->
        Console.sayf state
          More_fmt.(
            fun ppf () ->
              vertical_box ~indent:2 ppf (fun ppf ->
                  pf ppf "Curl-node:%d -> %s" port doc ;
                  cut ppf () ;
                  pp_json ppf processed_json )) )

  let curl_metadata state ~default_port =
    curl_unit_display state ["m"; "metadata"] ~default_port
      ~path:"/chains/main/blocks/head/metadata"
      ~doc:"Display `/chains/main/blocks/head/metadata`."

  let curl_level state ~default_port =
    curl_unit_display state ["l"; "level"] ~default_port
      ~path:"/chains/main/blocks/head" ~doc:"Display current head block info."
      ~pp_json:Tezos_protocol.Pretty_print.(verbatim_protection block_head_rpc)

  let curl_baking_rights state ~default_port =
    curl_unit_display state ["bk"; "baking-rights"] ~default_port
      ~path:"/chains/main/blocks/head/helpers/baking_rights"
      ~doc:"Display baking rights."

  let all_levels state ~nodes =
    unit_loop_no_args ~description:"Get all “head” levels of all the nodes."
      ["al"; "all-levels"] (fun () ->
        Test_scenario.Queries.all_levels state ~nodes
        >>= fun results ->
        Console.say state
          EF.(
            desc (af "Node-levels:")
              (list
                 (List.map results ~f:(fun (id, result) ->
                      desc (haf "%s" id)
                        ( match result with
                        | `Failed -> af "Failed"
                        | `Level i -> af "[%d]" i
                        | `Null -> af "{Null}"
                        | `Unknown s -> af "¿%s?" s ) ) ) )) )

  let mempool state ~default_port =
    curl_unit_display state ["mp"; "mempool"] ~default_port
      ~path:"/chains/main/mempool/pending_operations"
      ~doc:"Display the status of the mempool."
      ~pp_json:
        Tezos_protocol.Pretty_print.(
          verbatim_protection mempool_pending_operations_rpc)

  let show_process state =
    Console.Prompt.unit_and_loop
      ~description:"Show more of a process (by name-prefix)." ["show"] (function
      | [Atom name] ->
          let prefix = String.lowercase name in
          Running_processes.find_process_by_id state ~f:(fun n ->
              String.is_prefix (String.lowercase n) ~prefix )
          >>= fun procs ->
          List.fold procs ~init:(return []) ~f:(fun prevm {process; lwt} ->
              prevm
              >>= fun prev ->
              let open Running_processes in
              let out = output_path state process `Stdout in
              let err = output_path state process `Stderr in
              Running_processes.run_cmdf state "tail %s" out
              >>= fun tailout ->
              Running_processes.run_cmdf state "tail %s" err
              >>= fun tailerr ->
              return
                EF.(
                  desc_list
                    (haf "%S (%d)" process.Process.id lwt#pid)
                    [ desc (af "out: %s" out) (ocaml_string_list tailout#out)
                    ; desc (af "err: %s" err) (ocaml_string_list tailerr#out) ]
                  :: prev) )
          >>= fun ef -> Console.say state EF.(list ef)
      | _other -> cmdline_fail "command expects 1 argument: name-prefix" )

  let kill_all state =
    unit_loop_no_args ~description:"Kill all known processes/process-groups."
      ["ka"; "killall"] (fun () -> Running_processes.kill_all state)

  let secret_keys state ~protocol =
    unit_loop_no_args ~description:"Show the protocol's “bootstrap” accounts."
      ["boa"; "bootstrap-accounts"] (fun () ->
        Console.sayf state
          More_fmt.(
            fun ppf () ->
              vertical_box ~indent:0 ppf (fun ppf ->
                  prompt ppf (fun ppf -> pf ppf "Bootstrap Accounts:") ;
                  List.iter (Tezos_protocol.bootstrap_accounts protocol)
                    ~f:(fun acc ->
                      let open Tezos_protocol.Account in
                      cut ppf () ;
                      pf ppf "* Account %S:@," (name acc) ;
                      pf ppf "  * Public Key Hash: %s@," (pubkey_hash acc) ;
                      pf ppf "  * Public Key:      %s@," (pubkey acc) ;
                      pf ppf "  * Private Key:     %s" (private_key acc) ) )) )

  let show_connections state nodes =
    unit_loop_no_args ~description:"Show all node connections"
      ["ac"; "all-connections"] (fun () ->
        Helpers.dump_connections state nodes )

  let balances state ~default_port =
    Console.Prompt.unit_and_loop
      ~description:"Show the balances of all known accounts."
      ~details:
        (Sexp_options.pp_options
           [Sexp_options.port_number_doc state ~default_port] )
      ["sb"; "show-balances"]
      (fun sexps ->
        Sexp_options.port_number state sexps ~default_port
        >>= fun port ->
        Helpers.curl_rpc_cmd state ~port
          ~path:"/chains/main/blocks/head/context/contracts"
        >>= fun json_opt ->
        do_jq state ~msg:"Getting contract list" ~f:Jqo.get_strings json_opt
        >>= fun contracts ->
        Helpers.curl_rpc_cmd state ~port ~path:"/chains/main/checkpoint"
        >>= fun chkpto ->
        do_jq state chkpto ~msg:"Getting checkpoint"
          ~f:
            Jqo.(
              fun json ->
                match field ~k:"history_mode" json |> get_string with
                | "archive" -> 1
                | _ ->
                    let sp = field ~k:"save_point" json |> get_int in
                    Int.max 1 sp)
        >>= fun save_point ->
        let balance block contract =
          let path =
            sprintf "/chains/main/blocks/%s/context/contracts/%s/balance" block
              contract in
          Helpers.curl_rpc_cmd state ~port ~path
          >>= fun jo ->
          do_jq state jo ~msg:"Getting balance" ~f:(fun j ->
              Jqo.get_string j |> Int.of_string ) in
        List.fold contracts ~init:(return []) ~f:(fun prevm hsh ->
            prevm
            >>= fun prev ->
            balance (Int.to_string save_point) hsh
            >>= fun init ->
            balance "head" hsh
            >>= fun current -> return ((hsh, init, current) :: prev) )
        >>= fun results ->
        Console.say state
          EF.(
            desc_list
              (af "Balances from levels %d to “head” (port :%d)" save_point port)
              (List.map results ~f:(fun (hsh, init, cur) ->
                   let tz i = Float.of_int i /. 1_000_000. in
                   desc (haf "%s:" hsh)
                     ( if init = cur then af "%f (unchanged)" (tz cur)
                     else
                       af "%f → %f (delta: %f)" (tz init) (tz cur)
                         (tz (cur - init)) ) ) )) )

  let better_call_dev state ~default_port =
    Console.Prompt.unit_and_loop
      ~description:"Show URIs to all contracts with `better-call.dev`."
      ~details:
        (Sexp_options.pp_options
           [Sexp_options.port_number_doc state ~default_port] )
      ["bcd"; "better-call-dev"]
      (fun sexps ->
        Sexp_options.port_number state sexps ~default_port
        >>= fun port ->
        Helpers.curl_rpc_cmd state ~port
          ~path:"/chains/main/blocks/head/context/contracts"
        >>= fun json_opt ->
        do_jq state ~msg:"Getting contract list" ~f:Jqo.get_strings json_opt
        >>= fun contracts ->
        let kt1s =
          List.filter contracts ~f:(fun c -> String.is_prefix c ~prefix:"KT1")
        in
        Console.sayf state
          Fmt.(
            fun ppf () ->
              vbox ~indent:2
                (fun ppf () ->
                  let block_url_arg =
                    kstr Uri.pct_encode
                      "blockUrl=http://127.0.0.1:%d/chains/main/blocks" port
                  in
                  let base =
                    Environment_configuration.better_call_dev_base_url state
                  in
                  match kt1s with
                  | [] ->
                      pf ppf "There are no KT1 contracts in this sandbox." ;
                      cut ppf () ;
                      pf ppf "You can still go to %s?%s" base block_url_arg
                  | some_kt1s ->
                      pf ppf "Links:" ;
                      cut ppf () ;
                      List.iter some_kt1s ~f:(fun c ->
                          if String.is_prefix c ~prefix:"KT1" then (
                            pf ppf "* %s/%s/operations?%s" base c block_url_arg ;
                            cut ppf () ) ) )
                ppf ()) )

  let arbitrary_command_on_all_clients ?make_admin
      ?(command_names = ["atc"; "all-clients"]) state ~clients =
    let details =
      let open Sexp_options in
      let only_opt =
        make_option "only"
          ~placeholders:["<name1>"; "<name2>"; "..."]
          "Restrict the clients by name." in
      let all =
        (match clients with [_] -> [] | _ -> [only_opt])
        @ Option.value_map make_admin ~default:[] ~f:(fun _ ->
              [make_option "admin" "Use the admin-client instead."] ) in
      match all with [] -> None | _ -> Some (pp_options all) in
    Console.Prompt.unit_and_loop
      ~description:
        (Fmt.str "Run a tezos-client command on %s"
           ( match clients with
           | [] -> "NO CLIENT, so this is useless…"
           | [one] -> sprintf "the %S client." one.Tezos_client.id
           | more ->
               sprintf "all the following clients: %s."
                 ( List.map more ~f:(fun c -> c.Tezos_client.id)
                 |> String.concat ~sep:", " ) ) )
      ?details command_names
      (fun sexps ->
        let args =
          let open Base.Sexp in
          List.filter_map sexps ~f:(function Atom s -> Some s | _ -> None) in
        let subset_of_clients =
          let open Base.Sexp in
          List.find_map sexps ~f:(function
            | List (Atom "only" :: l) ->
                Some
                  (List.map l ~f:(function
                    | Atom a -> a
                    | other ->
                        ksprintf failwith
                          "Option `only` only accepts a list of names: %s"
                          (to_string_hum other) ) )
            | _ -> None )
          |> function
          | None -> clients
          | Some more ->
              List.filter clients ~f:(fun c ->
                  List.mem more c.Tezos_client.id ~equal:String.equal ) in
        let use_admin =
          match make_admin with
          | None -> `Client
          | Some of_client ->
              if
                List.exists sexps
                  ~f:
                    Base.Sexp.(
                      function List [Atom "admin"] -> true | _ -> false)
              then `Admin of_client
              else `Client in
        List.fold ~init:(return []) subset_of_clients ~f:(fun prevm client ->
            prevm
            >>= fun prev ->
            Running_processes.run_cmdf state "sh -c %s"
              ( ( match use_admin with
                | `Client -> Tezos_client.client_command state client args
                | `Admin mkadm ->
                    Tezos_admin_client.make_command state (mkadm client) args )
              |> Genspio.Compile.to_one_liner |> Caml.Filename.quote )
            >>= fun res ->
            Console.display_errors_of_command state res
            >>= function
            | true -> return ((client, String.concat ~sep:"\n" res#out) :: prev)
            | false -> return prev )
        >>= fun results ->
        let different_results =
          List.dedup_and_sort results ~compare:(fun (_, a) (_, b) ->
              String.compare a b ) in
        Console.say state
          EF.(
            desc_list (af "Done")
              [ desc (haf "Command:")
                  (ocaml_string_list
                     ( ( match use_admin with
                       | `Client -> "<client>"
                       | `Admin _ -> "<admin>" )
                     :: args ) )
              ; desc (haf "Results")
                  (list
                     (List.map different_results ~f:(fun (_, res) ->
                          let clients =
                            List.filter_map results ~f:(function
                              | c, r when String.equal res r ->
                                  Some c.Tezos_client.id
                              | _ -> None ) in
                          desc
                            (haf "Client%s %s:"
                               ( if List.length subset_of_clients = 1 then ""
                               else "s" )
                               (String.concat ~sep:", " clients) )
                            (markdown_verbatim res) ) ) ) ]) )

  let arbitrary_commands_for_each_client ?make_admin
      ?(make_command_names = fun i -> [sprintf "c%d" i; sprintf "client-%d" i])
      state ~clients =
    List.mapi clients ~f:(fun i c ->
        arbitrary_command_on_all_clients state ?make_admin ~clients:[c]
          ~command_names:(make_command_names i) )

  let arbitrary_commands_for_each_and_all_clients ?make_admin
      ?make_individual_command_names ?all_clients_command_names state ~clients =
    arbitrary_command_on_all_clients state ?make_admin ~clients
      ?command_names:all_clients_command_names
    :: arbitrary_commands_for_each_client state ?make_admin ~clients
         ?make_command_names:make_individual_command_names

  let client_list_in_help_messages clients =
    match clients with
    | [] -> "NO CLIENT, this is just wrong"
    | [one] -> one.Tezos_client.Keyed.client.id
    | m ->
        Fmt.str "one of %s"
          ( List.mapi m ~f:(fun ith one ->
                Fmt.str "%d: %s" ith one.Tezos_client.Keyed.client.id )
          |> String.concat ~sep:", " )

  let bake_command state ~clients =
    Console.Prompt.unit_and_loop
      ~description:
        Fmt.(
          str "Manually bake a block (with %s)."
            (client_list_in_help_messages clients))
      ["bake"]
      (fun sexps ->
        let client =
          let open Base.Sexp in
          match sexps with
          | [] -> List.nth_exn clients 0
          | [Atom s] -> List.nth_exn clients (Int.of_string s)
          | _ -> Fmt.kstr failwith "Wrong command line: %a" pp (List sexps)
        in
        protect_with_keyed_client "manual-baking" ~client ~f:(fun () ->
            Tezos_client.Keyed.bake state client "Manual baking !" ) )

  let forge_template ~key_name ~counter ~branch ~fee ~src =
    let fee_mutez = fee *. 1_000_000. |> Int.of_float in
    Fmt.str
      {json|
// This is a template of an operation to be forged-and-injected
// Comments start with `//`
{
  "branch": "%s",
  "contents": [
    {  // Basic transaction:
      "kind": "transaction",
      "source": "%s",  // This is already the %s's PKH
      "destination": "tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F",
      "amount": "1",
      "fee": "%d",
      "counter": "%d",  // The counter was fetched from the RPC (not mempool yet)
      "gas_limit": "127",
      "storage_limit": "277"
    },
    {  // Key revelation
      "kind": "reveal",
      "source": "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb", // This is `alice`.
      "fee": "1257", "counter": "3",
      "gas_limit": "10000", "storage_limit": "0",
      "public_key": "edpkvGfYw3LyB1UcCahKQk4rF2tvbMUk8GFiTuMjL75uGXrpvKXhjn"
    },
    {  // Contract origination
      "kind": "origination",
      "source": "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb", "fee": "1240",
      "counter": "4", "gas_limit": "10744", "storage_limit": "309",
      "balance": "1000000",
      "script":
          { "code":
              [ { "prim": "parameter", "args": [ { "prim": "nat" } ] },
                { "prim": "storage", "args": [ { "prim": "nat" } ] },
                { "prim": "code",
                  "args": [ [ { "prim": "FAILWITH" } ] ] } ],
            "storage": { "int": "0" } }
    }
  ]
}
|json}
      branch src key_name fee_mutez counter

  let forge_and_inject_piece_of_json state ~clients =
    Console.Prompt.unit_and_loop
      ~description:
        Fmt.(
          str "Manually create an operation to inject (with %s)."
            (client_list_in_help_messages clients))
      ["forge-and-inject"; "fi"]
      (fun sexps ->
        let client =
          let open Base.Sexp in
          match sexps with
          | [] -> List.nth_exn clients 0
          | [Atom s] -> List.nth_exn clients (Int.of_string s)
          | _ -> Fmt.kstr failwith "Wrong command line: %a" pp (List sexps)
        in
        protect_with_keyed_client "manual-forge" ~client ~f:(fun () ->
            Traffic_generation.branch state client
            >>= fun branch ->
            Tezos_client.get_account state ~client:client.client
              ~name:client.key_name
            >>= function
            | Some acct -> (
                let src = Tezos_protocol.Account.pubkey_hash acct in
                Tezos_client.rpc state ~client:client.client `Get
                  ~path:
                    (Fmt.str
                       "/chains/main/blocks/head/context/contracts/%s/counter"
                       src )
                >>= fun counter_json ->
                let counter =
                  (Jqo.get_string counter_json |> Int.of_string) + 1 in
                let json_template =
                  forge_template ~key_name:client.key_name ~src ~counter ~fee:3.
                    ~branch in
                let tmp = Caml.Filename.temp_file "flextesa-forge" ".json" in
                System.write_file state tmp ~content:json_template
                >>= fun () ->
                System.editor state
                >>= fun editor ->
                Fmt.kstr (System.command state) "%s %s" editor tmp
                >>= function
                | true ->
                    System.read_file state tmp
                    >>= fun content ->
                    let cleaned_up =
                      String.split_lines content
                      |> List.map ~f:(fun line ->
                             match String.substr_index line ~pattern:"//" with
                             | None -> line
                             | Some idx -> String.sub line ~pos:0 ~len:idx )
                      |> String.concat ~sep:"\n" in
                    let json = Ezjsonm.value_from_string cleaned_up in
                    Tezos_client.Keyed.forge_and_inject state client ~json
                    >>= fun res_json ->
                    Console.sayf state
                      Fmt.(
                        fun ppf () ->
                          pf ppf "Forge and inject returned:@ %a" More_fmt.json
                            res_json)
                | false ->
                    Fmt.failwith "Editor ('%s') failed to edit %S" editor tmp )
            | None ->
                Fmt.kstr failwith "Cannot retrieve account for %S"
                  client.key_name ) )

  let generate_and_import_keys state client names =
    Console.say state
      EF.(
        desc
          (af "Importing keys for these signers:")
          (list (List.map names ~f:(fun name -> desc (haf "%s" name) (af "")))))
    >>= fun () ->
    List.fold names ~init:(return ()) ~f:(fun previous_m s ->
        previous_m
        >>= fun () ->
        let kp = Tezos_protocol.Account.of_name s in
        Tezos_client.import_secret_key state client
          ~name:(Tezos_protocol.Account.name kp)
          ~key:(Tezos_protocol.Account.private_key kp) )

  let generate_traffic_command state ~clients ~nodes =
    Console.Prompt.unit_and_loop
      ~description:
        Fmt.(
          str "Generate traffic from a client (%s); try `gen help`."
            ( match clients with
            | [] -> "NO CLIENT, this is just wrong"
            | [one] -> one.Tezos_client.Keyed.client.id
            | m ->
                str "use option (client ..) with one of %s"
                  ( List.mapi m ~f:(fun ith one ->
                        str "%d: %s" ith one.Tezos_client.Keyed.client.id )
                  |> String.concat ~sep:", " ) ))
      ["generate"; "gen"]
      (fun sexps ->
        let client =
          let open Sexp in
          match
            List.find_map sexps ~f:(function
              | List [Atom "client"; Atom s] ->
                  Some (List.nth_exn clients (Int.of_string s))
              | _ -> None )
          with
          | None -> List.nth_exn clients 0
          | Some c -> c
          | exception _ ->
              Fmt.kstr failwith "Wrong command line: %a" pp (List sexps) in
        match sexps with
        | Atom "help" :: __ ->
            Console.sayf state
              More_fmt.(
                let cmd ppf name desc options =
                  cut ppf () ;
                  vertical_box ~indent:2 ppf (fun ppf ->
                      pf ppf "* Command `gen %s ...`:" name ;
                      cut ppf () ;
                      wrapping_box ppf (fun ppf -> desc ppf ()) ;
                      cut ppf () ;
                      Sexp_options.pp_options options ppf () ) in
                fun ppf () ->
                  pf ppf "Generating traffic:" ;
                  cmd ppf "batch"
                    (const text "Make a batch operation (simple transfers).")
                    [counter_option; size_option; fee_option] ;
                  cmd ppf "multisig-batch"
                    (const text
                       "Make a batch operation (multi-sig transacitons)." )
                    [ counter_option; contract_repeat_option; num_signers_option
                    ; size_option; fee_option ] ;
                  cmd ppf "dsl"
                    (const text
                       "Invoke commands using the dsl syntax. Example: \n\
                       \  dsl (repeat :times 5 (random-choice \n\
                       \  ((multisig-batch :size 5 :contract-repeat 3 \
                        :num-signers 4)\n\
                       \  (batch :size 10))))" )
                    [repeat_all_option; random_choice_option] ;
                  cmd ppf "endorsement"
                    (const text "Make an endorsement for a given level")
                    [level_option])
        | Atom "endorsement" :: more_args ->
            protect_with_keyed_client "forge-and-inject" ~client ~f:(fun () ->
                Traffic_generation.branch state client
                >>= fun branch ->
                Sexp_options.get level_option more_args
                  ~f:Sexp_options.get_int_exn ~default:(fun () -> return 42)
                >>= fun level ->
                let json = Traffic_generation.Forge.endorsement ~branch level in
                Tezos_client.Keyed.forge_and_inject state client ~json
                >>= fun json_result ->
                Console.sayf state More_fmt.(fun ppf () -> json ppf json_result) )
        | Atom "batch" :: more_args ->
            get_batch_args state ~client all_opts more_args
            >>= fun ba ->
            run_actions state ~client ~nodes ~actions:[ba] ~rep_counter:1
        | Atom "multisig-batch" :: more_args ->
            get_multisig_args state ~client all_opts more_args
            >>= fun ma ->
            run_actions state ~client ~nodes ~actions:[ma] ~rep_counter:1
        | Atom "dsl" :: dsl_sexp ->
            Traffic_generation.Dsl.process_dsl state ~client ~nodes all_opts
              (Sexp.List dsl_sexp)
        | other ->
            Fmt.kstr failwith "Wrong command line: %a" Sexp.pp (List other) )

  let all_defaults state ~nodes =
    let default_port = (List.hd_exn nodes).Tezos_node.rpc_port in
    [ du_sh_root state; processes state; show_connections state nodes
    ; curl_level state ~default_port; balances state ~default_port
    ; curl_metadata state ~default_port; curl_baking_rights state ~default_port
    ; mempool state ~default_port; better_call_dev state ~default_port
    ; all_levels state ~nodes; show_process state; kill_all state ]
end

module Interactivity = struct
  type t = [`Full | `None | `On_error | `At_end]

  let is_interactive (state : < test_interactivity: t ; .. >) =
    Poly.equal state#test_interactivity `Full

  let pause_on_error state =
    match state#test_interactivity with
    | `Full | `On_error | `At_end -> true
    | `None -> false

  let pause_on_success state =
    match state#test_interactivity with
    | `Full | `At_end -> true
    | `None | `On_error -> false

  let cli_term ?(default : t = `None) () =
    let open Cmdliner in
    Term.(
      pure (fun interactive pause_end pause_error ->
          match (interactive, pause_end, pause_error) with
          | true, _, _ -> `Full
          | false, true, _ -> `At_end
          | false, false, true -> `On_error
          | false, false, false -> `None )
      $ Arg.(
          value
          & opt bool
              ( match default with
              | `None | `On_error | `At_end -> false
              | `Full -> true )
          & info ["interactive"] ~doc:"Add all pauses with command prompts.")
      $ Arg.(
          value
          & opt bool
              ( match default with
              | `None | `On_error -> false
              | `At_end | `Full -> true )
          & info ["pause-at-end"]
              ~doc:"Add a pause with a command prompt at the end of the test.")
      $ Arg.(
          value
          & opt bool
              ( match default with
              | `None -> false
              | `At_end | `Full | `On_error -> true )
          & info ["pause-on-error"]
              ~doc:
                "Add a pause with a command prompt at the end of the test, \
                 only in case of test failure."))
end

module Pauser = struct
  type t =
    { mutable extra_commands: Console.Prompt.item list
    ; default_end: [`Sleep of float] }

  let make ?(default_end = `Sleep 0.5) extra_commands =
    {extra_commands; default_end}

  let commands state = state#pauser.extra_commands
  let default_end state = state#pauser.default_end
  let add_commands state cl = state#pauser.extra_commands <- commands state @ cl

  let generic state ?(force = false) msgs =
    let do_pause = Interactivity.is_interactive state || force in
    Console.say state
      EF.(
        desc
          (if do_pause then haf "Pause" else haf "Not pausing")
          (list ~param:{default_list with space_before_separator= false} msgs))
    >>= fun () ->
    if do_pause then Console.Prompt.(command state ~commands:(commands state))
    else return ()

  let run_test state f ~pp_error () =
    let finish () =
      Console.say state EF.(af "Killing all processes.")
      >>= fun () ->
      Running_processes.kill_all state
      >>= fun () ->
      Console.say state EF.(af "Waiting for processes to all die.")
      >>= fun () -> Running_processes.wait_all state in
    Caml.Sys.catch_break false ;
    let cond = Lwt_condition.create () in
    let catch_signals () =
      Lwt_unix.on_signal Caml.Sys.sigint (fun i ->
          Caml.Printf.eprintf
            "\nReceived signal SIGINT (%d), type `q` to quit prompts.\n\n%!" i ;
          Lwt_condition.broadcast cond `Sig_int )
      |> ignore ;
      Lwt_unix.on_signal Caml.Sys.sigterm (fun i ->
          Caml.Printf.eprintf
            "\nReceived signal SIGTERM (%d), type `q` to quit prompts.\n\n%!" i ;
          Lwt_condition.broadcast cond `Sig_term )
      |> ignore in
    let wait_on_signals () =
      System_error.catch Lwt_condition.wait cond
      >>= fun sig_name -> return (`Woken_up_by_signal sig_name) in
    Dbg.e
      EF.(wf "Running test %s on %s" state#application_name (Paths.root state)) ;
    let wrap_result f = f () >>= fun o -> return (`Successful_procedure o) in
    let run_with_signal_catching ~name procedure =
      Lwt.(
        pick
          [ ( wrap_result (fun () -> procedure ())
            >>= fun res ->
            match res.Attached_result.result with
            | Ok _ ->
                Dbg.e EF.(wf "Procedure %s ended: ok" name) ;
                return res
            | Error (`System_error (_, System_error.Exception Lwt.Canceled)) ->
                Dbg.e EF.(wf "Procedure %s was cancelled" name) ;
                System.sleep 2.
                >>= fun _ ->
                Dbg.e EF.(wf "Procedure %s slept 2." name) ;
                return res
            | Error e ->
                Dbg.e EF.(wf "Procedure %s ended with error %a" name pp_error e) ;
                return res )
          ; ( wait_on_signals ()
            >>= fun res ->
            Dbg.e EF.(wf "Signal-wait %S go woken up" name) ;
            return res ) ]) in
    let last_pause_status = ref `Not_done in
    let rec protect ~name procedure =
      catch_signals () ;
      Dbg.e EF.(wf "protecting %S" name) ;
      let last_pause () =
        protect ~name:"Last-pause" (fun () ->
            generic state ~force:true
              EF.
                [ haf
                    "Last pause before the application will Kill 'Em All and \
                     Quit." ]
            >>= fun () ->
            last_pause_status := `Done ;
            return () ) in
      Asynchronous_result.bind_all
        ( try
            Dbg.e EF.(wf "protecting: %S in-try" name) ;
            run_with_signal_catching ~name procedure
          with e ->
            System_error.fail_fatalf
              ~attach:[("protected", `String_value name)]
              "protecting %S: ocaml-exn: %a" name Exn.pp e )
        ~f:(fun result ->
          match result.result with
          | Ok (`Successful_procedure o) -> return (`Was_ok o)
          | Ok (`Woken_up_by_signal `Sig_term) ->
              Console.say state
                EF.(
                  desc (shout "Received SIGTERM")
                    (wf
                       "Will not pause because it's the wrong thing to do; \
                        killing everything and quitting." ))
              >>= fun () -> return `Quit_with_error
          | Ok (`Woken_up_by_signal `Sig_int) ->
              Console.say state
                EF.(
                  desc (shout "Received SIGINT")
                    (wf
                       "Please the command `q` (a.k.a. `quit`) for quitting \
                        prompts." ))
              >>= fun () -> return `Error_that_can_be_interactive
          | Error (`System_error (`Fatal, System_error.Exception End_of_file))
            ->
              Console.say state
                EF.(
                  desc
                    (shout "Received End-of-File (Ctrl-D?)")
                    (wf
                       "Cannot pause because interactivity broken; killing \
                        everything and quitting." ))
              >>= fun () -> return `Quit_with_error
          | Error _ ->
              Console.say state
                EF.(
                  desc
                    (shout "An error happened")
                    (custom (fun ppf ->
                         Attached_result.pp ~pp_error ppf result ) ))
              >>= fun () -> return `Error_that_can_be_interactive )
      >>= fun todo_next ->
      match todo_next with
      | `Was_ok ()
        when Interactivity.pause_on_success state
             && Poly.equal !last_pause_status `Not_done ->
          last_pause ()
      | `Was_ok () -> (
          finish ()
          >>= fun () ->
          match default_end state with
          | `Sleep n ->
              Console.say state EF.(wf "Test done, sleeping %.02f seconds" n)
              >>= fun () -> System.sleep n )
      | `Error_that_can_be_interactive when Interactivity.pause_on_error state
        ->
          last_pause ()
      | `Error_that_can_be_interactive | `Quit_with_error ->
          finish () >>= fun () -> Asynchronous_result.die 4 in
    protect f ~name:"Main-function"
    >>= fun () ->
    Dbg.e EF.(wf "Finiching Interactive_test.run_test") ;
    return ()
end
