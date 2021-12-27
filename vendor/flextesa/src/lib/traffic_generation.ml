open Internal_pervasives

let branch state client =
  Tezos_client.rpc state ~client:client.Tezos_client.Keyed.client `Get
    ~path:"/chains/main/blocks/head/hash"
  >>= fun br -> return (Jqo.get_string br)

let is_baking (state : < test_baking: bool ; .. >) =
  Poly.equal state#test_baking true

module Michelson = struct
  let prepare_origination_of_id_script ?delegate ?(push_drops = 0)
      ?(amount = "2") state ~name ~from ~protocol_kind ~parameter ~init_storage
      =
    let id_script parameter =
      Fmt.str
        "parameter %s;\n\
         storage %s;\n\
         code\n\
        \  {\n\
        \    %s\n\
        \    { CAR; NIL operation; PAIR }\n\
        \  };\n"
        parameter parameter
        ( match push_drops with
        | 0 -> "# No push-drops"
        | n ->
            Fmt.str "# %d push-drop%s\n    %s" n
              (if n > 1 then "s" else "")
              ( List.init push_drops ~f:(fun ith ->
                    Fmt.str "{ PUSH string %S ; DROP } ;"
                      (Fmt.str
                         "push-dropping %d adds stupid bytes to the contract"
                         ith ) )
              |> String.concat ~sep:"\n    " ) ) in
    let tmp = Caml.Filename.temp_file "little-id-script" ".tz" in
    System.write_file state tmp ~content:(id_script parameter)
    >>= fun () ->
    Dbg.e EF.(wf "id_script %s: %s" parameter tmp) ;
    let origination =
      let opt = Option.value_map ~default:[] in
      ["--wait"; "none"; "originate"; "contract"; name]
      @ ( if Tezos_protocol.Protocol_kind.wants_contract_manager protocol_kind
        then ["for"; from]
        else [] )
      @ [ "transferring"; amount; "from"; from; "running"; tmp; "--init"
        ; init_storage; "--force"; "--burn-cap"; "300000000000"
        ; (* ; "--fee-cap" ; "20000000000000" *) "--gas-limit"
        ; "1000000000000000"; "--storage-limit"; "20000000000000"
        ; "--verbose-signing" ]
      @ opt delegate ~f:(fun s -> (* Baby & Aths *) ["--delegate"; s]) in
    return origination
end

module Forge = struct
  let batch_transfer
      ?(protocol_kind : Tezos_protocol.Protocol_kind.t = `Babylon)
      ?(counter = 0)
      ?(dst =
        [("tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F", Random.int_incl 1 1000)]) ~src
      ~fee ~branch n : Ezjsonm.value =
    let open Ezjsonm in
    ignore protocol_kind ;
    dict
      [ ("branch", `String branch)
      ; ( "contents"
        , `A
            (List.map (List.range 0 n) ~f:(fun i ->
                 let dest, amount = List.nth_exn dst (i % List.length dst) in
                 `O
                   [ ("kind", `String "transaction"); ("source", `String src)
                   ; ("destination", `String dest)
                   ; ("amount", `String (Int.to_string amount))
                   ; ( "fee"
                     , `String (Int.to_string (Float.to_int (fee *. 1000000.)))
                     ); ("counter", `String (Int.to_string (counter + i)))
                   ; ("gas_limit", `String (Int.to_string 127))
                   ; ("storage_limit", `String (Int.to_string 277)) ] ) ) ) ]

  let endorsement ?(protocol_kind : Tezos_protocol.Protocol_kind.t = `Babylon)
      ~branch level : Ezjsonm.value =
    let open Ezjsonm in
    ignore protocol_kind ;
    dict
      [ ("branch", `String branch)
      ; ( "contents"
        , `A [`O [("kind", `String "endorsement"); ("level", int level)]] ) ]
end

let get_chain_id state (client : Tezos_client.Keyed.t) =
  Tezos_client.rpc state ~client:client.client `Get
    ~path:"/chains/main/chain_id"
  >>= fun chain_id_json ->
  try return (Jqo.get_string chain_id_json)
  with e ->
    System_error.fail_fatalf "Exception getting the chain id: %a" Exn.pp e

module Multisig = struct
  let signer_names_base =
    [ "Alice"; "Bob"; "Charlie"; "David"; "Elsa"; "Frank"; "Gail"; "Harry"
    ; "Ivan"; "Jane"; "Iris"; "Jackie"; "Linda"; "Mary"; "Nemo"; "Opal"; "Paul"
    ; "Quincy"; "Rhonda"; "Steve"; "Theodore"; "Uma"; "Venus"; "Wimpy"; "Xaviar"
    ; "Yuri"; "Zed" ]

  let get_signer_names signers n =
    let k = List.length signers in
    let rec append = function 0 -> signers | x -> signers @ append (x - 1) in
    let suffix = function 0 -> "" | n -> "-" ^ Int.to_string (n + 1) in
    let fold_f ((xs, i) : string list * int) (x : string) : string list * int =
      let fst = List.cons (x ^ suffix (i / k)) xs in
      (fst, i + 1) in
    let big_list = List.take (append (n / k)) n in
    let result, _ = List.fold big_list ~init:([], 0) ~f:fold_f in
    List.rev result

  (* The multisig contract script written by Arthur Breitman
      https://github.com/murbard/smart-contracts/blob/master/multisig/michelson/multisig.tz *)
  (* Updated to take the chain id into account *)
  let multisig_script_string =
    "[{\"prim\":\"parameter\",\"args\":[{\"prim\":\"pair\",\"args\": \
     [{\"prim\":\"pair\",\"args\":[{\"prim\":\"nat\",\"annots\": \
     [\"%counter\"]},{\"prim\":\"or\",\"args\":[{\"prim\":\"pair\", \
     \"args\":[{\"prim\":\"mutez\",\"annots\":[\"%amount\"]},{\"prim\": \
     \"contract\",\"args\":[{\"prim\":\"unit\"}],\"annots\":[\"%dest\"]}], \
     \"annots\":[\":transfer\"]},{\"prim\":\"or\",\"args\":[{\"prim\": \
     \"option\",\"args\":[{\"prim\":\"key_hash\"}],\"annots\": \
     [\"%delegate\"]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"nat\", \
     \"annots\":[\"%threshold\"]},{\"prim\":\"list\",\"args\":[{\"prim\": \
     \"key\"}],\"annots\":[\"%keys\"]}],\"annots\":[\"%change_keys\"]}]}], \
     \"annots\":[\":action\"]}],\"annots\":[\":payload\"]},{\"prim\": \
     \"list\",\"args\":[{\"prim\":\"option\",\"args\":[{\"prim\": \
     \"signature\"}]}],\"annots\":[\"%sigs\"]}]}]},{\"prim\": \
     \"storage\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\": \
     \"nat\",\"annots\":[\"%stored_counter\"]},{\"prim\":\"pair\", \
     \"args\":[{\"prim\":\"nat\",\"annots\":[\"%threshold\"]}, \
     {\"prim\":\"list\",\"args\":[{\"prim\":\"key\"}],\"annots\": \
     [\"%keys\"]}]}]}]},{\"prim\":\"code\",\"args\":[[[[{\"prim\": \
     \"DUP\"},{\"prim\":\"CAR\"},{\"prim\":\"DIP\",\"args\": \
     [[{\"prim\":\"CDR\"}]]}]],{\"prim\":\"SWAP\"},{\"prim\":\"DUP\"}, \
     {\"prim\":\"DIP\",\"args\":[[{\"prim\":\"SWAP\"}]]},{\"prim\":\"DIP\", \
     \"args\":[[[[{\"prim\":\"DUP\"},{\"prim\":\"CAR\"},{\"prim\":\"DIP\", \
     \"args\":[[{\"prim\":\"CDR\"}]]}]],{\"prim\":\"DUP\"},{\"prim\": \
     \"SELF\"},{\"prim\":\"ADDRESS\"},{\"prim\":\"CHAIN_ID\"}, \
     {\"prim\":\"PAIR\"},{\"prim\":\"PAIR\"},{\"prim\":\"PACK\"}, \
     {\"prim\":\"DIP\",\"args\":[[[[{\"prim\":\"DUP\"},{\"prim\":\"CAR\", \
     \"annots\":[\"@counter\"]},{\"prim\":\"DIP\",\"args\":[[{\"prim\": \
     \"CDR\"}]]}]],{\"prim\":\"DIP\",\"args\":[[{\"prim\":\"SWAP\"}]]}]]}, \
     {\"prim\":\"SWAP\"}]]},[[{\"prim\":\"DUP\"},{\"prim\":\"CAR\", \
     \"annots\":[\"@stored_counter\"]},{\"prim\":\"DIP\",\"args\": \
     [[{\"prim\":\"CDR\"}]]}]],{\"prim\":\"DIP\",\"args\":[[{\"prim\": \
     \"SWAP\"}]]},[[{\"prim\":\"COMPARE\"},{\"prim\":\"EQ\"}],{\"prim\": \
     \"IF\",\"args\":[[],[[{\"prim\":\"UNIT\"},{\"prim\":\"FAILWITH\"}]]]}], \
     {\"prim\":\"DIP\",\"args\":[[{\"prim\":\"SWAP\"}]]},[[{\"prim\":\"DUP\"}, \
     {\"prim\":\"CAR\",\"annots\":[\"@threshold\"]},{\"prim\":\"DIP\", \
     \"args\":[[{\"prim\":\"CDR\",\"annots\":[\"@keys\"]}]]}]],{\"prim\": \
     \"DIP\",\"args\":[[{\"prim\":\"PUSH\",\"args\":[{\"prim\":\"nat\"}, \
     {\"int\":\"0\"}],\"annots\":[\"@valid\"]},{\"prim\":\"SWAP\"}, \
     {\"prim\":\"ITER\",\"args\":[[{\"prim\":\"DIP\",\"args\":[[{\"prim\": \
     \"SWAP\"}]]},{\"prim\":\"SWAP\"},{\"prim\":\"IF_CONS\",\"args\": \
     [[[{\"prim\":\"IF_NONE\",\"args\":[[{\"prim\":\"SWAP\"},{\"prim\": \
     \"DROP\"}],[{\"prim\":\"SWAP\"},{\"prim\":\"DIP\",\"args\":[[{\"prim\": \
     \"SWAP\"},{\"prim\":\"DIP\",\"args\":[{\"int\":\"2\"},[[{\"prim\": \
     \"DIP\",\"args\":[[{\"prim\":\"DUP\"}]]},{\"prim\":\"SWAP\"}]]]}, \
     {\"prim\":\"CHECK_SIGNATURE\"},[{\"prim\":\"IF\",\"args\":[[], \
     [[{\"prim\":\"UNIT\"},{\"prim\":\"FAILWITH\"}]]]}],{\"prim\":\"PUSH\" \
     ,\"args\":[{\"prim\":\"nat\"},{\"int\":\"1\"}]},{\"prim\":\"ADD\" \
     ,\"annots\":[\"@valid\"]}]]}]]}]],[[{\"prim\":\"UNIT\"},{\"prim\": \
     \"FAILWITH\"}]]]},{\"prim\":\"SWAP\"}]]}]]},[[{\"prim\":\"COMPARE\"}, \
     {\"prim\":\"LE\"}],{\"prim\":\"IF\",\"args\":[[],[[{\"prim\":\"UNIT\"}, \
     {\"prim\":\"FAILWITH\"}]]]}],{\"prim\":\"DROP\"},{\"prim\":\"DROP\"}, \
     {\"prim\":\"DIP\",\"args\":[[[[{\"prim\":\"DUP\"},{\"prim\":\"CAR\"}, \
     {\"prim\":\"DIP\",\"args\":[[{\"prim\":\"CDR\"}]]}]],{\"prim\":\"PUSH\", \
     \"args\":[{\"prim\":\"nat\"},{\"int\":\"1\"}]},{\"prim\":\"ADD\", \
     \"annots\":[\"@new_counter\"]},{\"prim\":\"PAIR\"}]]},{\"prim\":\"NIL\", \
     \"args\":[{\"prim\":\"operation\"}]},{\"prim\":\"SWAP\"},{\"prim\": \
     \"IF_LEFT\",\"args\":[[[[{\"prim\":\"DUP\"},{\"prim\":\"CAR\"}, \
     {\"prim\":\"DIP\",\"args\":[[{\"prim\":\"CDR\"}]]}]],{\"prim\":\"UNIT\"}, \
     {\"prim\":\"TRANSFER_TOKENS\"},{\"prim\":\"CONS\"}],[{\"prim\": \
     \"IF_LEFT\",\"args\":[[{\"prim\":\"SET_DELEGATE\"},{\"prim\":\"CONS\"}], \
     [{\"prim\":\"DIP\",\"args\":[[{\"prim\":\"SWAP\"},{\"prim\":\"CAR\"}]]}, \
     {\"prim\":\"SWAP\"},{\"prim\":\"PAIR\"},{\"prim\":\"SWAP\"}]]}]]}, \
     {\"prim\":\"PAIR\"}]]}]"

  let multisig_script_json (counter : int) (sig_threshold : int)
      (signers : string list) : Ezjsonm.value =
    let foldf acc signer = ("{ \"string\":" ^ "\"" ^ signer ^ "\" },") ^ acc in
    let pks =
      List.map
        ~f:(fun n ->
          Tezos_protocol.Account.pubkey (Tezos_protocol.Account.of_name n) )
        signers in
    let folded =
      String.rstrip
        ~drop:(fun ch -> Char.equal ch ',')
        (List.fold pks ~init:"" ~f:foldf) in
    let signer_list = "[" ^ folded ^ "]" in
    let storage_prefix_str =
      Printf.sprintf
        "{\"prim\": \"Pair\", \"args\": [{\"int\": \"%d\"}, {\"prim\": \
         \"Pair\", \"args\": [{\"int\": \"%d\"},"
        counter sig_threshold in
    let script_str =
      "{ \"code\": " ^ multisig_script_string ^ ", " ^ "\"storage\": "
      ^ storage_prefix_str ^ signer_list ^ "] } ] } }" in
    Ezjsonm.value_from_string script_str

  let multisig_param_template =
    Caml.(
      "{\"entrypoint\":\"default\",\"value\":{\"prim\":\"Pair\",\"args\":"
      ^^ "[{\"prim\":\"Pair\",\"args\":[{\"int\":\"%d\"},{\"prim\":\"Left\","
      ^^ "\"args\":[{\"prim\":\"Pair\",\"args\":[{\"int\":\"%d\"},{\"string\":"
      ^^ "\"%s\"}]}]}]},[%s]]}}")

  let sig_template =
    Caml.("{\"prim\":\"Some\",\"args\":" ^^ "[{\"string\":\"%s\"}]},")

  let multisig_params_json sigs counter amount msig : Ezjsonm.value =
    let the_signatures =
      let sigs_rev = List.rev sigs in
      let folded =
        List.fold ~init:""
          ~f:(fun acc s -> Printf.sprintf sig_template s ^ acc)
          sigs_rev in
      String.rstrip ~drop:(fun ch -> Char.equal ch ',') folded in
    let multisig_params =
      sprintf multisig_param_template counter amount msig the_signatures in
    Ezjsonm.value_from_string multisig_params

  let deploy_multisig
      ?(protocol_kind : Tezos_protocol.Protocol_kind.t = `Babylon)
      ?(counter = 0) sig_threshold ~branch ~signers ~src ~fee ~balance =
    let open Ezjsonm in
    ignore protocol_kind ;
    dict
      [ ("branch", `String branch)
      ; ( "contents"
        , `A
            [ `O
                [ ("kind", `String "origination"); ("source", `String src)
                ; ( "fee"
                  , `String (Int.to_string (Float.to_int (fee *. 1000000.))) )
                ; ("counter", `String (Int.to_string counter))
                ; ("gas_limit", `String (Int.to_string 1040000))
                ; ("storage_limit", `String (Int.to_string 60000))
                ; ("balance", `String (Int.to_string balance))
                ; ("script", multisig_script_json counter sig_threshold signers)
                ] ] ) ]

  let hash_multisig_data state client mutez ~chain_id ~contract_addr ~dest =
    let data_type =
      "(pair (pair address chain_id) (pair int (or (pair mutez (contract \
       unit)) unit)))" in
    Tezos_client.multisig_storage_counter state client contract_addr
    >>= fun contract_counter ->
    let data_to_hash =
      sprintf "(Pair (Pair \"%s\" \"%s\") (Pair %d (Left (Pair %d \"%s\"))))"
        contract_addr chain_id contract_counter mutez dest in
    let gas = 1040000 in
    Tezos_client.hash_data state client ~data_to_hash ~data_type ~gas

  let sign_multisig state client ~contract_addr ~amt ~to_acct ~signer_name =
    get_chain_id state client
    >>= fun chain_id ->
    hash_multisig_data state client.client amt ~chain_id ~contract_addr
      ~dest:to_acct
    >>= fun bytes ->
    Tezos_client.Keyed.sign_bytes state client ~bytes ~key_name:signer_name
    >>= fun ret ->
    let cleaned =
      match String.chop_prefix ret ~prefix:"Signature: " with
      | Some s -> s
      | None -> ret in
    return cleaned

  let transfer_from_multisig
      ?(protocol_kind : Tezos_protocol.Protocol_kind.t = `Babylon)
      ?(counter = 0) fee ~branch ~src ~destination ~contract ~amount
      ~signatures (* ~signature ~burn_cap *) =
    let open Ezjsonm in
    ignore protocol_kind ;
    dict
      [ ("branch", `String branch)
      ; ( "contents"
        , `A
            [ `O
                [ ("kind", `String "transaction"); ("source", `String src)
                ; ( "fee"
                  , `String (Int.to_string (Float.to_int (fee *. 1000000.))) )
                ; ("counter", `String (Int.to_string counter))
                ; ("gas_limit", `String (Int.to_string 1040000))
                ; ("storage_limit", `String (Int.to_string 60000))
                ; ("amount", `String (Int.to_string amount))
                ; ("destination", `String destination)
                ; ( "parameters"
                  , multisig_params_json signatures counter amount contract ) ]
            ] ) ]

  let deploy_and_transfer ?initial_counter_override state
      (client : Tezos_client.Keyed.t) (nodes : Tezos_node.t list) ~src ~fee
      ~num_signers ~outer_repeat ~contract_repeat =
    Tezos_client.Keyed.update_counter
      ?current_counter_override:initial_counter_override state client
      "deploy_and_transfer"
    >>= fun origination_ctr ->
    (*loop through 'size' *)
    Loop.n_times_fold outer_repeat origination_ctr (fun n origination_counter ->
        let more_signers = num_signers + (n - 1) in
        let signer_names_plus =
          get_signer_names signer_names_base more_signers in
        (* generate and import keys *)
        let signer_names = List.take signer_names_plus more_signers in
        Helpers.import_keys_from_seeds state client.client ~seeds:signer_names
        >>= fun _ ->
        let s = List.hd_exn signer_names in
        let kp = Tezos_protocol.Account.of_name s in
        let _destination = Tezos_protocol.Account.pubkey_hash kp in
        (* deploy the multisig contract *)
        branch state client
        >>= fun the_branch ->
        let json =
          deploy_multisig ~counter:origination_counter more_signers
            ~branch:the_branch ~signers:signer_names ~src ~fee:(fee *. 10.0)
            ~balance:(Random.int 10000 + 1) in
        Tezos_client.Keyed.forge_and_inject state client ~json
        >>= fun deploy_result ->
        Console.sayf state More_fmt.(fun ppf () -> json ppf deploy_result)
        >>= fun () ->
        ( if is_baking state then
          Test_scenario.Queries.wait_for_bake state ~nodes
        else Tezos_client.Keyed.bake state client "Multisig deploy_and_transfer"
        )
        >>= fun () ->
        let _ = Tezos_client.Keyed.operations_from_chain state client in
        Tezos_client.Keyed.get_contract_id state client
          (Jqo.to_string_hum deploy_result)
        >>= fun contract_addr ->
        (* for each signer, sign the contract *)
        let to_acct =
          Tezos_protocol.Account.pubkey_hash
            (Tezos_protocol.Account.of_name "Bob") in
        let m_sigs =
          List.map [List.hd_exn signer_names] ~f:(fun s ->
              sign_multisig state client ~contract_addr ~amt:100 ~to_acct
                ~signer_name:s ) in
        Asynchronous_result.all m_sigs
        >>= fun signatures ->
        (* submit the fully signed multisig contract *)
        Loop.n_times contract_repeat (fun k ->
            Tezos_client.Keyed.update_counter state client
              (sprintf "Inner transfer_from_multisig loop with k:%d" k)
            >>= fun new_counter ->
            let xfer_json =
              transfer_from_multisig ~counter:new_counter fee ~branch:the_branch
                ~src ~destination:contract_addr ~contract:contract_addr
                ~amount:100 ~signatures in
            Tezos_client.Keyed.forge_and_inject state client ~json:xfer_json
            >>= fun xfer_res ->
            Console.say state
              EF.(
                desc
                  (haf "Multi-sig contract generation")
                  (af "Multisig transaction (%n) results: %s" k
                     (Jqo.to_string_hum xfer_res) )) )
        >>= fun () ->
        Tezos_client.Keyed.update_counter state client
          "Bottom of outer multisig loop"
        >>= fun new_ctr -> return new_ctr )
end

module Commands = struct
  let cmdline_fail fmt = Fmt.kstr (fun s -> fail (`Command_line s)) fmt

  module Sexp_options = struct
    type t = {name: string; placeholders: string list; description: string}
    type option = t

    let make_option name ?(placeholders = []) description =
      {name; placeholders; description}

    let pp_options l ppf () =
      let open More_fmt in
      vertical_box ~indent:2 ppf (fun ppf ->
          pf ppf "Options:" ;
          List.iter l ~f:(fun {name; placeholders; description} ->
              cut ppf () ;
              wrapping_box ~indent:2 ppf (fun ppf ->
                  let opt_ex ppf () =
                    prompt ppf (fun ppf ->
                        pf ppf "%s%s" name
                          ( if Poly.equal placeholders [] then ""
                          else
                            List.map ~f:(str " %s") placeholders
                            |> String.concat ~sep:"" ) ) in
                  pf ppf "* %a  %a" opt_ex () text description ) ) )

    let find opt sexps f =
      List.find_map sexps
        ~f:
          Sexp.(
            function
            | List (Atom a :: more)
              when String.equal a opt.name
                   && Int.(List.length more = List.length opt.placeholders) ->
                Some (f more)
            | _ -> None)

    let find_new opt sexps g =
      let sub_list =
        List.drop_while sexps ~f:(function
          | Sexp.Atom a when String.equal a opt.name -> false
          | _ -> true ) in
      match sub_list with
      | Sexp.Atom _ :: Sexp.Atom o :: _ -> Some (g [Sexp.Atom o])
      | _ -> None

    let get opt sexps ~default ~f =
      match find_new opt sexps f with
      | Some n -> return n
      | None -> (
        match find opt sexps f with Some n -> return n | None -> default () )
      | exception e -> cmdline_fail "Getting option %s: %a" opt.name Exn.pp e

    let get_int_exn = function
      | Sexp.[Atom a] -> (
        try Int.of_string a with _ -> Fmt.failwith "%S is not an integer" a )
      | other -> Fmt.failwith "wrong structure: %a" Sexp.pp (Sexp.List other)

    let get_opt opt sexps ~f =
      match find_new opt sexps f with
      | Some n -> return (Some n)
      | None -> return (find opt sexps f)
      | exception e -> cmdline_fail "Getting option %s: %a" opt.name Exn.pp e

    let get_float_exn = function
      | Sexp.[Atom a] -> (
        try Float.of_string a with _ -> Fmt.failwith "%S is not a float" a )
      | other -> Fmt.failwith "wrong structure: %a" Sexp.pp (Sexp.List other)

    let port_number_doc _ ~default_port =
      make_option "port" ~placeholders:["<int>"]
        Fmt.(str "Use port number <int> instead of %d (default)." default_port)

    let port_number _state ~default_port sexps =
      match
        List.find_map sexps
          ~f:
            Base.Sexp.(
              function
              | List [Atom "port"; Atom p] -> (
                try Some (`Ok (Int.of_string p)) with _ -> Some (`Not_an_int p)
                )
              | List (Atom "port" :: _ as other) -> Some (`Wrong_option other)
              | _other -> None)
      with
      | None -> return default_port
      | Some (`Ok p) -> return p
      | Some ((`Not_an_int _ | `Wrong_option _) as other) ->
          let problem =
            match other with
            | `Not_an_int s -> Fmt.str "This is not an integer: %S." s
            | `Wrong_option s ->
                Fmt.str "Usage is (port <int>), too many arguments here: %s."
                  Base.Sexp.(to_string_hum (List s)) in
          fail (`Command_line "Error parsing (port ...) option")
            ~attach:[("Problem", `Text problem)]

    let rec fmt_sexp sexp =
      Base.Sexp.(
        match sexp with
        | Atom a -> "Atom:" ^ "\"" ^ a ^ "\""
        | Sexp.List xs ->
            let prefix = "Sexp.List [" ^ fmt_sexps xs in
            String.drop_suffix prefix 2 ^ "]")

    and fmt_sexps xs =
      match xs with [] -> "" | x :: xs -> fmt_sexp x ^ "; " ^ fmt_sexps xs
  end

  let protect_with_keyed_client msg ~client ~f =
    let msg =
      Fmt.str "Command-line %s with client %s (account: %s)" msg
        client.Tezos_client.Keyed.client.id client.Tezos_client.Keyed.key_name
    in
    Asynchronous_result.bind_on_error (f ()) ~f:(fun ~result:_ -> function
      | #Process_result.Error.t as e ->
          cmdline_fail "%s -> Error: %a" msg Process_result.Error.pp e
      | #System_error.t as e ->
          cmdline_fail "%s -> Error: %a" msg System_error.pp e
      | `Waiting_for (msg, `Time_out) ->
          cmdline_fail "WAITING-FOR “%s”: Time-out" msg
      | `Command_line _ as e -> fail e )

  let counter_option =
    Sexp_options.make_option ":counter" ~placeholders:["<int>"]
      "The counter to provide (get it from the node by default)."

  let size_option =
    Sexp_options.make_option ":size" ~placeholders:["<int>"]
      "The batch size (default: 10)."

  let fee_option =
    Sexp_options.make_option ":fee" ~placeholders:["<float-tz>"]
      "The fee per operation (default: 0.02)."

  let level_option =
    Sexp_options.make_option ":level" ~placeholders:["<int>"] "The level."

  let contract_repeat_option =
    Sexp_options.make_option ":operation-repeat" ~placeholders:["<int>"]
      "The number of repeated calls to execute the fully-signed multi-sig \
       contract (default: 1)."

  let num_signers_option =
    Sexp_options.make_option ":num-signers" ~placeholders:["<int>"]
      "The number of signers required for the multi-sig contract (default: 3)."

  let repeat_all_option =
    Sexp_options.make_option "repeat"
      ~placeholders:[":times"; "<int>"; "<list of commands>"]
      "The number of times to repeat any dsl commands that follow (default: 1)."

  let random_choice_option =
    Sexp_options.make_option "random-choice"
      ~placeholders:["<list of commands>"]
      "Randomly chose a command from a list of dsl commands."

  type all_options =
    { counter_option: Sexp_options.option
    ; size_option: Sexp_options.option
    ; fee_option: Sexp_options.option
    ; num_signers_option: Sexp_options.option
    ; contract_repeat_option: Sexp_options.option }

  type batch_action =
    {src: string; initial_counter_override: int option; size: int; fee: float}
  [@@deriving sexp]

  type multisig_action =
    { src: string
    ; initial_counter_override: int option
    ; fee: float
    ; num_signers: int
    ; outer_repeat: int
    ; contract_repeat: int }
  [@@deriving sexp]

  type action =
    [`Batch_action of batch_action | `Multisig_action of multisig_action]
  [@@deriving sexp]

  let all_opts : all_options =
    { counter_option
    ; size_option
    ; fee_option
    ; num_signers_option
    ; contract_repeat_option }

  let history_file_path (state : < paths: Paths.t ; .. >) =
    Paths.root state ^ "/traffic-generation-history.txt"

  let get_timestamp () =
    let date = Date.Local.now () |> Date.to_rfc3339 in
    sprintf "[%s]" date

  let init_cmd_history state =
    System.write_file state (history_file_path state) ~content:""

  let get_cmd_history state = System.read_file state (history_file_path state)

  let add_cmd_to_history state ~new_cmd ~start_time ~end_time =
    get_cmd_history state
    >>= fun prev ->
    let content =
      prev ^ start_time ^ "\n" ^ new_cmd ^ "\n" ^ end_time ^ "\n\n" in
    System.write_file state (history_file_path state) ~content

  let address_of_account acctOpt err_str =
    match acctOpt with
    | Some a -> Tezos_protocol.Account.pubkey_hash a
    | None -> err_str

  let get_batch_args state ~client opts more_args =
    protect_with_keyed_client "generate batch" ~client ~f:(fun () ->
        Tezos_client.get_account state ~client:client.client
          ~name:client.key_name
        >>= fun acct ->
        let src = address_of_account acct "<Unable to parse account>" in
        Sexp_options.get_opt opts.counter_option more_args
          ~f:Sexp_options.get_int_exn
        >>= fun initial_counter_override ->
        Sexp_options.get opts.size_option more_args ~f:Sexp_options.get_int_exn
          ~default:(fun () -> return 10)
        >>= fun size ->
        Sexp_options.get opts.fee_option more_args ~f:Sexp_options.get_float_exn
          ~default:(fun () -> return 10.02)
        >>= fun fee ->
        return (`Batch_action {src; initial_counter_override; size; fee}) )

  let get_multisig_args state ~client opts (more_args : Sexp.t list) =
    protect_with_keyed_client "generate batch" ~client ~f:(fun () ->
        Tezos_client.get_account state ~client:client.client
          ~name:client.key_name
        >>= fun acct ->
        let src = address_of_account acct "<Unable to parse account>" in
        Sexp_options.get_opt opts.counter_option more_args
          ~f:Sexp_options.get_int_exn
        >>= fun initial_counter_override ->
        Sexp_options.get opts.size_option more_args ~f:Sexp_options.get_int_exn
          ~default:(fun () -> return 10)
        >>= fun outer_repeat ->
        Sexp_options.get opts.fee_option more_args ~f:Sexp_options.get_float_exn
          ~default:(fun () -> return 10.02)
        >>= fun fee ->
        Sexp_options.get opts.contract_repeat_option more_args
          ~f:Sexp_options.get_int_exn ~default:(fun () -> return 1)
        >>= fun contract_repeat ->
        Sexp_options.get opts.num_signers_option more_args
          ~f:Sexp_options.get_int_exn ~default:(fun () -> return 3)
        >>= fun num_signers ->
        return
          (`Multisig_action
            { src
            ; initial_counter_override
            ; fee
            ; num_signers
            ; outer_repeat
            ; contract_repeat } ) )

  let to_action state ~(client : Tezos_client.Keyed.t) opts sexp =
    match sexp with
    | Sexp.List (Atom "batch" :: more_args) ->
        get_batch_args state ~client opts more_args
    | Sexp.List (Atom "multisig-batch" :: more_args) ->
        get_multisig_args state ~client opts more_args
    | Sexp.List (Atom a :: _) ->
        Fmt.kstr failwith "to_action - unexpected atom inside list: %s" a
    | Sexp.List z ->
        Fmt.kstr failwith "to_action - unexpected list. %s"
          (Sexp.to_string (List z))
    | Sexp.Atom b -> Fmt.kstr failwith "to_action - unexpected atom. %s" b

  let process_repeat_action sexp =
    match sexp with
    | Sexp.List (y :: _) -> (
      match y with
      | Sexp.List (Atom "repeat" :: Atom ":times" :: Atom n :: more_args) ->
          let c = Int.of_string n in
          let count = if c <= 0 then 1 else c in
          (count, Sexp.List more_args)
      | _ -> (1, sexp) )
    | _ -> (1, sexp)

  let process_random_choice sexp =
    match sexp with
    | Sexp.List (y :: _) -> (
      match y with
      | Sexp.List (Atom "random-choice" :: more_args) ->
          (true, Sexp.List more_args)
      | _ -> (false, sexp) )
    | other ->
        Fmt.kstr failwith
          "process_random_choice - expecting a list but got - from pp: %a, \
           from fmt_sexp: %s, from Sexp.to_string: %s"
          Sexp.pp other
          (Sexp_options.fmt_sexp other)
          (Sexp.to_string other)

  let process_action_cmds state ~client opts sexp ~random_choice =
    let rec loop (actions : action list) (exps : Base.Sexp.t list) =
      match exps with
      | [x] -> to_action state ~client opts x >>= fun a -> return (a :: actions)
      | x :: xs ->
          to_action state ~client opts x >>= fun a -> loop (a :: actions) xs
      | other ->
          System_error.fail_fatalf "process_action_cmds - something weird: %a"
            Sexp.pp (List other) in
    let exp_list =
      match sexp with
      | Sexp.List (y :: _) -> (
        match y with
        | Sexp.List z -> z
        | other ->
            Fmt.kstr failwith
              "process_action_cmds - inner fail - expecting a list within a \
               list but got: %a"
              Sexp.pp other )
      | other ->
          Fmt.kstr failwith
            "process_action_cmds - outer fail - expecting a list within a list \
             but got: %a"
            Sexp.pp other in
    loop [] exp_list
    >>= fun action_list ->
    if not random_choice then return action_list
    else
      let len = List.length action_list in
      if len = 0 then return []
      else
        let rand = Base.Random.int len in
        let one_action = (List.to_array action_list).(rand) in
        return [one_action]

  let process_gen_batch state ~client (act : batch_action) =
    let start_time = get_timestamp () in
    protect_with_keyed_client "generate batch" ~client ~f:(fun () ->
        Helpers.Timing.duration
          (fun aFee ->
            Tezos_client.Keyed.update_counter
              ?current_counter_override:act.initial_counter_override state
              client "in process_gen_batch"
            >>= fun new_counter ->
            branch state client
            >>= fun the_branch ->
            let json =
              Forge.batch_transfer ~counter:new_counter ~src:act.src ~fee:aFee
                ~branch:the_branch act.size in
            Tezos_client.Keyed.forge_and_inject state client ~json
            >>= fun json_result ->
            Console.sayf state More_fmt.(fun ppf () -> json ppf json_result) )
          act.fee
        >>= fun ((), sec) ->
        Console.say state EF.(desc (haf "Execution time:") (af " %fs\n%!" sec)) )
    >>= fun () ->
    let batch_str = "batch: " ^ Sexp.to_string_hum (sexp_of_batch_action act) in
    add_cmd_to_history state ~new_cmd:batch_str ~start_time
      ~end_time:(get_timestamp ())

  let process_gen_multi_sig state ~client ~nodes act =
    let start_time = get_timestamp () in
    protect_with_keyed_client "generate multisig" ~client ~f:(fun () ->
        Helpers.Timing.duration
          (fun () ->
            Multisig.deploy_and_transfer
              ?initial_counter_override:act.initial_counter_override state
              client nodes ~src:act.src ~fee:act.fee
              ~num_signers:act.num_signers ~outer_repeat:act.outer_repeat
              ~contract_repeat:act.contract_repeat )
          ()
        >>= fun ((), sec) ->
        Console.say state EF.(desc (haf "Execution time:") (af " %fs\n%!" sec)) )
    >>= fun () ->
    let multisig_str =
      "multisig: " ^ Sexp.to_string_hum (sexp_of_multisig_action act) in
    add_cmd_to_history state ~new_cmd:multisig_str ~start_time
      ~end_time:(get_timestamp ())

  let run_actions state ~client ~nodes ~actions ~rep_counter =
    Loop.n_times rep_counter (fun _ ->
        List_sequential.iter actions ~f:(fun a ->
            match a with
            | `Batch_action ba -> process_gen_batch state ~client ba
            | `Multisig_action ma ->
                process_gen_multi_sig state ~client ~nodes ma ) )
end

module Random = struct
  let run state ~protocol ~nodes ~clients ~until_level kind =
    assert (Poly.equal kind `Any) ;
    let tbb =
      protocol.Tezos_protocol.time_between_blocks |> List.hd
      |> Option.value ~default:10 in
    let info fmt =
      Fmt.kstr
        (fun s ->
          Console.sayf state Fmt.(fun ppf () -> pf ppf "Randomizer: %s" s) )
        fmt in
    let from = "bootacc-0" in
    let keyed_client = List.hd_exn clients in
    let client = keyed_client.Tezos_client.Keyed.client in
    let pp_success ppf = function
      | true -> Fmt.pf ppf "Success"
      | false -> Fmt.pf ppf "Failure" in
    let valid_contracts = ref [] in
    let rec loop iteration =
      let client_cmd name l =
        Tezos_client.client_cmd ~verbose:false state ~client
          ~id_prefix:(Fmt.str "randomizer-%04d-%s" iteration name)
          l in
      let continue_or_not () =
        Test_scenario.Queries.all_levels state ~nodes
        >>= fun all_levels ->
        if
          List.for_all all_levels ~f:(function
            | _, `Level l when l >= until_level -> true
            | _ -> false )
        then info "Max-level reached: %d" until_level
        else loop (iteration + 1) in
      List.random_element
        [`Sleep; `Add_contract; `Call_contract; `Multisig_contract]
      |> function
      | Some `Sleep ->
          let secs =
            Float.(Random.float_range (of_int tbb * 0.3) (of_int tbb * 1.5))
          in
          info "Sleeping %.2f seconds." secs
          >>= fun () ->
          let start_time = Commands.get_timestamp () in
          System.sleep secs
          >>= fun () ->
          let sleep_str = sprintf "Sleep for %.2f seconds" secs in
          Commands.add_cmd_to_history state ~new_cmd:sleep_str ~start_time
            ~end_time:(Commands.get_timestamp ())
          >>= fun () -> continue_or_not ()
      | Some `Call_contract ->
          let start_time = Commands.get_timestamp () in
          ( match List.random_element !valid_contracts with
          | None -> info "No valid contracts to call."
          | Some (name, params) ->
              Tezos_client.get_account state ~client ~name:from
              >>= fun acct ->
              let show_from =
                Commands.address_of_account acct "Unable to parse account>"
              in
              Tezos_client.show_known_contract state client ~name
              >>= fun show_to ->
              client_cmd
                (Fmt.str "transfer-%s" name)
                ["transfer"; "1"; "from"; from; "to"; name; "--arg"; params]
              >>= fun (success, _) ->
              info "Called %s(%s): %a" name params pp_success success
              >>= fun () ->
              let call_str =
                sprintf "Call_contract: (from: %s (%s)) (to: %s (Hash: %s))"
                  from show_from name show_to in
              Commands.add_cmd_to_history state ~new_cmd:call_str ~start_time
                ~end_time:(Commands.get_timestamp ()) )
          >>= fun () -> continue_or_not ()
      | Some `Add_contract ->
          let start_time = Commands.get_timestamp () in
          let name = Fmt.str "contract-%d" iteration in
          let push_drops = Random.int 100 in
          let parameter, init_storage =
            match List.random_element [`Unit; `String] with
            | Some `String ->
                ( "string"
                , Fmt.str "%S"
                    (String.init
                       (Random.int 42 + 1)
                       ~f:(fun _ -> Random.int 20 + 40 |> Char.of_int_exn) ) )
            | _ -> ("unit", "Unit") in
          Michelson.prepare_origination_of_id_script state ~name ~from
            ~protocol_kind:protocol.Tezos_protocol.kind ~parameter ~init_storage
            ~push_drops
          >>= fun origination ->
          client_cmd (Fmt.str "originate-%s" name) origination
          >>= fun (success, _) ->
          ( if success then (
            valid_contracts := (name, init_storage) :: !valid_contracts ;
            info "Origination of `%s` (%s : %s): `%a`." name init_storage
              parameter pp_success success
            >>= fun () -> Tezos_client.show_known_contract state client ~name )
          else
            info "Failure during Origination of `%s` (%s : %s): `%a`." name
              init_storage parameter pp_success success
            >>= fun () -> return "<error>" )
          >>= fun contract_pk_hash ->
          let add_str = sprintf "Add_contract: (name: %s)" contract_pk_hash in
          Commands.add_cmd_to_history state ~new_cmd:add_str ~start_time
            ~end_time:(Commands.get_timestamp ())
          >>= fun () -> continue_or_not ()
      | Some `Multisig_contract ->
          Commands.protect_with_keyed_client "generate batch"
            ~client:keyed_client ~f:(fun () ->
              Tezos_client.get_account state ~client ~name:keyed_client.key_name
              >>= fun acct ->
              let src =
                Commands.address_of_account acct "<Unable to parse account>"
              in
              let initial_counter_override = None in
              let fee = Random.float_range 10.1 10.3 in
              let num_signers = Random.int 5 + 1 in
              let outer_repeat = Random.int 5 + 1 in
              let contract_repeat = Random.int 5 + 1 in
              let act =
                ( { src
                  ; initial_counter_override
                  ; fee
                  ; num_signers
                  ; outer_repeat
                  ; contract_repeat }
                  : Commands.multisig_action ) in
              Commands.process_gen_multi_sig state ~client:keyed_client ~nodes
                act )
          >>= fun () -> continue_or_not ()
      | None -> continue_or_not () in
    loop 0
end

module Dsl = struct
  let process_dsl state ~(client : Tezos_client.Keyed.t) ~nodes opts sexp =
    Commands.protect_with_keyed_client "process_dsl" ~client ~f:(fun () ->
        let n, sexp2 = Commands.process_repeat_action sexp in
        let b, sexp3 = Commands.process_random_choice sexp2 in
        Commands.process_action_cmds state ~client opts sexp3 ~random_choice:b
        >>= fun actions ->
        Commands.run_actions state ~client ~nodes ~actions ~rep_counter:n )

  let run state ~nodes ~clients dsl_command =
    let client = List.hd_exn clients in
    process_dsl state ~client ~nodes Commands.all_opts dsl_command
end
