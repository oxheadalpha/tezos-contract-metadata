open Flextesa.Internal_pervasives

module Small_utilities = struct
  let key_of_name_command () =
    let open Cmdliner in
    let open Term in
    ( ( pure (fun n ->
            let open Flextesa.Tezos_protocol.Account in
            let account = of_name n in
            Caml.Printf.printf "%s,%s,%s,%s\n%!" (name account) (pubkey account)
              (pubkey_hash account) (private_key account) )
      $ Arg.(
          required
            (pos 0 (some string) None
               (info [] ~docv:"NAME" ~doc:"String to generate the data from.") ))
      )
    , info "key-of-name"
        ~doc:"Make an unencrypted key-pair deterministically from a string."
        ~man:
          [ `P
              "`flextesa key-of-name hello-world` generates a key-pair of the \
               `unencrypted:..` kind and outputs it as a 4 values separated by \
               commas: `name,pub-key,pub-key-hash,private-uri` (hence \
               compatible with the `--add-bootstrap-account` option of some of \
               the test scenarios)." ] )

  let netstat_ports ~pp_error () =
    let open Cmdliner in
    let open Term in
    Flextesa.Test_command_line.Run_command.make ~pp_error
      ( pure (fun state ->
            Flextesa.
              ( state
              , fun () ->
                  Helpers.Netstat.used_listening_ports state
                  >>= fun ports ->
                  let to_display =
                    List.map ports ~f:(fun (p, _) -> p)
                    |> List.sort ~compare:Int.compare in
                  Console.sayf state
                    Fmt.(
                      hvbox ~indent:2 (fun ppf () ->
                          box words ppf "Netstat listening ports:" ;
                          sp ppf () ;
                          box
                            (list
                               ~sep:(fun ppf () -> string ppf "," ; sp ppf ())
                               (fun ppf p -> fmt "%d" ppf p) )
                            ppf to_display )) ) )
      $ Flextesa.Test_command_line.cli_state ~disable_interactivity:true
          ~name:"netstat-ports" () )
      (info "netstat-listening-ports"
         ~doc:"Like `netstat -nut | awk something-something` but glorified." )

  let vanity_chain_id ~pp_error () =
    let open Cmdliner in
    let open Term in
    Flextesa.Test_command_line.Run_command.make ~pp_error
      ( pure (fun state stop_at_first machine_readable seed attempts pattern ->
            Flextesa.
              ( state
              , fun () ->
                  let sayf f =
                    match machine_readable with
                    | Some _ -> return ()
                    | None -> Console.sayf state f in
                  sayf Fmt.(fun ppf () -> pf ppf "Looking for %S" pattern)
                  >>= fun () ->
                  let rec loop count res =
                    if count >= attempts || (stop_at_first && Poly.(res <> []))
                    then res
                    else
                      let seed = seed ^ Int.to_string count in
                      let open Tezai_base58_digest.Identifier in
                      let block_hash = Block_hash.(hash_string seed |> encode) in
                      let chain_id = Chain_id.of_base58_block_hash block_hash in
                      let acc =
                        if String.is_suffix ~suffix:pattern chain_id then
                          (seed, block_hash, chain_id) :: res
                        else res in
                      loop (count + 1) acc in
                  let res = loop 0 [] in
                  ( match machine_readable with
                  | None -> ()
                  | Some fmt ->
                      let sep =
                        match fmt with
                        | `Csv -> fun () -> Fmt.pr ","
                        | `Tsv -> fun () -> Fmt.pr "\t" in
                      List.iter res ~f:(fun (seed, bh, ci) ->
                          Fmt.pr "%S" seed ;
                          sep () ;
                          Fmt.pr "%s" bh ;
                          sep () ;
                          Fmt.pr "%s" ci ;
                          Fmt.pr "\n%!" ) ) ;
                  sayf
                    More_fmt.(
                      fun ppf () ->
                        vertical_box ~indent:2 ppf (fun ppf ->
                            pf ppf "Results:" ;
                            match res with
                            | [] -> pf ppf " EMPTY!"
                            | more ->
                                List.iter more ~f:(fun (seed, bh, ci) ->
                                    cut ppf () ;
                                    wrapping_box ~indent:2 ppf (fun ppf ->
                                        pf ppf
                                          "* Seed: %S@ → block: %S@ → \
                                           chain-id: %S"
                                          seed bh ci ) ) )) ) )
      $ Flextesa.Test_command_line.cli_state ~disable_interactivity:true
          ~name:"vanity-chain-id" ()
      $ Arg.(value (flag (info ["first"] ~doc:"Stop at the first result.")))
      $ Arg.(
          value
            (let cases = [("csv", `Csv); ("tsv", `Tsv)] in
             opt
               (some (enum cases))
               None
               (info ["machine-readable"]
                  ~docv:(List.map cases ~f:fst |> String.concat ~sep:"|")
                  ~doc:"Print the results on stdout in a parsing friendly way." )
            ))
      $ Arg.(
          value
            (opt string "flextesa"
               (info ["seed"] ~doc:"The constant seed to use.") ))
      $ Arg.(
          value
            (opt int 100_000 (info ["attempts"] ~doc:"The number of attempts.")))
      $ Arg.(required (pos 0 (some string) None (info [] ~docv:"PATTERN"))) )
      (info "vanity-chain-id"
         ~doc:
           "Find a block hash to set as Genesis which makes-up a given \
            chain-id suffix." )

  let all ~pp_error () =
    [ key_of_name_command (); netstat_ports ~pp_error ()
    ; vanity_chain_id ~pp_error () ]
end

let () =
  let open Cmdliner in
  let pp_error = Flextesa.Test_command_line.Common_errors.pp in
  let help = Term.(ret (pure (`Help (`Auto, None))), info "flextesa") in
  Term.exit
    (Term.eval_choice
       (help : unit Term.t * _)
       ( Small_utilities.all ~pp_error ()
       @ [Flextesa.Interactive_mini_network.cmd ()] ) )
