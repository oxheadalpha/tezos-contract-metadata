open Internal_pervasives

(** Helpers to generate traffic in sandboxes. *)

val branch :
     < application_name: string
     ; console: Console.t
     ; env_config: Environment_configuration.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; .. >
  -> Tezos_client.Keyed.t
  -> ( string
     , [> Process_result.Error.t
       | `System_error of [`Fatal] * System_error.static ] )
     Asynchronous_result.t

val is_baking : < test_baking: bool ; .. > -> bool

module Michelson : sig
  val prepare_origination_of_id_script :
       ?delegate:string
    -> ?push_drops:int
    -> ?amount:string
    -> < application_name: string ; .. >
    -> name:string
    -> from:string
    -> protocol_kind:Tezos_protocol.Protocol_kind.t
    -> parameter:string
    -> init_storage:string
    -> (string list, [> System_error.t]) Asynchronous_result.t
end

module Forge : sig
  val batch_transfer :
       ?protocol_kind:Tezos_protocol.Protocol_kind.t
    -> ?counter:int
    -> ?dst:(string * int) list
    -> src:string
    -> fee:float
    -> branch:string
    -> int
    -> Ezjsonm.value

  val endorsement :
       ?protocol_kind:Tezos_protocol.Protocol_kind.t
    -> branch:string
    -> int
    -> Ezjsonm.value
end

val get_chain_id :
     < application_name: string
     ; console: Console.t
     ; env_config: Environment_configuration.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; .. >
  -> Tezos_client.Keyed.t
  -> (string, [> Process_result.Error.t | System_error.t]) Asynchronous_result.t

module Multisig : sig
  val deploy_multisig :
       ?protocol_kind:Tezos_protocol.Protocol_kind.t
    -> ?counter:int
    -> int
    -> branch:string
    -> signers:string list
    -> src:string
    -> fee:float
    -> balance:int
    -> Ezjsonm.value

  val hash_multisig_data :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; env_config: Environment_configuration.t
       ; test_baking: bool
       ; .. >
    -> Tezos_client.t
    -> int
    -> chain_id:string
    -> contract_addr:string
    -> dest:string
    -> ( string
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val sign_multisig :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; env_config: Environment_configuration.t
       ; test_baking: bool
       ; .. >
    -> Tezos_client.Keyed.t
    -> contract_addr:string
    -> amt:int
    -> to_acct:string
    -> signer_name:string
    -> ( string
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val transfer_from_multisig :
       ?protocol_kind:Tezos_protocol.Protocol_kind.t
    -> ?counter:int
    -> float
    -> branch:string
    -> src:string
    -> destination:string
    -> contract:string
    -> amount:int
    -> signatures:string list (* -> signature:string -> burn_cap:float *)
    -> Ezjsonm.value

  val deploy_and_transfer :
       ?initial_counter_override:int
    -> < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; env_config: Environment_configuration.t
       ; test_baking: bool
       ; .. >
    -> Tezos_client.Keyed.t
    -> Tezos_node.t list
    -> src:string
    -> fee:float
    -> num_signers:int
    -> outer_repeat:int
    -> contract_repeat:int
    -> ( unit
       , [> Process_result.Error.t
         | System_error.t
         | `Waiting_for of string * [`Time_out] ] )
       Asynchronous_result.t

  val get_signer_names : string list -> int -> string list
end

module Commands : sig
  val cmdline_fail :
       ( 'a
       , Formatter.t
       , unit
       , ('b, [> `Command_line of string]) Asynchronous_result.t )
       format4
    -> 'a

  module Sexp_options : sig
    type t = {name: string; placeholders: string list; description: string}
    type option = t

    val make_option : string -> ?placeholders:string list -> string -> t
    val pp_options : t list -> unit Fmt.t

    val get :
         t
      -> Sexp.t list
      -> default:
           (   unit
            -> ('a, ([> `Command_line of string] as 'b)) Asynchronous_result.t
           )
      -> f:(Sexp.t list -> 'a)
      -> ('a, 'b) Asynchronous_result.t

    val get_int_exn : Sexp.t list -> Int.t
    val get_float_exn : Sexp.t list -> Float.t

    val port_number_doc :
         < application_name: string
         ; console: Console.t
         ; paths: Paths.t
         ; runner: Running_processes.State.t
         ; .. >
      -> default_port:int
      -> t

    val port_number :
         < application_name: string
         ; console: Console.t
         ; env_config: Environment_configuration.t
         ; paths: Paths.t
         ; env_config: Environment_configuration.t
         ; runner: Running_processes.State.t
         ; .. >
      -> default_port:int
      -> Sexp.t list
      -> (int, [> `Command_line of string]) Asynchronous_result.t

    val fmt_sexp : Sexp.t -> string
    val fmt_sexps : Sexp.t list -> string
  end

  val protect_with_keyed_client :
       string
    -> client:Tezos_client.Keyed.t
    -> f:
         (   unit
          -> ( 'a
             , [< `Command_line of string
               | Process_result.Error.t
               | System_error.t
               | `Waiting_for of string * [< `Time_out] ] )
             Asynchronous_result.t )
    -> ( 'a
       , [> `Command_line of string | Process_result.Error.t] )
       Asynchronous_result.t

  val counter_option : Sexp_options.option
  val size_option : Sexp_options.option
  val fee_option : Sexp_options.option
  val level_option : Sexp_options.option
  val contract_repeat_option : Sexp_options.option
  val num_signers_option : Sexp_options.option
  val repeat_all_option : Sexp_options.option
  val random_choice_option : Sexp_options.option

  type all_options =
    { counter_option: Sexp_options.option
    ; size_option: Sexp_options.option
    ; fee_option: Sexp_options.option
    ; num_signers_option: Sexp_options.option
    ; contract_repeat_option: Sexp_options.option }

  val all_opts : all_options

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

  val history_file_path : < paths: Paths.t ; .. > -> string
  val get_timestamp : unit -> string

  val init_cmd_history :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> (unit, [> System_error.t]) Attached_result.t Lwt.t

  val get_cmd_history :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> (string, [> System_error.t]) Attached_result.t Lwt.t

  val add_cmd_to_history :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> new_cmd:string
    -> start_time:string
    -> end_time:string
    -> (unit, [> System_error.t]) Attached_result.t Lwt.t

  val get_batch_args :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:Tezos_client.Keyed.t
    -> all_options
    -> Base.Sexp.t list
    -> ([> action], [> `Command_line of string]) Asynchronous_result.t

  val get_multisig_args :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:Tezos_client.Keyed.t
    -> all_options
    -> Base.Sexp.t list
    -> ( [> `Multisig_action of multisig_action]
       , [> `Command_line of string | Process_result.Error.t] )
       Asynchronous_result.t

  val to_action :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:Tezos_client.Keyed.t
    -> all_options
    -> Base.Sexp.t
    -> ( [> action]
       , [> `Command_line of string
         | Process_result.Error.t
         | `System_error of [`Fatal] * System_error.static ] )
       Asynchronous_result.t

  val process_repeat_action : Base.Sexp.t -> int * Base.Sexp.t
  val process_random_choice : Base.Sexp.t -> bool * Base.Sexp.t

  val process_action_cmds :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:Tezos_client.Keyed.t
    -> all_options
    -> Base.Sexp.t
    -> random_choice:bool
    -> ( action list
       , [> `Command_line of string
         | Process_result.Error.t
         | `System_error of [`Fatal] * System_error.static ] )
       Asynchronous_result.t

  val process_gen_batch :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:Tezos_client.Keyed.t
    -> batch_action
    -> ( unit
       , [> `Command_line of string | System_error.t | Process_result.Error.t]
       )
       Asynchronous_result.t

  val process_gen_multi_sig :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; test_baking: bool
       ; .. >
    -> client:Tezos_client.Keyed.t
    -> nodes:Tezos_node.t list
    -> multisig_action
    -> ( unit
       , [> `Command_line of string | System_error.t] )
       Asynchronous_result.t

  val run_actions :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; test_baking: bool
       ; .. >
    -> client:Tezos_client.Keyed.t
    -> nodes:Tezos_node.t list
    -> actions:[< action] list
    -> rep_counter:int
    -> ( unit
       , [> `Command_line of string | System_error.t | Process_result.Error.t]
       )
       Asynchronous_result.t
end

module Random : sig
  val run :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; test_baking: bool
       ; .. >
    -> protocol:Tezos_protocol.t
    -> nodes:Tezos_node.t list
    -> clients:Tezos_client.Keyed.t list
    -> until_level:int
    -> [> `Any]
    -> ( unit
       , [> `Command_line of string | System_error.t | Process_result.Error.t]
       )
       Asynchronous_result.t
end

module Dsl : sig
  val process_dsl :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; test_baking: bool
       ; .. >
    -> client:Tezos_client.Keyed.t
    -> nodes:Tezos_node.t list
    -> Commands.all_options
    -> Base.Sexp.t
    -> ( unit
       , [> `Command_line of string | Process_result.Error.t] )
       Asynchronous_result.t

  val run :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; test_baking: bool
       ; .. >
    -> nodes:Tezos_node.t list
    -> clients:Tezos_client.Keyed.t list
    -> Sexp.t
    -> ( unit
       , [> Process_result.Error.t | System_error.t | `Command_line of string]
       )
       Asynchronous_result.t
end
