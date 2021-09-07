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

module License = struct
  type t = {name: string; details: string option}

  let pp ppf {name; details} =
    Fmt.pf ppf "%s%a" name
      Fmt.(option ~none:(const string "") (sp ++ parens string))
      details

  let encoding =
    let open Json_encoding in
    conv
      (function {name; details} -> (name, details))
      (fun (name, details) -> {name; details})
      (obj2
         (req "name" string
            ~description:
              "A mnemonic name for the license, see also the License-name case." )
         (opt "details" string
            ~description:
              "Paragraphs of free text, with punctuation and proper language." ) )
end

module Michelson_blob = struct
  open Tezos_micheline

  type t = Micheline of string Micheline.canonical

  let pp ppf (Micheline m) =
    Fmt.pf ppf "'%a'" Micheline_printer.print_expr
      (Micheline_printer.printable Base.Fn.id m)

  let encoding =
    let open Json_encoding in
    conv
      (function Micheline m -> m)
      (fun m -> Micheline m)
      (Data_encoding.Json.convert
         (Micheline.canonical_encoding ~variant:"tzip-16" Data_encoding.string) )
end

module View = struct
  module Implementation = struct
    open Tezos_micheline

    module Michelson_storage = struct
      type t =
        { parameter: Michelson_blob.t option
        ; return_type: Michelson_blob.t
        ; code: Michelson_blob.t
        ; human_annotations: (string * string) list
        ; version: string option }
    end

    module Rest_api_query = struct
      type t =
        { specification_uri: string
        ; base_uri: string option
        ; path: string
        ; meth: Cohttp.Code.meth }
    end

    type t =
      | Michelson_storage of Michelson_storage.t
      | Rest_api_query of Rest_api_query.t

    let michelson_storage ?parameter ~return_type ?(annotations = []) ?version
        code =
      Michelson_storage
        {parameter; return_type; code; human_annotations= annotations; version}

    let rest_api_query ?base_uri ?(meth = `GET) specification_uri path =
      Rest_api_query {specification_uri; base_uri; path; meth}

    let pp ?(with_code = true) ppf impl =
      let open Fmt in
      pf ppf "%a"
        (box ~indent:2
           ( match impl with
           | Michelson_storage
               {parameter; return_type; code; human_annotations; version} ->
               let michelfield field_name =
                 box
                   ( cut ++ const string field_name ++ const string ":" ++ sp
                   ++ Michelson_blob.pp ) in
               vbox ~indent:2
                 ( const string "Michelson-storage:"
                 ++ const
                      (option
                         (cut ++ field ~label:string "Version" Base.Fn.id string) )
                      version
                 ++ const
                      (option ~none:nop (cut ++ michelfield "Parameter"))
                      parameter
                 ++ const (cut ++ michelfield "Return-type") return_type
                 ++ ( if with_code then const (cut ++ michelfield "Code") code
                    else nop )
                 ++ const
                      (fun ppf -> function
                        | [] -> nop ppf ()
                        | annots ->
                            pf ppf "%a"
                              ( cut
                              ++ field ~label:string "Annotations" Base.Fn.id
                                   (vbox
                                      (list ~sep:cut
                                         (box ~indent:2
                                            (pair ~sep:(any " ->@ ") string text) ) ) )
                              )
                              annots )
                      human_annotations )
           | Rest_api_query {specification_uri; base_uri; path; meth} ->
               let string_field f = field ~label:string f Base.Fn.id string in
               vbox ~indent:2
                 ( const string "REST-API-Query:"
                 ++ const
                      (cut ++ string_field "Specification-URI")
                      specification_uri
                 ++ const
                      (option ~none:nop (cut ++ string_field "Base-URI"))
                      base_uri
                 ++ const (cut ++ string_field "Specification-URI") path
                 ++ const
                      ( cut
                      ++ field ~label:string "Path" Cohttp.Code.string_of_method
                           string )
                      meth ) ) )
        ()

    let encoding =
      let open Json_encoding in
      union
        [ case ~title:"michelsonStorageView"
            ~description:
              "An off-chain view using Michelson as a scripting language to \
               interpret the storage of a contract."
            (obj1
               (req "michelsonStorageView"
                  (obj5
                     (opt "parameter" Michelson_blob.encoding
                        ~description:
                          "The Michelson type of the potential external \
                           parameters required by the code of the view." )
                     (req "returnType" Michelson_blob.encoding
                        ~description:
                          "The type of the result of the view, i.e. the value \
                           left on the stack by the code." )
                     (req "code" Michelson_blob.encoding
                        ~description:
                          "The Michelson code expression implementing the view." )
                     (dft "annotations"
                        ~description:
                          "List of objects documenting the annotations used in \
                           the 3 above fields."
                        (list
                           (obj2 (req "name" string) (req "description" string)) )
                        [] )
                     (opt "version" string
                        ~description:
                          "A string representing the version of Michelson that \
                           the view is meant to work with; versions here \
                           should be base58check-encoded protocol hashes." ) ) ) )
            (function
              | Michelson_storage
                  {parameter; return_type; code; human_annotations; version} ->
                  Some (parameter, return_type, code, human_annotations, version)
              | Rest_api_query _ -> None )
            (fun (parameter, return_type, code, human_annotations, version) ->
              Michelson_storage
                {parameter; return_type; code; human_annotations; version} )
        ; case ~title:"restApiQueryView"
            ~description:
              "An off-chain view using a REST API described in a separate \
               OpenAPI specification. The following parameters form a pointer \
               to the localtion in the OpenAPI description."
            (obj1
               (req "restApiQuery"
                  (obj4
                     (req "specificationUri" string
                        ~description:
                          "A URI pointing at the location of the OpenAPI \
                           specification." )
                     (opt "baseUri" string
                        ~description:"The URI-prefix to use to query the API." )
                     (req "path" string
                        ~description:
                          "The path component of the URI to look-up in the \
                           OpenAPI specification." )
                     (dft "method" ~description:"The HTTP method to use."
                        (string_enum
                           [("GET", `GET); ("POST", `POST); ("PUT", `PUT)] )
                        `GET ) ) ) )
            (function
              | Michelson_storage _ -> None
              | Rest_api_query {specification_uri; base_uri; path; meth} ->
                  Some (specification_uri, base_uri, path, meth) )
            (fun (specification_uri, base_uri, path, meth) ->
              Rest_api_query {specification_uri; base_uri; path; meth} ) ]

    module Example = struct
      let build = function
        | 0 ->
            michelson_storage
              ~return_type:
                (Micheline
                   Micheline.(Prim (0, "nat", [], []) |> strip_locations) )
              (Micheline Micheline.(Seq (0, []) |> strip_locations))
        | 1 ->
            rest_api_query ~meth:`GET "https://example.com/v1.json"
              "/get-something"
        | 2 ->
            let mich s =
              let toks, errs = Micheline_parser.tokenize s in
              assert (errs = []) ;
              let node, errs = Micheline_parser.parse_expression toks in
              assert (errs = []) ;
              node in
            let elson e =
              Michelson_blob.Micheline (Micheline.strip_locations e) in
            michelson_storage
              ( mich "{ DUP ; DIP { CDR ; PUSH string \"Huh\" ; FAILWITH } }"
              |> elson )
              ~parameter:(mich "(pair (mutez %amount) (string %name))" |> elson)
              ~return_type:(mich "map string string" |> elson)
              ~version:"PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb"
              ~annotations:
                [ ( "%amount"
                  , "The amount which should mean something in context. It's \
                     in `mutez` which should also mean something more than \
                     lorem ipsum dolor whatever …" )
                ; ("%name", "The name of the thing being queried.") ]
        | _ -> assert false
    end
  end

  type t =
    { name: string
    ; description: string option
    ; implementations: Implementation.t list
    ; is_pure: bool }

  let make ?description ?(is_pure = false) name implementations =
    {name; description; implementations; is_pure}

  let pp ?with_code ppf view =
    let open Fmt in
    pf ppf "@[<v 2>%sView %S:%a@,%a@]"
      (if view.is_pure then "Pure-" else "")
      view.name
      (option ~none:nop
         ( cut
         ++ hovbox ~indent:2
              (const string "Description:" ++ sp ++ box paragraphs) ) )
      view.description
      (vbox (list ~sep:cut (Implementation.pp ?with_code)))
      view.implementations

  let encoding =
    let open Json_encoding in
    conv
      (fun {name; description; implementations; is_pure} ->
        (name, description, implementations, is_pure) )
      (fun (name, description, implementations, is_pure) ->
        {name; description; implementations; is_pure} )
      (obj4 (req "name" string)
         (opt "description" string
            ~description:
              "Plain language documentation of the off-chain view; with \
               punctuation." )
         (req "implementations"
            ~description:"The list of available and equivalent implementations."
            (list Implementation.encoding) )
         (dft "pure" bool false) )

  module Example = struct
    let build = function
      | 0 ->
          make "view0" ~is_pure:true
            [Implementation.Example.build 0; Implementation.Example.build 1]
      | 1 -> make "view-01" [Implementation.Example.build 2]
      | _ -> assert false
  end
end

module Source = struct
  type t = {tools: string list; location: string option}

  let pp ppf {tools; location} =
    let open Fmt in
    pf ppf "@[<2>Tools:@ @[%a@]@]@,Location:@ %a"
      (list ~sep:(any ", ") (quote string))
      tools
      (option ~none:(any "None") string)
      location

  let encoding =
    let open Json_encoding in
    conv
      (fun {tools; location} -> (tools, location))
      (fun (tools, location) -> {tools; location})
      (obj2
         (dft "tools" (list string) [] ~title:"Contract Producing Tools"
            ~description:
              "List of tools/versions used in producing the Michelson." )
         (opt "location" string ~title:"Source Location"
            ~description:"Location (URL) of the source code." ) )
end

module Errors = struct
  module Translation = struct
    type t =
      | Static of
          { error: Michelson_blob.t
          ; expansion: Michelson_blob.t
          ; languages: string list option }
      | Dynamic of {view_name: string; languages: string list option}

    let pp ppf =
      let open Fmt in
      let langs ppf = function
        | None -> pf ppf ""
        | Some more -> pf ppf "@ (langs: %a)" (list ~sep:(any "|") string) more
      in
      function
      | Static {error; expansion; languages} ->
          pf ppf "@[<2>%a -> %a%a@]" Michelson_blob.pp error Michelson_blob.pp
            expansion langs languages
      | Dynamic {view_name; languages} ->
          pf ppf "@[<2>View %S%a@]" view_name langs languages

    let encoding =
      let open Json_encoding in
      union
        [ case ~title:"staticErrorTranslator"
            ~description:
              "A convertor between error codes and expanded messages."
            (obj3
               (req "error" Michelson_blob.encoding)
               (req "expansion" Michelson_blob.encoding)
               (opt "languages" (list string)) )
            (function
              | Static {error; expansion; languages} ->
                  Some (error, expansion, languages)
              | Dynamic _ -> None )
            (fun (error, expansion, languages) ->
              Static {error; expansion; languages} )
        ; case ~title:"dynamicErrorTranslator"
            ~description:
              "An off-chain-view to call to convert error codes to expanded \
               messages."
            (obj2 (req "view" string) (opt "languages" (list string)))
            (function
              | Static _ -> None
              | Dynamic {view_name; languages} -> Some (view_name, languages) )
            (fun (view_name, languages) -> Dynamic {view_name; languages}) ]
  end

  type t = Translation.t list

  let pp ppf t =
    Fmt.pf ppf "@[<2>[%a]@]" Fmt.(list ~sep:(any "; ,") Translation.pp) t

  let encoding =
    let open Json_encoding in
    list Translation.encoding
end

type t =
  { name: string option
  ; description: string option
  ; version: string option
  ; license: License.t option
  ; authors: string list
  ; homepage: string option
  ; source: Source.t option
  ; interfaces: string list
  ; errors: Errors.t option
  ; views: View.t list
  ; unknown: (string * Ezjsonm.value) list }

let make ?name ?description ?version ?license ?(authors = []) ?homepage ?source
    ?(interfaces = []) ?errors ?(extras = []) views =
  { name
  ; description
  ; version
  ; license
  ; authors
  ; homepage
  ; source
  ; interfaces
  ; errors
  ; views
  ; unknown= extras }

let pp_gen ?(shorter = false) ppf t =
  let open Fmt in
  let all =
    let field name conv value =
      const
        (option ~none:nop (cut ++ field ~label:string name Base.Fn.id conv))
        value in
    let optlist = function [] -> None | m -> Some m in
    let { name
        ; description
        ; version
        ; license
        ; authors
        ; homepage
        ; source
        ; interfaces
        ; errors
        ; views
        ; unknown } =
      t
      (* we force ocaml to warn us for missing fields *) in
    vbox ~indent:2
      ( const string "Contract-Metadata-TZIP-16:"
      ++ field "Name" string name
      ++ field "Description" paragraphs description
      ++ field "Version" string version
      ++ field "License" License.pp license
      ++ field "Authors" (list ~sep:comma string) (optlist authors)
      ++ field "Homepage" string homepage
      ++ field "Source" Source.pp source
      ++ field "Interfaces" (list ~sep:comma string) (optlist interfaces)
      ++ field "Errors" Errors.pp errors
      ++ field "Views"
           (list ~sep:cut (View.pp ~with_code:(not shorter)))
           (optlist views)
      ++
      match unknown with
      | [] -> nop
      | more ->
          cut
          ++ vbox ~indent:2
               ( const string "Unknown:" ++ cut
               ++ const lines (Ezjsonm.value_to_string ~minify:false (`O more))
               ) ) in
  pf ppf "%a" all ()

let pp = pp_gen ~shorter:false
let pp_short = pp_gen ~shorter:true

let _reserved_fields =
  [ "name"
  ; "description"
  ; "version"
  ; "license"
  ; "authors"
  ; "homepage"
  ; "source"
  ; "interfaces"
  ; "errors"
  ; "views" ]

let encoding =
  let open Json_encoding in
  let extensible fields (enc : 'a encoding) :
      ('a * (string * Ezjsonm.value) list) encoding =
    (* merge_objs enc (assoc any_ezjson_value) *)
    let schema =
      let o = schema enc in
      let open Json_schema in
      let root = root o in
      match root.kind with
      | Object ob ->
          assert (List.length ob.properties = List.length fields) ;
          let new_root =
            { root with
              kind= Object {ob with additional_properties= Some (element Any)}
            } in
          Json_schema.update new_root o
      | _ -> assert false in
    custom ~schema
      (fun (a, args) ->
        match construct enc a with
        | `O l -> `O (l @ args)
        | _ -> Fmt.failwith "extensible: failed to get an object" )
      (fun json ->
        let retained, unknown =
          match json with
          | `O obj ->
              Base.List.partition_tf obj ~f:(fun (k, _) -> List.mem k fields)
          | other ->
              Fmt.failwith "Wrong json: %s"
                (Ezjsonm.value_to_string ~minify:false other) in
        (destruct enc (`O retained), unknown) ) in
  def "contractMetadataTzip16" ~title:"contractMetadataTzip16"
    ~description:"Smart Contract Metadata Standard (TZIP-16)."
    (conv
       (fun { name
            ; description
            ; version
            ; license
            ; authors
            ; homepage
            ; source
            ; interfaces
            ; errors
            ; views
            ; unknown } ->
         ( ( name
           , description
           , version
           , license
           , authors
           , homepage
           , source
           , interfaces
           , errors
           , views )
         , unknown ) )
       (fun ( ( name
              , description
              , version
              , license
              , authors
              , homepage
              , source
              , interfaces
              , errors
              , views )
            , unknown ) ->
         { name
         ; description
         ; version
         ; license
         ; authors
         ; homepage
         ; source
         ; interfaces
         ; errors
         ; views
         ; unknown } )
       (extensible _reserved_fields
          (obj10
             (opt "name" string
                ~description:"The identification of the contract." )
             (opt "description" string
                ~description:
                  "Natural language description of the contract and/or its \
                   behavior." )
             (opt "version" string
                ~description:"The version of the contract code." )
             (opt "license" License.encoding
                ~description:"The software license of the contract." )
             (dft "authors" ~description:"The list of authors of the contract."
                (list string) [] )
             (opt "homepage" string
                ~description:
                  "A link for humans to follow for documentation, sources, \
                   issues, etc." )
             (opt "source" Source.encoding
                ~description:
                  "Description of how the contract's Michelson was generated." )
             (dft "interfaces"
                ~description:
                  "The list of interfaces the contract claims to implement \
                   (e.g. TZIP-12)."
                (list string) [] )
             (opt "errors" ~description:"Error translators." Errors.encoding)
             (dft "views"
                ~description:
                  "The storage queries, a.k.a. off-chain views provided."
                (list View.encoding) [] ) ) ) )

let of_json s =
  try
    let jsonm = Ezjsonm.value_from_string s in
    let contents = Json_encoding.destruct encoding jsonm in
    Ok contents
  with e -> Error_monad.error_exn e

let to_json c =
  let jsonm = Json_encoding.construct encoding c in
  Ezjsonm.value_to_string ~minify:false jsonm

module Validation = struct
  module Error = struct
    type t =
      | Forbidden_michelson_instruction of {view: string; instruction: string}
      | Michelson_version_not_a_protocol_hash of {view: string; value: string}

    let pp ppf =
      let open Fmt in
      let textf f = kstr (fun s -> (box text) ppf s) f in
      function
      | Forbidden_michelson_instruction {view; instruction} ->
          textf "Forbidden Michelson instruction %S in view %S" instruction view
      | Michelson_version_not_a_protocol_hash {view; value} ->
          textf "Michelson version %S in view %S is not a protocol hash" value
            view
  end

  module Warning = struct
    type t =
      | Wrong_author_format of string
      | Unexpected_whitespace of {field: string; value: string}
      | Self_unaddressed of {view: string; instruction: string option}

    let pp ppf =
      let open Fmt in
      let textf f = kstr (fun s -> (box text) ppf s) f in
      function
      | Wrong_author_format auth ->
          textf "Wrong format for author field: %S" auth
      | Unexpected_whitespace {field; value} ->
          textf "Unexpected whitespace character(s) in field %S = %S" field
            value
      | Self_unaddressed {view; instruction} ->
          textf "SELF instruction not followed by ADDRESS (%s) in view %S"
            (Option.value instruction ~default:"by nothing")
            view
  end

  module Data = struct
    let author_re = lazy Re.Posix.(re "^[^\\<\\>]*<[^ ]+>$" |> compile)

    let forbidden_michelson_instructions =
      [ "AMOUNT"
      ; "CREATE_CONTRACT"
      ; "SENDER"
      ; "SET_DELEGATE"
      ; "SOURCE"
      ; "TRANSFER_TOKENS" ]
  end

  open Data

  let validate ?(protocol_hash_is_valid = fun _ -> true) (metadata : t) =
    let errors = ref [] in
    let warnings = ref [] in
    let error e = errors := e :: !errors in
    let warning e = warnings := e :: !warnings in
    let nl_or_tab = function '\n' | '\t' -> true | _ -> false in
    let nl_or_tab_or_sp = function '\n' | '\t' | ' ' -> true | _ -> false in
    let check_for_whitespace ?(whitespace = nl_or_tab) field value =
      if Base.String.exists value ~f:whitespace then
        warning Warning.(Unexpected_whitespace {field; value}) in
    let check_author = function
      | s when not (Re.execp (Lazy.force author_re) s) ->
          warning Warning.(Wrong_author_format s)
      | _ -> () in
    List.iter
      (fun a ->
        check_author a ;
        check_for_whitespace "author" a )
      metadata.authors ;
    Option.iter (check_for_whitespace "name") metadata.name ;
    Option.iter (check_for_whitespace "version") metadata.version ;
    let check_view (v : View.t) =
      let implementation (i : View.Implementation.t) =
        let open View.Implementation in
        match i with
        | Michelson_storage {code= Micheline mich_code; version; _} -> (
            Option.iter
              (fun value ->
                if protocol_hash_is_valid value then ()
                else
                  error
                    (Error.Michelson_version_not_a_protocol_hash
                       {view= v.name; value} ) )
              version ;
            let open Tezos_micheline.Micheline in
            let node = root mich_code in
            let rec iter = function
              | Int _ | String _ | Bytes _ -> `Other "literal"
              | Prim (_loc, p, args, _annots) -> (
                  if List.mem p forbidden_michelson_instructions then
                    error
                      (Error.Forbidden_michelson_instruction
                         {view= v.name; instruction= p} ) ;
                  let _ = List.map iter args in
                  match p with
                  | "SELF" -> `Self
                  | "ADDRESS" -> `Address
                  | _ -> `Other p )
              | Seq (_loc, l) ->
                  let selves = List.map iter l in
                  ListLabels.fold_left
                    (selves : [`Address | `Other of string | `Self] list)
                    ~init:(`Other "none" : [`Address | `Other of string | `Self])
                    ~f:(fun prev cur ->
                      match (prev, cur) with
                      | `Other _, _ -> cur
                      | `Self, `Address -> cur
                      | `Self, _ ->
                          warning
                            Warning.(
                              Self_unaddressed
                                { view= v.name
                                ; instruction=
                                    ( match cur with
                                    | `Self -> Some "SELF"
                                    | `Other p -> Some p
                                    | `Address -> assert false ) }) ;
                          cur
                      | `Address, _ -> cur ) in
            match iter node with
            | `Self ->
                warning
                  Warning.(Self_unaddressed {view= v.name; instruction= None})
            | _ -> () )
        | Rest_api_query _ -> () in
      check_for_whitespace "view.name" v.name ~whitespace:nl_or_tab_or_sp ;
      List.iter implementation v.implementations in
    List.iter check_view metadata.views ;
    (List.rev !errors, List.rev !warnings)

  let pp ppf =
    let open Fmt in
    function
    | [], [] -> pf ppf "No errors nor warnings."
    | errs, warns ->
        let pp_events prompt pp =
          let itemize ppf = function
            | [] -> const string "None" ppf ()
            | more -> (cut ++ list ~sep:cut (const string "* " ++ pp)) ppf more
          in
          vbox ~indent:2 (const string prompt ++ itemize) in
        vbox
          ( const (pp_events "Errors: " Error.pp) errs
          ++ cut
          ++ const (pp_events "Warnings: " Warning.pp) warns )
          ppf ()
end

module Example = struct
  let rec build = function
    | 0 -> make []
    | 1 ->
        make [] ~homepage:"https://gitlab.com/tezos/tezos"
          ~name:"example-from-the-source"
          ~description:
            {txt|This is a fake metadata blob constructed at
src/lib_contract_metadata/core/metadata_contents.ml in the Tezos codebase.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat.
          
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est
laborum.
|txt}
          ~version:"0.42.0"
          ~license:License.{name= "MIT"; details= Some "The MIT License"}
    | 2 ->
        let m1 = build 1 in
        {m1 with interfaces= ["TZIP-16"; "TZIP-12"]}
    | 3 ->
        let m2 = build 2 in
        { m2 with
          unknown=
            [ ("source-repository", `String "https://gitlab.com/tezos/tezos")
            ; ("commit", `String "8d3077fb78ff157b36a72f15ff2d17df7c4763f7") ]
        }
    | 4 ->
        let m2 = build 2 in
        {m2 with views= [View.Example.build 0]}
    | 5 ->
        let m2 = build 2 in
        {m2 with views= [View.Example.build 0; View.Example.build 1]}
    | _ -> assert false

  let all () =
    let rec o n =
      try
        let ex = build n in
        ex :: o (n + 1)
      with _ -> [] in
    o 0
end
