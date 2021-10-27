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

open Base

let dbg out fmt = Fmt.pf out "@[tezos-contract-metadata-debug: %a@]%!\n" fmt ()

let dbgf ctxt fmt =
  Fmt.(kstr (fun s -> dbg ctxt#formatter (const string (ctxt#log_context ^ s))))
    fmt

(** Return a list, elements mapped through ~map, with elements before the last
    separated with ~sep, and with the last element separated with ~last_sep.
    Note that ~last_sep must include ~sep if you want a genuine Oxford comma --
    but then you'll get it in two-element lists too*)
let rec oxfordize_list l ~map ~sep ~last_sep =
  match l with
  | [] -> []
  | [one] -> [map one]
  | [one; two] -> [map one; last_sep (); map two]
  | one :: more -> map one :: sep () :: oxfordize_list more ~map ~sep ~last_sep

let%test "oxfordize_list" =
  let comma () = ", " in
  let and_ () = ", and " in
  List.equal String.equal
    (oxfordize_list
       ["my parents"; "God"; "Ayn Rand"]
       ~map:Fn.id ~sep:comma ~last_sep:and_ )
    ["my parents"; ", "; "God"; ", and "; "Ayn Rand"]

let%test "oxfordize_list two element" =
  let comma () = ", " in
  let and_ () = ", and " in
  List.equal String.equal
    (oxfordize_list ["God"; "Ayn Rand"] ~map:Fn.id ~sep:comma ~last_sep:and_)
    ["God"; ", and "; "Ayn Rand"]

let%test "oxfordize_list single-element" =
  let sep () = "unused" in
  List.equal String.equal
    (oxfordize_list ~map:String.capitalize ~sep ~last_sep:sep ["foo"])
    ["Foo"]

(** If s is longer than ~max_length, replace any characters after ~max_length
    with ?ellipsis *)
let ellipsize_string ?(ellipsis = " …") s ~max_length =
  if String.length s <= max_length then s
  else String.prefix s max_length ^ ellipsis

let%test "ellipsize shortens" =
  String.equal
    (ellipsize_string ?ellipsis:(Some " yadda yadda yadda") "too long"
       ~max_length:5 )
    "too l yadda yadda yadda"

let%test "ellipsize at exact len" =
  String.equal
    (ellipsize_string ?ellipsis:(Some " yadda yadda yadda") "just right"
       ~max_length:10 )
    "just right"

module Context = struct
  type 'a t = 'a
    constraint
      'a =
      < formatter: Caml.Format.formatter
      ; log_context: string
      ; http_client: Http_client.t
      ; program_time: float
      ; with_log_context: string -> 'a t
      ; nodes: Query_node.Node.t list
      ; .. >
end

module Message = struct
  type t =
    | Text of string
    | Inline_code of string
    | Code_block of string
    | List of t list

  let text s = Text s
  let int f i : t = f (Int.to_string_hum ~delimiter:'_' i)
  let kpp f pp x : t = Fmt.kstr f "%a" pp x
  let inline_code s = Inline_code s
  let code_block s = Code_block s
  let list l = List l
  let ( % ) a b = List [a; b]
  let ( %% ) a b = List [a; text " "; b]
  let parens tt = list [text "("; tt; text ")"]

  let rec pp ppf =
    let open Fmt in
    function
    | Text s -> pf ppf "%s" s
    | Inline_code s -> pf ppf "`%s`" s
    | Code_block s -> pf ppf "@.```@.%s@.```@." s
    | List l -> List.iter l ~f:(pp ppf)
end

module Decorate_error = struct
  exception E of {message: Message.t; trace: exn list}

  let raise ?(trace = []) message = raise (E {message; trace})
  let reraise message ~f = Lwt.catch f (fun e -> raise message ~trace:[e])

  let () =
    Caml.Printexc.register_printer (function
      | E {message; _} -> Some (Fmt.str "Decorated-Error %a" Message.pp message)
      | _ -> None )
end

(** We need this because JSOO can't p[to,oze] generalized tail calls -- see e.g.
    https://github.com/mirage/ezjsonm/issues/41 *)
module Ezjsonm = struct
  include Ezjsonm

  module Stack_reimplementation = struct
    exception Escape of ((int * int) * (int * int)) * Jsonm.error

    let json_of_src src =
      let d = Jsonm.decoder src in
      let dec () =
        match Jsonm.decode d with
        | `Lexeme l -> l
        | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
        | `End | `Await -> assert false in
      let pp_value ppf v = Fmt.pf ppf "%s" (Ezjsonm.value_to_string v) in
      let module Stack_type = struct
        type t =
          [ `A of Ezjsonm.value List.t
          | `Bool of bool
          | `Float of float
          | `In_array of Ezjsonm.value list
          | `In_object of string option * (string * Ezjsonm.value) list
          | `Null
          | `O of (string * Ezjsonm.value) list
          | `String of string ]
      end in
      let pp_stack =
        let open Fmt in
        list ~sep:(any " :: ") (fun ppf -> function
          | `In_object (m, l) ->
              pf ppf "(in-obj %a %a)" (Dump.option string) m
                (list (pair ~sep:(any ":") string pp_value))
                l
          | `In_array l -> pf ppf "(in-array %a)" (list pp_value) l
          | #Ezjsonm.value as v -> pp_value ppf v ) in
      let stack = ref [] in
      let fail_stack fmt =
        Fmt.kstr
          (fun m ->
            let (a, b), (c, d) = Jsonm.decoded_range d in
            Fmt.failwith "%s [%d,%d - %d,%d stack: %a]" m a b c d pp_stack
              !stack )
          fmt in
      let rec go () =
        let stack_value (v : [< Ezjsonm.value]) =
          match !stack with
          | `In_array l :: more -> stack := `In_array (v :: l) :: more
          | `In_object (Some n, l) :: more ->
              stack := `In_object (None, (n, v) :: l) :: more
          | [] -> stack := [(v :> Stack_type.t)]
          | _ -> fail_stack "wrong stack" in
        let pop () =
          match !stack with
          | _ :: more -> stack := more
          | [] -> fail_stack "cannot remove element from stack" in
        ( match dec () with
        | `Os -> stack := `In_object (None, []) :: !stack
        | `Oe -> (
          match !stack with
          | `In_object (Some _, _) :: _ -> fail_stack "name not none"
          | `In_object (None, l) :: _ ->
              pop () ;
              stack_value (`O (List.rev l))
          | _ -> fail_stack "wrong stack, expecting in-object to close object" )
        | `As -> stack := `In_array [] :: !stack
        | `Ae -> (
          match !stack with
          | `In_array l :: _ ->
              pop () ;
              stack_value (`A (List.rev l))
          | _ -> fail_stack "array end not in array" )
        | `Name n -> (
          match !stack with
          | `In_object (None, l) :: more ->
              stack := `In_object (Some n, l) :: more
          | _ -> fail_stack "wrong stack, expecting in-object for field-name" )
        | (`Bool _ | `Null | `Float _ | `String _) as v -> stack_value v ) ;
        match !stack with
        | `In_array _ :: _ | `In_object _ :: _ -> go ()
        | [(#Ezjsonm.value as one)] -> one
        | [] -> fail_stack "stack is empty"
        | _ :: _ :: _ -> go () in
      try `JSON (go ()) with Escape (r, e) -> `Error (r, e)

    let value_to_dst ?(minify = true) dst json =
      let encoder = Jsonm.encoder ~minify dst in
      let encode l = ignore (Jsonm.encode encoder (`Lexeme l)) in
      let rec go = function
        | [] -> ()
        | `Value ((`Bool _ | `Null | `Float _ | `String _) as v) :: more ->
            encode v ; go more
        | `Value (`O l) :: more ->
            encode `Os ;
            go (`Object l :: more)
        | `Value (`A l) :: more ->
            encode `As ;
            go (`Array l :: more)
        | `Object [] :: more ->
            encode `Oe ;
            go more
        | `Object ((k, v) :: o) :: more ->
            encode (`Name k) ;
            go (`Value v :: `Object o :: more)
        | `Array [] :: more ->
            encode `Ae ;
            go more
        | `Array (v :: aa) :: more -> go (`Value v :: `Array aa :: more) in
      go [`Value json] ;
      ignore (Jsonm.encode encoder `End)
  end

  open Stack_reimplementation

  let value_to_buffer ?minify buf json = value_to_dst ?minify (`Buffer buf) json

  let value_to_string ?minify json =
    let buf = Buffer.create 1024 in
    value_to_buffer ?minify buf json ;
    Buffer.contents buf

  let value_from_string s =
    match json_of_src (`String s) with
    | `JSON j -> j
    | `Error (((line, col), (_eline, _ecol)), err) ->
        Decorate_error.raise
          Message.(
            (* Adapted from
               https://github.com/dbuenzli/jsonm/blob/master/src/jsonm.ml *)
            let control_char u = Fmt.kstr inline_code "U+%04X" u in
            let uchar u =
              let module Uchar = Caml.Uchar in
              if Uchar.to_int u <= 0x1F (* most control chars *) then
                control_char (Uchar.to_int u)
              else
                let b = Buffer.create 4 in
                Uutf.Buffer.add_utf_8 b u ;
                Fmt.kstr text "“%s” (=" (Buffer.contents b)
                %% control_char (Uchar.to_int u)
                % text ")" in
            let err_message =
              let pp = Fmt.kstr in
              let ppf = text in
              match err with
              | `Illegal_BOM ->
                  pp ppf
                    "Illegal initial Byte-Order-Mark (BOM) in character stream."
              | `Illegal_escape r -> (
                  pp ppf "Illegal escape:"
                  %%
                  match r with
                  | `Not_hex_uchar u -> uchar u %% text "is not a hex-digit"
                  | `Not_esc_uchar u ->
                      uchar u %% text "is not an escape character"
                  | `Lone_lo_surrogate p ->
                      control_char p %% text "lone low surrogate"
                  | `Lone_hi_surrogate p ->
                      control_char p %% text "lone high surrogate"
                  | `Not_lo_surrogate p ->
                      control_char p %% text "not a low surrogate" )
              | `Illegal_string_uchar u ->
                  text "Illegal character in JSON string:" %% uchar u
              | `Illegal_bytes bs ->
                  let l = String.length bs in
                  let (`Hex hx) = Hex.of_string bs in
                  text "Illegal bytes in character stream ("
                  % Fmt.kstr inline_code "0x%s" hx
                  % text ", length:" %% int inline_code l % text ")"
              | `Illegal_number n -> text "Illegal number:" %% inline_code n
              | `Illegal_literal l -> text "Illegal literal:" %% inline_code l
              | `Unclosed r -> (
                  text "Unclosed"
                  %%
                  match r with
                  | `As -> text "array"
                  | `Os -> text "object"
                  | `String -> text "string"
                  | `Comment -> text "comment" )
              | `Expected r -> (
                  let value_sep =
                    text "value separator" %% parens (inline_code ",") in
                  let tor = text "or" in
                  let array_end =
                    text "end of array" %% parens (inline_code "]") in
                  let object_end =
                    text "end of object" %% parens (inline_code "}") in
                  let field_name =
                    text "field name" %% parens (inline_code "\"…\"") in
                  text "Expected "
                  %%
                  match r with
                  | `Comment -> text "JavaScript comment"
                  | `Value -> text "JSON value"
                  | `Name -> field_name
                  | `Name_sep ->
                      text "field-name separator" %% parens (inline_code ":")
                  | `Aval true -> text "JSON-value" %% tor %% array_end
                  | `Aval false -> value_sep %% tor %% array_end
                  | `Omem true -> field_name %% tor %% object_end
                  | `Omem false -> value_sep %% tor %% object_end
                  | `Json -> text "JSON value"
                  | `Eoi -> text "end of input" ) in
            text "JSON Parsing: at line"
            %% int inline_code line %% text ", column" %% int inline_code col
            % text ":" %% err_message % text ".")
    | exception e -> Fmt.failwith "JSON Parising error: exception %a" Exn.pp e
end

let%test "Test ezjsonm bad json" =
  let src = `String "five" in
  let decoded = Ezjsonm.Stack_reimplementation.json_of_src src in
  match decoded with `Error (((_, _), (_, _)), _) -> true | `JSON _ -> false

let%test "Test ezjsonm unclosed" =
  let src = `String "[5" in
  let decoded = Ezjsonm.Stack_reimplementation.json_of_src src in
  match decoded with `Error (((_, _), (_, _)), _) -> true | `JSON _ -> false

let%test "Test ezjsonm short json" =
  let src = `String "[4]" in
  let decoded = Ezjsonm.Stack_reimplementation.json_of_src src in
  match decoded with `Error (((_, _), (_, _)), _) -> false | `JSON _ -> true
