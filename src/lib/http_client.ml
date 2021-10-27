type http_error = Failure of string
type result = (string, http_error) Base.Result.t

type t =
  { get: ?limit_bytes:int -> string -> result Lwt.t
  ; post: headers:Cohttp.Header.t -> body:string -> string -> result Lwt.t }

let failure msg = Error (Failure msg)

let pp_http_error ppf error =
  match error with Failure msg -> Fmt.pf ppf "%s" msg
