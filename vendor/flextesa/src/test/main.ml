open Flextesa
open Internal_pervasives
module MFmt = More_fmt

module Test_fold_process = struct
  let run () =
    let open Running_processes.Async in
    Dbg.e EF.(af "test_fold_process: Starting") ;
    Lwt_process.with_process_full
      ( ""
      , [| "sh"; "-c"
         ; "ls -l /delijde ; ls / ; echo -n bouh ; ls ; echo ba >&2 ; ls /no ; \
            ls /still-no ; echo A >&2 ; echo B ; echo C" |] )
      (fun proc ->
        fold_process proc ~init:0 ~f:(fun n outs errs ->
            Dbg.e EF.(af "test_fold_process: %d %S %S" n outs errs) ;
            if n < 5 then return (`Continue (n + 1)) else return (`Done (n + 1)) )
        )
    >>= fun _ -> return ()
end

let all () = Test_fold_process.run ()

let () =
  match Lwt_main.run (all ()) with
  | {result= test_result; attachments} ->
      MFmt.(
        epr "%a@.%!"
          (fun ppf () ->
            vertical_box ~indent:2 ppf (fun ppf ->
                string ppf "Tests done, result:" ;
                cut ppf () ;
                vertical_box ~indent:2 ppf (fun ppf ->
                    string ppf "* Attachments:" ;
                    match attachments with
                    | [] -> pf ppf " None."
                    | more ->
                        List.iter more ~f:(fun _ ->
                            cut ppf () ; pf ppf "* todo" ) ) ;
                cut ppf () ;
                vertical_box ~indent:2 ppf (fun ppf ->
                    string ppf "* Result:" ;
                    match test_result with
                    | Ok () -> pf ppf " Ok."
                    | Error _ -> pf ppf " Error: TODO" ) ) )
          ())
