
let revolt () = print_endline "Revolt!"

let () = match !Sys.interactive with
| true -> ()
| false ->
    let open Cmdliner in
    let revolt_t = Term.(const revolt $ const ()) in
    Term.exit @@ Term.eval (revolt_t, Term.info "revolt")
