let log fmt = Printf.printf (fmt ^^ "\n")

let dump_cwd () =
  log "Current working directory:";
  match Sys.getcwd () with
  | exception Sys_error e -> log "Error: %s" e
  | cwd -> log " %s" cwd

let dump_some_env () =
  let dump_env_var var = match Sys.getenv var with
  | exception Not_found -> log " %s: not found" var
  | v -> log " %s=%s" var v
  in
  log "Some environment variables:";
  dump_env_var "HOME";
  dump_env_var "PATH"

let dump_args () =
  let quote s = Printf.sprintf "'%s'" s in
  let qargs = List.map quote (Array.to_list Sys.argv) in
  log "Command line arguments:";
  log " %s" (String.concat " " qargs)

let dump_file_arg i f =
  if i = 0 then () else
  match open_in_bin f with
  | exception Sys_error _ -> ()
  | ic ->
      log "Contents of file %s:" f;
      try
        let rec loop ic = match input_line ic with
        | exception End_of_file -> log ""
        | l -> log "%s" l; loop ic
        in
        loop ic; close_in ic
      with
      | Sys_error e -> log " Error: %s\n" e

let dump () =
  dump_cwd (); log "";
  dump_some_env (); log "";
  dump_args (); log "";
  Array.iteri dump_file_arg Sys.argv;
  flush stdout

let () = if !Sys.interactive then () else dump ()
