(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open B0_sexp

(* A few notes that may end up being ux quirks.

   1. When we output on stdout we add a final newline if there's none.
      I'm sure this is a bad idea, but let's do it.
   2. We get/set key bindings as sequences of s-expression. This is
      syntactically compatible with the fact that set always splices. *)

let show_sexp pp sexp =
  let s = Fmt.str "@[%a@]" pp sexp in
  let s = if s <> "" && s.[String.length s - 1] <> '\n' then s ^ "\n" else s in
  if Log.level () > Log.Quiet then output_string stdout s

let get_action_arg parse ~kind v k =
  Log.if_error' ~use:Brzo.Exit.some_error @@
  match parse v with
  | Ok v -> k v
  | Error _ ->
      (* The parse errors are not really fit/good here we circumvent them
         for now. *)
      Fmt.error "Could not parse %s argument %S." kind v

(* FIXME this should be moved to cmdliner cli parsing **)
let get_spath = get_action_arg Sexp.path_of_string ~kind:"s-expression path"
let get_caret = get_action_arg Sexp.caret_of_string ~kind:"s-expression caret"
let get_sexps = get_action_arg Brzo.Sexp.of_string ~kind:"s-expression"

let get_brzo_file ~conf k =
  Log.if_error' ~use:Brzo.Exit.conf_error @@
  match Brzo.Conf.brzo_file conf with
  | Some file -> k file
  | None ->
      let pp_root = Fmt.code' Fpath.pp_unquoted in
      Fmt.error "No BRZO file found in root %a" pp_root (Brzo.Conf.root conf)

let get_sexp_of_file file k =
  Log.if_error' ~header:"" ~use:Brzo.Exit.conf_error @@
  Result.bind (Brzo.Sexp.of_file file) k

let update_file ~dry_run file sexp k =
  Log.if_error' ~use:Brzo.Exit.some_error @@
  match dry_run with
  | true -> k (show_sexp Sexp.pp_seq_layout sexp)
  | false ->
      let data = Fmt.str "@[%a@]" Sexp.pp_seq_layout sexp in
      Result.bind (Os.File.write ~force:true ~make_path:true file data) k

let sexp_query query sexp k =
  Log.if_error' ~header:"" ~use:Brzo.Exit.no_such_sexp_path @@
  Result.bind (Brzo.Sexp.query query sexp) k

let delete ~conf ~spath ~dry_run =
  Log.if_error ~use:Brzo.Exit.some_error @@
  get_brzo_file ~conf @@ fun file ->
  get_sexp_of_file file @@ fun sexp ->
  get_spath spath @@ fun spath ->
  sexp_query (Sexpq.delete_at_path ~must_exist:true spath) sexp @@ fun sexp ->
  update_file ~dry_run file sexp @@ fun () ->
  Ok Brzo.Exit.ok

let edit ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let brzo_file = match Brzo.Conf.brzo_file conf with
  | Some file -> file
  | None -> Fpath.(Brzo.Conf.root conf / Brzo.Conf.brzo_file_name)
  in
  let* editor = B0_editor.find () in
  let* exit = B0_editor.edit_files editor [brzo_file] in
  match exit with
  | `Exited 0 -> Ok Brzo.Exit.ok
  | _ -> Ok Brzo.Exit.some_error

let get ~conf ~spath =
  Log.if_error ~use:Brzo.Exit.some_error @@
  get_brzo_file ~conf @@ fun file ->
  get_sexp_of_file file @@ fun sexp ->
  match spath with
  | None -> show_sexp Sexp.pp_seq_layout (fst sexp); Ok Brzo.Exit.ok
  | Some spath ->
      get_spath spath @@ fun path ->
      sexp_query (Sexpq.path path Sexpq.sexp) sexp @@ fun r ->
      let is_key = function Sexp.Key _ :: _ -> true | _ -> false in
      let sexp = match r with
      | `L (l, _) when is_key path -> l
      | sexp -> [sexp]
      in
      show_sexp Fmt.(list ~sep:sp Sexp.pp) sexp;
      Ok Brzo.Exit.ok

let path ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  get_brzo_file ~conf @@ fun file ->
  Log.stdout (fun m -> m "%a" Fpath.pp_unquoted file);
  Ok Brzo.Exit.ok

let set ~conf ~caret ~sexps ~dry_run =
  Log.if_error ~use:Brzo.Exit.some_error @@
  get_caret caret @@ fun caret ->
  get_sexps sexps @@ fun sexps ->
  get_brzo_file ~conf @@ fun file ->
  get_sexp_of_file file @@ fun sexp ->
  let query = Sexpq.splice_at_caret ~must_exist:false caret ~rep:sexps in
  sexp_query query sexp @@ fun sexp ->
  update_file ~dry_run file sexp @@ fun () ->
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let dry_run =
  let doc = "Do not edit the BRZO file in place but show the result on stdout."
  in
  Arg.(value & flag & info ["t"; "dry-run"] ~doc)

let subcmd
    ?(exits = Brzo.Exit.Info.base_cmd) ?(envs = []) name ~doc ~descr term
  =
  let man = [`S Manpage.s_description; descr] in
  Cmd.make (Cmd.info name ~doc ~exits ~envs ~man) term

(* Commands *)

let delete =
  let doc = "Delete an s-expression from the BRZO file" in
  let descr = `Blocks [
      `P "$(cmd) delete the s-expression at path $(i,SPATH) from the BRZO \
          file. If $(i,SPATH) ends with a key index it removes the binding.";
      `P "If $(b,--dry-run) is specified the file is not modified and the \
          result of the operation on the BRZO file is output on $(b,stdout)." ]
  in
  subcmd "delete" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ spath =
    let doc = "The s-expression path to act on. See $(tool) $(b,file --help) \
               for the syntax." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"SPATH")
  and+ dry_run in
  delete ~conf ~spath ~dry_run

let edit =
  let doc = "Edit or create the BRZO file" in
  let descr = `P "$(cmd) edits opens the BRZO file in your editor." in
  let envs = B0_editor.Env.infos in
  subcmd "edit" ~doc ~envs ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file in
  edit ~conf

let get =
  let doc = "Get an s-expression from the BRZO file" in
  let descr =
    `P "$(cmd) outputs the s-expression bound to path $(i,SPATH). \
        If no path is specified the whole BRZO file is output. Key bindings \
        are returned as a sequence of s-expressions."
  in
  subcmd "get" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ spath =
    let doc = "The s-expression path to get. See $(tool) $(b,file --help) \
               for the path syntax." in
    Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"SPATH")
  in
  get ~conf ~spath

let path =
  let doc = "Output the BRZO file path (default command)" in
  let descr =
    `P "$(cmd) outputs the file path to the BRZO file. Errors if there is none."
  in
  subcmd "path" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file in
  path ~conf

let set =
  let doc = "Set an s-expression in the BRZO file" in
  let descr = `Blocks [
      `P "$(cmd) splices the s-expression sequence $(i,SEXPSEQ) (a single \
          cli arg) at the caret $(i,CARET) in the BRZO file.";
      `P "To set a key binding use a sequence of s-expressions not a list; \
           the latter sets the first element of the binding to a list.";
      `P "If $(b,--dry-run) is specified the file is not modified and the \
          result of the operation on the BRZO file is output on $(b,stdout)."]
  in
  subcmd "set" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ caret =
    let doc = "The s-expression caret to act on. See $(tool) $(b,file --help) \
               for the caret syntax."
    in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"CARET")
  and+ sexps =
    let doc =
      "The $(b,set) value. This is a sequence of s-expressions \
       spliced at the caret. The sequence must be a single cli argument."
    in
    Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"SEXPSEQ")
  and+ dry_run in
  set ~conf ~caret ~sexps ~dry_run

let cmd =
  let doc = "Query and edit the BRZO file" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "The $(cmd) command queries and edits the BRZO file. The default \
        command is $(b,path).";
    `P "The file is edited from the command line by using s-expression paths \
        and carets which are described in a dedicated section below.";
    `S Manpage.s_commands;
    `S "PATHS AND CARETS";
    `P "An s-expression path $(b,SPATH) is a sequence of dot separated \
        bracketed key dictionary and list indexing operations. Brackets \
        can be omited if there is no ambiguity. List indices are \
        zero-based and negative numbers count from the end: -1 is the \
        last element of a list. Examples:";
    `Pre "$(b,ocaml.libs)       # value of key $(b,libs) in dictionary \
          $(b,ocaml)"; `Noblank;
    `Pre "$(b,ocaml.libs.[0])   # first element of value bound to \
          $(b,ocaml.libs)"; `Noblank;
    `Pre "$(b,ocaml.libs.[-1])  # last element of value bound to \
          $(b,ocaml.libs)";
    `P "An s-expression caret $(b,CARET) is an s-expression path whose last \
        index may specify an insertion point $(b,v) before or after the \
        brackets to denote an insertion before or after the expression found \
        by the path. Examples:";
    `Pre "$(b,ocaml.v[libs])     # before key $(b,libs) in dictionary \
          $(b,ocaml)"; `Noblank;
    `Pre "$(b,ocaml.libs.v[0])   # before first element of value bound to \
          $(b,ocaml.libs)"; `Noblank;
    `Pre "$(b,ocaml.libs.[-1]v)  # after last element of value bound to \
          $(b,ocaml.libs)";
    Brzo.Cli.man_see_manual ]
  in
  Cmd.group (Cmd.info "file" ~doc ~exits ~man) @@
  [delete; edit; path; get; set]
