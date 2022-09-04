(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Cmdliner

let domain_cmd (Brzo_domain.V d as domain) =
  let module D = (val d : Brzo_domain.T with type Conf.t = 'a) in
  let doc = Fmt.str "%s domain" D.doc_name and docs = Brzo.Cli.s_domains in
  let sdocs = Manpage.s_common_options and man_xrefs = [ `Main ] in
  let exits = Brzo.Exit.Info.domain_cmd in
  let man = [
    `S Manpage.s_description;
    `P (Fmt.str
          "The $(tname) command has outcomes for the %s domain. More \
           information is available in the manual, see $(b,odig doc brzo)."
          D.doc_name);
    `S Manpage.s_arguments;
    `S Brzo.Cli.s_outcomes;
    `S Brzo.Cli.s_outcome_opts;
    `P "Some of these options are ignored by certain outcomes.";
    `S Brzo.Cli.s_outcome_mode;
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.v (Cmd.info D.name ~doc ~docs ~sdocs ~exits ~man ~man_xrefs)
    Term.(const Brzo_domain.run $ Brzo_tie_conf.for_domain ~domain $
          const domain)

let cmds =
  Brzo_cmd_cache.cmd :: Brzo_cmd_default.cmd :: Brzo_cmd_domain.cmd ::
  Brzo_cmd_delete.cmd :: Brzo_cmd_file.cmd :: Brzo_cmd_log.cmd ::
  Brzo_cmd_root.cmd :: Brzo_cmd_sources.cmd ::
  List.rev_map domain_cmd Brzo_domain_list.v

let tool =
  let doc = "Quick-setting builds" in
  let sdocs = Manpage.s_common_options in
  let exits = Brzo.Exit.Info.domain_cmd in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(mname) -- [$(i,ARG)]…"; `Noblank;
    `P "$(mname) $(i,COMMAND) …";
    `S Manpage.s_description;
    `P "$(mname) quickly turns source files of various languages into \
        executable programs and documents. It has support for C, CommonMark, \
        LaTeX and OCaml.";
    `Pre "Use $(mname) -- [$(b,ARG)]… to build and execute a program with \
          given arguments."; `Noblank;
    `Pre
      "Use $(mname) $(b,--doc) to build and show source file documentation.";
    `Pre "Use $(mname) [$(i,COMMAND)]… $(b,--help) for help about any command.";
    `P "More information is available in the manual, see $(b,odig doc brzo).";
    `S Brzo.Cli.s_domains;
    `S Manpage.s_arguments;
    `S Brzo.Cli.s_outcomes;
    `S Brzo.Cli.s_outcome_mode;
    Brzo.Cli.man_see_manual;
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information."; ]
  in
  Cmd.group
    (Cmd.info "brzo" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man)
    ~default:Brzo_cmd_default.term cmds

let main () =
  B00_cli.Exit.exit ~exec_error:Brzo.Exit.some_error @@
  Log.time (fun _ m -> m "total time brzo %%VERSION%%") @@ fun () ->
  B00_cli.Exit.of_eval_result ~term_error:Brzo.Exit.conf_error @@
  Cmd.eval_value tool

let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
