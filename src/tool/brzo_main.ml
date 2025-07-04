(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Cmdliner
open Cmdliner.Term.Syntax

let domain_cmd (Brzo_domain.V d as domain) =
  let module D = (val d : Brzo_domain.T with type Conf.t = 'a) in
  let doc = Fmt.str "%s domain" D.doc_name and docs = Brzo.Cli.s_domains in
  let sdocs = Manpage.s_common_options and man_xrefs = [ `Main ] in
  let exits = Brzo.Exit.Info.domain_cmd in
  let man = [
    `S Manpage.s_description;
    `P (Fmt.str
          "The $(cmd) command has outcomes for the %s domain. More \
           information is available in the manual, see $(b,odig doc brzo)."
          D.doc_name);
    `S Manpage.s_arguments;
    `S Brzo.Cli.s_outcomes;
    `S Brzo.Cli.s_outcome_opts;
    `P "Some of these options are ignored by certain outcomes.";
    `S Brzo.Cli.s_outcome_mode;
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.make (Cmd.info D.name ~doc ~docs ~sdocs ~exits ~man ~man_xrefs) @@
  let+ conf = Brzo_tie_conf.for_domain ~domain in
  Brzo_domain.run conf domain

let tool =
  let doc = "Quick-setting builds" in
  let sdocs = Manpage.s_common_options in
  let exits = Brzo.Exit.Info.domain_cmd in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(tool) -- [$(i,ARG)]…"; `Noblank;
    `P "$(tool) $(i,COMMAND) …";
    `S Manpage.s_description;
    `P "$(tool) quickly turns source files of various languages into \
        executable programs and documents. It has support for C, CommonMark, \
        LaTeX and OCaml.";
    `Pre "Use $(tool) -- [$(b,ARG)]… to build and execute a program with \
          given arguments."; `Noblank;
    `Pre
      "Use $(tool) $(b,--doc) to build and show source file documentation.";
    `Pre "Use $(tool) [$(i,COMMAND)]… $(b,--help) for help about any command.";
    `P "More information is available in the manual, see $(b,odig doc brzo).";
    `S Brzo.Cli.s_domains;
    `S Manpage.s_arguments;
    `S Brzo.Cli.s_outcomes;
    `S Brzo.Cli.s_outcome_mode;
    Brzo.Cli.man_see_manual;
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information."; ]
  in
  Cmd.group (Cmd.info "brzo" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man)
    ~default:Cmd_default.term @@
  Cmd_cache.cmd :: Cmd_default.cmd :: Cmd_domain.cmd ::
  Cmd_delete.cmd :: Cmd_file.cmd :: Cmd_log.cmd ::
  Cmd_root.cmd :: Cmd_sources.cmd ::
  List.rev_map domain_cmd Brzo_domain_list.v

let main () =
  Log.time (fun _ m -> m "total time brzo %%VERSION%%") @@ fun () ->
  B0_std_cli.Exit.of_eval_result ~term_error:Brzo.Exit.conf_error @@
  Cmd.eval_value tool

let () = if !Sys.interactive then () else Os.Exit.exit (main ())
