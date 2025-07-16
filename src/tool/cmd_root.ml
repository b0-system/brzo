(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let root ~conf =
  Log.stdout (fun m -> m "%a" Fpath.pp_unquoted (Brzo.Conf.root conf));
  Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Output root directory" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the brzo root directory.";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.make (Cmd.info "root" ~doc ~exits ~man) @@
  let+ conf = Brzo_tie_conf.no_brzo_file in
  root ~conf
