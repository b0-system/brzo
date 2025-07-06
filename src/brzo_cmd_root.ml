(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let root c =
  Log.stdout (fun m -> m "%a" Fpath.pp_unquoted (Brzo.Conf.root c));
  Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Show root directory" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command shows the brzo root directory.";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.v (Cmd.info "root" ~doc ~exits ~man)
    Term.(const root $ Brzo_tie_conf.no_brzo_file)
