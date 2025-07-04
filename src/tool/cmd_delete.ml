(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let delete ~conf ~clean =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let b0_dir = Brzo.Conf.b0_dir conf in
  let del_dir = match clean with
  | true -> b0_dir
  | false -> Fpath.(b0_dir / Brzo.Conf.brzo_dir_name)
  in
  let* _ = Os.Path.delete ~recurse:true del_dir in
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Delete builds" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "The $(cmd) command deletes all outcome builds and artefacts. The \
        build cache is however kept intact.";
    `P "Use option $(b,--clean) option to get rid of the $(b,_b0) directory \
        altogeter. The will likely also get rid of the build cache. \
        See $(tool) $(b,cache) for finer control over build cache deletions.";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.make (Cmd.info "delete" ~doc ~exits ~man) @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ clean =
    let doc = "Delete the $(b,_b0) directory." in
    Arg.(value & flag & info ["c"; "clean"] ~doc)
  in
  delete ~conf ~clean
