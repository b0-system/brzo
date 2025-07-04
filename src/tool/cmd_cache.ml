(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let find_used_keys ~conf =
  let* exists = Os.Dir.exists (Brzo.Conf.cache_dir conf) in
  if exists then Ok (String.Set.empty) else
  (Result.map_error (Fmt.str "Cannot determine used keys: %s") @@
   let* l = B0_memo_log.read (Brzo.Conf.log_file conf) in
   Ok (B0_cli.File_cache.keys_of_success_ops (B0_memo_log.ops l)))

let get_used_keys ~conf =
  Result.value ~default:String.Set.empty (find_used_keys ~conf)

let delete ~conf ~keys =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let* _ = B0_cli.File_cache.delete ~dir keys in
  Ok Brzo.Exit.ok

let gc ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let* used = find_used_keys ~conf in
  let* _ = B0_cli.File_cache.gc ~dir ~used in
  Ok Brzo.Exit.ok

let keys ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let* _ = B0_cli.File_cache.keys ~dir in
  Ok Brzo.Exit.ok

let path ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  Log.stdout (fun m -> m "%a" Fpath.pp_unquoted dir);
  Ok Brzo.Exit.ok

let stats ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let* _ = B0_cli.File_cache.stats ~dir ~used:(get_used_keys ~conf) in
  Ok Brzo.Exit.ok

let trim ~conf ~trim_spec:(max_byte_size, pct) =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let used = get_used_keys ~conf in
  let* _ = B0_cli.File_cache.trim ~dir ~used ~max_byte_size ~pct in
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let subcmd
    ?(exits = Brzo.Exit.Info.base_cmd) ?(envs = []) name ~doc ~descr term
  =
  let man = [`S Manpage.s_description; descr] in
  Cmd.make (Cmd.info name ~doc ~exits ~envs ~man) term

(* Commands *)

let delete =
  let doc = "Delete cache or given keys" in
  let descr = `P "$(cmd) deletes cache or given keys." in
  subcmd "delete" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ keys = B0_cli.File_cache.keys_none_is_all ~pos_right:(-1) () in
  delete ~conf ~keys

let gc =
  let doc = "Only keep keys used by the last build." in
  let descr = `P "$(cmd) all keys except those used by the last build." in
  subcmd "gc" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file in
  gc ~conf

let keys =
  let doc = "List cache keys" in
  let descr = `P "$(cmd) lists all cache keys." in
  subcmd "keys" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file in
  keys ~conf

let path =
  let doc = "Output cache directory path (may not exist)" in
  let descr = `P "$(cmd) outputs the cache directory path." in
  subcmd "path" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file in
  path ~conf

let stats =
  let doc = "Output cache statistics (default command)" in
  let descr = `P "$(cmd) outputs cache statistics." in
  subcmd "stats" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file in
  stats ~conf

let trim =
  let doc = "Trim the cache to a given budget." in
  let descr = `Blocks [
      `P "$(cmd) trims the cache to the minimal given budget. Keys used \
          by the last build are preserved whenever possible.";
      `P "Without options trims to 50% of the current size." ]
  in
  subcmd "trim" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ trim_spec = B0_cli.File_cache.trim_cli () in
  trim ~conf ~trim_spec

let cmd =
  let doc = "Operate on the build cache" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man_xrefs = [ `Main; `Tool "b00-cache" ] in
  let man = [
    `S Manpage.s_description;
    `P "The $(cmd) command operates on the build cache. The default \
        command is $(b,stats).";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.group (Cmd.info "cache" ~doc ~exits ~man ~man_xrefs) @@
  [delete; gc; keys; path; stats; trim]
