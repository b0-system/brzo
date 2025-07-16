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
   Ok (B0_memo_cli.File_cache.keys_of_success_ops (B0_memo_log.ops l)))

let try_find_used_keys ?kind ~conf () = match kind with
| Some `Any -> String.Set.empty
| None | Some (`Used | `Unused)  ->
    Log.if_error ~level:Log.Warning ~use:String.Set.empty @@
    find_used_keys ~conf

let delete ~conf ~kind ~keys =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let used = try_find_used_keys ~kind ~conf () in
  let* _exists = B0_memo_cli.File_cache.delete ~dir ~used ~kind keys in
  Ok Brzo.Exit.ok

let gc ~dry_run ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let* used = find_used_keys ~conf in
  let* _exists = B0_memo_cli.File_cache.gc ~dry_run ~dir ~used in
  Ok Brzo.Exit.ok

let keys ~kind ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let used = try_find_used_keys ~kind ~conf () in
  let* _exists = B0_memo_cli.File_cache.keys ~dir ~used ~kind in
  Ok Brzo.Exit.ok

let path ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  Fmt.pr "%a@." Fpath.pp_unquoted dir;
  Ok Brzo.Exit.ok

let stats ~conf =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let used = try_find_used_keys ~conf () in
  let* _exists = B0_memo_cli.File_cache.stats ~dir ~used in
  Ok Brzo.Exit.ok

let trim ~conf ~dry_run ~trim_spec:(max_byte_size, pct) =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir conf in
  let used = try_find_used_keys ~conf () in
  let* _ =
    B0_memo_cli.File_cache.trim ~dry_run ~dir ~used ~max_byte_size ~pct
  in
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd
    ?(exits = Brzo.Exit.Info.base_cmd) ?(envs = []) name ~doc ~descr term
  =
  let man = [`S Manpage.s_description; descr] in
  Cmd.make (Cmd.info name ~doc ~exits ~envs ~man) term

let dry_run = B0_memo_cli.File_cache.dry_run ()
let kind = B0_memo_cli.File_cache.key_kind_cli ()

(* Commands *)

let delete_cmd =
  let doc = "Delete cache or given keys" in
  let descr =
    `P "$(cmd) deletes the cache or the given keys. Use \
        $(cmd.parent) $(b,keys) to list them.";
  in
  cmd "delete" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ keys = B0_memo_cli.File_cache.keys_none_is_all () and+ kind in
  delete ~conf ~keys ~kind

let gc_cmd =
  let doc = "Only keep keys used by the build" in
  let descr = `Blocks [
      `P "$(cmd) deletes all keys except those used by the build. This is \
          the same as $(cmd.parent) $(b,delete --unused).";
      `P "Use $(cmd.parent) $(b,trim) to trim down to a size budget."; ]
  in
  cmd "gc" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ dry_run in
  gc ~conf ~dry_run

let keys_cmd =
  let doc = "List cache keys" in
  let descr = `P "$(cmd) lists cache keys." in
  cmd "keys" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ kind in
  keys ~conf ~kind

let path_cmd =
  let doc = "Output cache directory path (may not exist)" in
  let descr = `P "$(cmd) outputs the cache directory path." in
  cmd "path" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file in
  path ~conf

let stats_cmd =
  let doc = "Output cache statistics" in
  let descr =
    `P "$(cmd) outputs cache statistics. The numbers reported as 'used' are \
        for the keys used by build.";
  in
  cmd "stats" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file in
  stats ~conf

let trim_cmd =
  let doc = "Trim the cache to a given size budget" in
  let descr = `Blocks [
      `P "$(cmd) trims the cache to the minimal given size budget. Without \
          options trims to 50% of the current size. Keys used \
          by the build are preserved whenever possible.";
      `P "Use $(tool) $(b,cache gc) to only keep the keys used \
          by the build." ]
  in
  cmd "trim" ~doc ~descr @@
  let+ conf = Brzo_tie_conf.auto_cwd_root_and_no_brzo_file
  and+ trim_spec = B0_memo_cli.File_cache.trim_cli () and+ dry_run in
  trim ~conf ~trim_spec ~dry_run

let cmd =
  let doc = "Operate on the build cache" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) operates on the build cache. A cache key is used by the \
        build if it corresponds to one of its operations. This is determined \
        by looking up the brzo build log file.";
    `Pre "$(cmd) $(b,stats)              # Output cache statistics";
    `Noblank;
    `Pre "$(cmd) $(b,gc)                 # Only keep keys used by the build";
    `Noblank;
    `Pre "$(cmd) $(b,trim)               # Trim cache by 50%";
    `Noblank;
    `Pre "$(cmd) $(b,trim --to-mb=100)   # Trim cache to 100MB";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.group (Cmd.info "cache" ~doc ~exits ~man) @@
  [delete_cmd; gc_cmd; keys_cmd; path_cmd; stats_cmd; trim_cmd]
