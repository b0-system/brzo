(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let find_used_keys c =
  let* exists = Os.Dir.exists (Brzo.Conf.cache_dir c) in
  if exists then Ok (String.Set.empty) else
  (Result.map_error (Fmt.str "Cannot determine used keys: %s") @@
   let* l = B0_memo_log.read (Brzo.Conf.log_file c) in
   Ok (B0_cli.File_cache.keys_of_success_ops (B0_memo_log.ops l)))

let get_used_keys c = Result.value ~default:String.Set.empty (find_used_keys c)

let delete c keys =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir c in
  let* _ = B0_cli.File_cache.delete ~dir keys in
  Ok Brzo.Exit.ok

let gc c =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir c in
  let* used = find_used_keys c in
  let* _ = B0_cli.File_cache.gc ~dir ~used in
  Ok Brzo.Exit.ok

let keys c =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir c in
  let* _ = B0_cli.File_cache.keys ~dir in
  Ok Brzo.Exit.ok

let path c =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir c in
  Log.app (fun m -> m "%a" Fpath.pp_unquoted dir);
  Ok Brzo.Exit.ok

let stats c =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir c in
  let* _ = B0_cli.File_cache.stats ~dir ~used:(get_used_keys c) in
  Ok Brzo.Exit.ok

let trim c (max_byte_size, pct) =
  Log.if_error ~use:Brzo.Exit.some_error @@
  let dir = Brzo.Conf.cache_dir c in
  let used = get_used_keys c in
  let* _ = B0_cli.File_cache.trim ~dir ~used ~max_byte_size ~pct in
  Ok Brzo.Exit.ok

(* Command line interface *)

open Cmdliner

let subcmd
    ?(exits = Brzo.Exit.Info.base_cmd) ?(envs = []) name ~doc ~descr term
  =
  let man = [`S Manpage.s_description; descr] in
  Cmd.v (Cmd.info name ~doc ~exits ~envs ~man) term

let stats_term =
  Term.(const stats $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file)

(* Commands *)

let delete =
  let doc = "Delete cache or given keys" in
  let descr = `P "$(tname) deletes cache or given keys." in
  let keys = B0_cli.File_cache.keys_none_is_all ~pos_right:(-1) () in
  subcmd "delete" ~doc ~descr
    Term.(const delete $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file $ keys)

let gc =
  let doc = "Only keep keys used by the last build." in
  let descr = `P "$(tname) all keys except those used by the last build." in
  subcmd "gc" ~doc ~descr
    Term.(const gc $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file)

let keys =
  let doc = "List cache keys" in
  let descr = `P "$(tname) lists all cache keys." in
  subcmd "keys" ~doc ~descr
    Term.(const keys $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file)

let path =
  let doc = "Output cache directory path (may not exist)" in
  let descr = `P "$(tname) outputs the cache directory path." in
  subcmd "path" ~doc ~descr
    Term.(const path $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file)

let stats =
  let doc = "Output cache statistics (default command)" in
  let descr = `P "$(tname) outputs cache statistics." in
  subcmd "stats" ~doc ~descr
    Term.(const stats $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file)

let trim =
  let doc = "Trim the cache to a given budget." in
  let descr = `Blocks [
      `P "$(tname) trims the cache to the minimal given budget. Keys used \
          by the last build are preserved whenever possible.";
      `P "Without options trims to 50% of the current size." ]
  in
  subcmd "trim" ~doc ~descr
    Term.(const trim $ Brzo_tie_conf.auto_cwd_root_and_no_brzo_file $
          B0_cli.File_cache.trim_cli ())

let subs = [delete; gc; keys; path; stats; trim]
let cmd =
  let doc = "Operate on the build cache" in
  let exits = Brzo.Exit.Info.base_cmd in
  let man_xrefs = [ `Main; `Tool "b00-cache" ] in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command operates on the build cache. The default \
        command is $(b,stats).";
    Brzo.Cli.man_see_manual; ]
  in
  Cmd.group (Cmd.info "cache" ~doc ~exits ~man ~man_xrefs)
    ~default:stats_term subs



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
