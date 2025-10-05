(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax

let tool = B0_memo.Tool.by_name ~vars:["PATH"] "opam"
let opam_bin = Cmd.arg "opam"

(* FIXME memo is used here but only for future proofing but we should
   really use it. Also we need an easy no caching option spawns in Memo. *)

let exists m = match Os.Cmd.find opam_bin with
| None -> Fut.return false | Some _ -> Fut.return true

let if_exists m f =
  let* exists = exists m in
  if not exists then Fut.return None else
  Fut.map Option.some (f ())

let run m ?switch:s cmd oargs =
  let opam = Os.Cmd.get opam_bin |> B0_memo.fail_if_error m in
  let s = match s with None -> Cmd.empty | Some s -> Cmd.(arg "--switch" % s) in
  Fut.return
    (Os.Cmd.run_out ~trim:true Cmd.(opam % cmd %% s %% oargs))

let lib_dir m ?switch () =
  let* r = run m ?switch "var" Cmd.(arg "lib") in
  let lib_dir = Result.bind r Fpath.of_string in
  let lib_dir = B0_memo.fail_if_error m lib_dir in
  Fut.return lib_dir

let list m ?switch which () =
  let which = match which with
  | `Available -> Cmd.arg "--available"
  | `Installed -> Cmd.arg "--installed"
  in
  let* r = run m ?switch "list" Cmd.(which % "--short") in
  let list = Result.bind r @@ fun s ->
    let add_pkg _ acc pkg = pkg :: acc in
    Ok (List.rev (String.fold_ascii_lines ~drop_newlines:true add_pkg [] s))
  in
  let list = B0_memo.fail_if_error m list in
  Fut.return list

(* FIXME *)
type pkg = string * string option
let pkg_list ?switch:s () =
  let parse_pkg n acc s =
    if s = "" then acc else
    match String.split_first ~sep:" " s with
    | None -> Fmt.failwith_line n " Cannot parse package from %S" s
    | Some (pkg, version) ->
        let pkg = match String.trim version with
        | "--" -> (String.trim pkg, None)
        | v -> (String.trim pkg, Some v)
        in
        pkg :: acc
  in
  Result.bind (Os.Cmd.get opam_bin) @@ fun opam ->
  let s =
    match s with None -> Cmd.empty | Some s -> Cmd.(arg "--switch" % s)
  in
  let list =
    Cmd.(opam % "list" %% s % "--columns=name,installed-version"  %
         "--short" % "--normalise" % "--all")
  in
  Result.bind (Os.Cmd.run_out ~trim:true list) @@ fun s ->
  try Ok (String.fold_ascii_lines ~drop_newlines:true parse_pkg [] s) with
  | Failure e -> Fpath.error Fpath.dash "%s" e
