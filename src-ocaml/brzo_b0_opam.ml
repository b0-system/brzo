(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
    Ok (List.rev (String.fold_ascii_lines ~strip_newlines:true add_pkg [] s))
  in
  let list = B0_memo.fail_if_error m list in
  Fut.return list

(* FIXME *)
type pkg = string * string option
let pkg_list ?switch:s () =
  let parse_pkg n acc s =
    if s = "" then acc else
    match String.cut ~sep:" " s with
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
  try Ok (String.fold_ascii_lines ~strip_newlines:true parse_pkg [] s) with
  | Failure e -> Fpath.error Fpath.dash "%s" e


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
