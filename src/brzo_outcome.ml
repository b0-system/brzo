(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

(* Outcome steps *)

type 'a build_dir_suff =
  B0_memo.t -> Brzo.Conf.t -> 'a -> string Fut.t

type 'a artefact =
  B0_memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t -> Fpath.t Fut.t

type 'a build =
  B0_memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t ->
  artefact:Fpath.t -> srcs:B0_file_exts.map -> unit Fut.t

type 'a action =
  B0_memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t ->
  artefact:Fpath.t -> (unit -> (Os.Exit.t, string) result) Fut.t

(* Outcomes *)

type 'a t =
  { pre_outcome : Brzo.Pre_domain.outcome;
    build_dir_suff : 'a build_dir_suff;
    artefact : 'a artefact;
    build : 'a build;
    action_has_args : bool;
    action : 'a action }

let v
    ~name ~doc ?(build_dir_suff = fun m _ _ -> Fut.return "")
    ~artefact ~build ~action_has_args ~action ()
  =
  let pre_outcome = Brzo.Pre_domain.outcome ~name ~doc in
  { pre_outcome; build_dir_suff; artefact; build; action_has_args; action }

let name o = Brzo.Pre_domain.outcome_name o.pre_outcome
let build_dir_suff o = o.build_dir_suff
let artefact o = o.artefact
let build o = o.build
let action_has_args o = o.action_has_args
let action o = o.action
let pre_outcome o = o.pre_outcome
let with_outcome ~name ~doc t =
  let pre_outcome = Brzo.Pre_domain.outcome ~name ~doc in
  { t with pre_outcome }

let get o os = try List.find (fun o' -> String.equal o (name o')) os with
| Not_found -> assert false

(* Predefined actions *)

module Action = struct
  open Result.Syntax

  let exec m c _ ~build_dir:_ ~artefact =
    Fut.return @@ fun () ->
    let action_args = Brzo.Conf.action_args c in
    let cmd = Cmd.(path artefact %% list action_args) in
    Ok (Os.Exit.execv cmd)

  let show_pdf m c _ ~build_dir:_ ~artefact =
    Fut.return @@ fun () ->
    let cmd = Brzo.Conf.pdf_viewer c in
    let* pdf_viewer = B0_pdf_viewer.find ?cmd () in
    let* () = B0_pdf_viewer.show pdf_viewer artefact in
    Ok Brzo.Exit.ok

  let show_uri m c _ ~build_dir:_ ~artefact =
    Fut.return @@ fun () ->
    let url = Fmt.str "file://%s" (Fpath.to_string artefact) in
    let browser = Brzo.Conf.web_browser c in
    let background = Brzo.Conf.background c in
    let* browser = B0_web_browser.find ?cmd:browser () in
    let* () = B0_web_browser.show ~background ~prefix:true browser url in
    Ok Brzo.Exit.ok
end

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
