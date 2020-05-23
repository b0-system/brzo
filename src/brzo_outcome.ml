(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

(* Outcome steps *)

type 'a build_dir_suff =
  B00.Memo.t -> Brzo.Conf.t -> 'a -> string B00.Memo.fiber

type 'a artefact =
  B00.Memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t -> Fpath.t B00.Memo.fiber

type 'a build =
  B00.Memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t -> artefact:Fpath.t ->
  srcs:B00_fexts.map -> unit B00.Memo.fiber

type 'a action =
  B00.Memo.t -> Brzo.Conf.t -> 'a -> build_dir:Fpath.t -> artefact:Fpath.t ->
  (unit -> (Brzo.Exit.t, string) result) B00.Memo.fiber

(* Outcomes *)

type 'a t =
  { pre_outcome : Brzo.Pre_domain.outcome;
    build_dir_suff : 'a build_dir_suff;
    artefact : 'a artefact;
    build : 'a build;
    action_has_args : bool;
    action : 'a action }

let v
    ~name ~doc ?(build_dir_suff = fun _ _ _ k -> k "") ~artefact ~build
    ~action_has_args ~action ()
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
  let exec _ c _ ~build_dir:_ ~artefact k =
    k @@ fun () ->
    let action_args = Brzo.Conf.action_args c in
    let cmd = Cmd.(path artefact %% args action_args) in
    Ok (Brzo.Exit.Exec (artefact, cmd))

  let show_pdf _ c _ ~build_dir:_ ~artefact k =
    k @@ fun () ->
    let pdf_viewer = Brzo.Conf.pdf_viewer c in
    Result.bind (B00_pdf_viewer.find ~pdf_viewer ()) @@ fun pdf_viewer ->
    Result.bind (B00_pdf_viewer.show pdf_viewer artefact) @@
    fun () -> Ok Brzo.Exit.ok

  let show_uri _ c _ ~build_dir:_ ~artefact k =
    k @@ fun () ->
    let uri = Fmt.str "file://%s" (Fpath.to_string artefact) in
    let browser = Brzo.Conf.www_browser c in
    let background = Brzo.Conf.background c in
    Result.bind (B00_www_browser.find ~browser ()) @@ fun browser ->
    Result.bind (B00_www_browser.show ~background ~prefix:true browser uri) @@
    fun () -> Ok Brzo.Exit.ok
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