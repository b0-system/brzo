(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00

module Mod_resolver = struct
  type dep = Fpath.t
  type deps = [ `Open of dep list | `Locked of dep list ]
  let unrestricted = `Open []
  let restrictions m index dep_dirs deps =
    let rec add_dep added acc miss dep = function
    | dir :: dirs ->
        let d = Fpath.(to_dir_path @@ dir // dep) in
        if Fpath.Set.mem d (B00_findex.dirs index)
        then add_dep true (d :: acc) miss dep dirs
        else add_dep added acc miss dep dirs
    | [] when added -> acc, miss
    | [] -> acc, (dep :: miss)
    in
    let rec loop acc miss dep_dirs = function
    | [] -> List.rev acc, miss
    | dep :: deps ->
        let acc, miss = add_dep false acc miss dep dep_dirs in
        loop acc miss dep_dirs deps
    in
    loop [] [] dep_dirs (match deps with `Locked deps | `Open deps -> deps)

  type t =
    { m : Memo.t;
      memo_dir : Fpath.t;
      index : B00_findex.t;
      deps : deps;
      restrictions : Fpath.t list; (* existing (dep_dirs x deps) dirs *)
      mutable cmi_by_file : Brzo_ocaml_cmi.t Fut.t Fpath.Map.t;
      mutable mod_ref_cmi : Brzo_ocaml_cmi.t list B00_ocaml.Mod_ref.Map.t;
      mutable cobjs :
        B00_ocaml.Cobj.t list Fut.t Fpath.Map.t; (* Mapped by dir. *)
      mutable mod_ref_cobj : B00_ocaml.Cobj.t list B00_ocaml.Mod_ref.Map.t; }

  let create m ~memo_dir ~dep_dirs deps =
    let m = B00.Memo.with_mark m "ocaml.mod_resolver" in
    let index =
      Log.time (fun _ msg -> msg "ocaml.mod_resolver file index") @@ fun () ->
      B00_findex.of_dirs dep_dirs
      |> Memo.fail_if_error m
    in
    let restrictions, miss = restrictions m index dep_dirs deps in
    { m; memo_dir; index; deps; restrictions;
      cmi_by_file = Fpath.Map.empty; mod_ref_cmi = B00_ocaml.Mod_ref.Map.empty;
      cobjs = Fpath.Map.empty; mod_ref_cobj = B00_ocaml.Mod_ref.Map.empty; },
    `Miss_deps miss

  let memo r = r.m
  let dep_dirs r = B00_findex.root_dirs r.index
  let deps r = r.deps
  let index r = r.index
  let dep_of_file r f =
    let dir_dep f dir = match Fpath.rem_prefix dir f with
    | None -> None | Some rel -> Some (Fpath.rem_empty_seg @@ Fpath.parent rel)
    in
    List.find_map (dir_dep f) (dep_dirs r)

  let dep_dirs_deps r =
    let find_deps_in_root_dir r root_dir =
      let parent = Fpath.parent root_dir in
      let rem_parent f = Option.get (Fpath.rem_prefix parent f) in
      let rec loop acc = function
      | [] -> List.rev_map Fpath.rem_empty_seg acc
      | d :: ds ->
          let subs = B00_findex.dir_dirs r.index d in
          let deps = List.rev_map rem_parent subs in
          loop (List.rev_append deps acc) (List.rev_append subs ds)
      in
      loop [rem_parent root_dir] [root_dir]
    in
    let root_dirs = List.concat_map (B00_findex.dir_dirs r.index) (dep_dirs r)in
    List.concat_map (find_deps_in_root_dir r) root_dirs

  let restrict_results r fs =
    let restrict pfs dir = List.assoc_opt dir pfs in
    match r.deps with
    | `Open [] -> fs
    | `Open _ ->
        let pfs = List.rev_map (fun f -> Fpath.parent f, f) fs in
        begin match List.find_map (restrict pfs) r.restrictions with
        | None -> fs
        | Some f -> [f]
        end
    | `Locked _ ->
        let pfs = List.rev_map (fun f -> Fpath.parent f, f) fs in
        begin match List.find_map (restrict pfs) r.restrictions with
        | None -> (* FIXME show hints *) []
        | Some f -> [f]
        end

  (* Lookups *)

  let cmi_obj r cmi_file = match Fpath.Map.find_opt cmi_file r.cmi_by_file with
  | Some info -> info
  | None ->
      let info, set_info = Fut.create () in
      r.cmi_by_file <- Fpath.Map.add cmi_file info r.cmi_by_file;
      begin
        ignore @@
        let* info = Brzo_ocaml_cmi.read r.m cmi_file in
        r.mod_ref_cmi <-
          B00_ocaml.Mod_ref.Map.add_to_list
            (Brzo_ocaml_cmi.mod_ref info) info r.mod_ref_cmi;
        set_info info;
        Fut.return ()
      end;
      info

  let find_cmi_side_cmx_file r cmi =
    let cmx = Fpath.(Brzo_ocaml_cmi.file cmi -+ ".cmx") in
    if not (Fpath.Set.mem cmx (B00_findex.files r.index)) then None else
    (Memo.file_ready r.m cmx; Some cmx)

  let find_cmi_files_for_mod_name r mname =
    let upper_cmi = Fmt.str "%s.cmi" mname in
    let lower_cmi = Fmt.str "%s.cmi" (String.Ascii.uncapitalize mname) in
    let find r n = B00_findex.find_filename r.index n in
    let cmi_files = List.rev_append (find r upper_cmi) (find r lower_cmi) in
    let cmi_files = restrict_results r cmi_files in
    List.iter (Memo.file_ready r.m) cmi_files; cmi_files

  let find_cmis_for_mod_name r mname =
    let cmis = find_cmi_files_for_mod_name r mname in
    Fut.of_list (List.map (cmi_obj r) cmis)

  let find_cmis_for_mod_ref r ref =
    match B00_ocaml.Mod_ref.Map.find_opt ref r.mod_ref_cmi with
    | Some cmis -> Fut.return cmis
    | None ->
        let cmi_files =
          find_cmi_files_for_mod_name r (B00_ocaml.Mod_ref.name ref)
        in
        let cmi_objs = List.map (cmi_obj r) cmi_files in
        let* infos = Fut.of_list cmi_objs in
        match B00_ocaml.Mod_ref.Map.find_opt ref r.mod_ref_cmi with
        | Some infos -> Fut.return infos
        | None ->
            Memo.fail r.m "%a: couldn't find a matching cmi: %a"
              B00_ocaml.Mod_ref.pp ref
              Fmt.(list Fpath.pp_quoted) cmi_files

  let get_cobjs_info ~ext r dir = match Fpath.Map.find_opt dir r.cobjs with
  | Some info -> info
  | None ->
      let info, set_info = Fut.create () in
      r.cobjs <- Fpath.Map.add dir info r.cobjs;
      begin
        ignore @@
        let files = B00_findex.dir_files r.index dir in
        let cobjs = List.filter (Fpath.has_ext ext) files in
        let o =
          let base = Fpath.basename dir in
          let uniq = Hash.to_hex (Memo.hash_string r.m (Fpath.to_string dir)) in
          Fpath.(r.memo_dir / Fmt.str "%s-%s%s.info" base uniq ext)
        in
        List.iter (Memo.file_ready r.m) cobjs;
        if ext = ".cmxa" then begin
          List.iter (fun o -> Memo.file_ready r.m (Fpath.set_ext ".a" o)) cobjs
        end;
        B00_ocaml.Cobj.write r.m ~cobjs ~o;
        let* cobjs = B00_ocaml.Cobj.read r.m o in
        let add_mod_ref cobj def =
          r.mod_ref_cobj <-
            B00_ocaml.Mod_ref.Map.add_to_list def cobj r.mod_ref_cobj
        in
        let add_mod_refs cobj =
          B00_ocaml.Mod_ref.Set.iter (add_mod_ref cobj)
            (B00_ocaml.Cobj.defs cobj)
        in
        List.iter add_mod_refs cobjs;
        set_info cobjs;
        Fut.return ()
      end;
      info

  let find_impl_for_mod_ref r ~ext ref =
    let amb cobjs =
      let pext = ".p" ^ ext in (* TODO doc filter out profile objects *)
      let not_pext cobj = not (Fpath.has_ext pext (B00_ocaml.Cobj.file cobj)) in
      match List.filter not_pext cobjs with
      | [cobj] -> Fut.return (Some cobj)
      | cobjs ->
          (* FIXME constraints. *)
          Memo.fail r.m "@[<v>ambiguous resolution for %a:@,%a@]"
            B00_ocaml.Mod_ref.pp ref (Fmt.list B00_ocaml.Cobj.pp) cobjs
    in
    match B00_ocaml.Mod_ref.Map.find_opt ref r.mod_ref_cobj with
    | Some [cobj] -> Fut.return (Some cobj)
    | Some cobjs -> amb cobjs
    | None ->
        Fut.bind (find_cmis_for_mod_ref r ref) @@ function
        | [] ->
            Memo.fail r.m "Could not resolve %a to a cmi file"
              B00_ocaml.Mod_ref.pp ref
        | cmis ->
            let rec loop r = function
            | cmi :: cmis ->
                let cmi_dir = Fpath.parent (Brzo_ocaml_cmi.file cmi) in
                let dirs = cmi_dir :: B00_findex.dir_dirs r.index cmi_dir in
                let rec go = function
                | [] -> loop r cmis
                | dir :: dirs ->
                    let* info = get_cobjs_info r ~ext dir in
                    match B00_ocaml.Mod_ref.Map.find_opt ref r.mod_ref_cobj with
                    | Some [cobj] -> Fut.return (Some cobj)
                    | Some cobjs -> amb cobjs
                    | None -> go dirs
                in
                go dirs
            | [] -> Fut.return None
            in
            loop r cmis

  let find_rec_impls_for_mod_refs
      ?(deps = B00_ocaml.Cobj.link_deps) r ~ext mrefs
    =
    let rec loop b cobjs defined todo =
      match B00_ocaml.Mod_ref.Set.choose_opt todo with
      | None ->
          let cobjs = B00_ocaml.Cobj.Set.elements cobjs in
          let cobjs, _ = B00_ocaml.Cobj.sort ~deps cobjs in
          Fut.return cobjs
      | Some ref ->
          let todo = B00_ocaml.Mod_ref.Set.remove ref todo in
          match B00_ocaml.Mod_ref.Set.mem ref defined with
          | true -> loop b cobjs defined todo
          | false ->
              Fut.bind (find_impl_for_mod_ref r ~ext ref) @@ function
              | None ->
                  Log.debug begin fun m ->
                    m "No resolution for %a, assuming cmi only"
                      B00_ocaml.Mod_ref.pp ref
                  end;
                  (* FIXME for toplevels this should be added to the include
                     set *)
                  let defined = B00_ocaml.Mod_ref.Set.add ref defined in
                  loop b cobjs defined todo
              | Some cobj ->
                  let cobjs = B00_ocaml.Cobj.Set.add cobj cobjs in
                  let defined =
                    B00_ocaml.Mod_ref.Set.union
                      (B00_ocaml.Cobj.defs cobj) defined
                  in
                  let new_refs =
                    B00_ocaml.Mod_ref.Set.diff (deps cobj) defined
                  in
                  let todo = B00_ocaml.Mod_ref.Set.union todo new_refs in
                  loop b cobjs defined todo
    in
    loop r B00_ocaml.Cobj.Set.empty B00_ocaml.Mod_ref.Set.empty mrefs
end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
