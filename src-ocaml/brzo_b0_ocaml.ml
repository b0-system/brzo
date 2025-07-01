(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax

module Mod_resolver = struct
  type dep = Fpath.t
  type deps = [ `Open of dep list | `Locked of dep list ]
  let unrestricted = `Open []
  let restrictions m index dep_dirs deps =
    let rec add_dep added acc miss dep = function
    | dir :: dirs ->
        let d = Fpath.(ensure_trailing_dir_sep @@ dir // dep) in
        if Fpath.Set.mem d (B0_findex.dirs index)
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
    { m : B0_memo.t;
      memo_dir : Fpath.t;
      index : B0_findex.t;
      deps : deps;
      restrictions : Fpath.t list; (* existing (dep_dirs x deps) dirs *)
      mutable cmi_by_file : Brzo_ocaml_cmi.t Fut.t Fpath.Map.t;
      mutable modref_cmi : Brzo_ocaml_cmi.t list B0_ocaml.Modref.Map.t;
      mutable cobjs :
        B0_ocaml.Cobj.t list Fut.t Fpath.Map.t; (* Mapped by dir. *)
      mutable modref_cobj : B0_ocaml.Cobj.t list B0_ocaml.Modref.Map.t; }

  let create m ~memo_dir ~dep_dirs deps =
    let m = B0_memo.with_mark m "ocaml.mod_resolver" in
    let index =
      Log.time (fun _ msg -> msg "ocaml.mod_resolver file index") @@ fun () ->
      B0_findex.of_dirs dep_dirs
      |> B0_memo.fail_if_error m
    in
    let restrictions, miss = restrictions m index dep_dirs deps in
    { m; memo_dir; index; deps; restrictions;
      cmi_by_file = Fpath.Map.empty; modref_cmi = B0_ocaml.Modref.Map.empty;
      cobjs = Fpath.Map.empty; modref_cobj = B0_ocaml.Modref.Map.empty; },
    `Miss_deps miss

  let memo r = r.m
  let dep_dirs r = B0_findex.root_dirs r.index
  let deps r = r.deps
  let index r = r.index
  let dep_of_file r f =
    let dir_dep f dir = match Fpath.strip_prefix dir f with
    | None -> None
    | Some rel -> Some (Fpath.strip_trailing_dir_sep @@ Fpath.parent rel)
    in
    List.find_map (dir_dep f) (dep_dirs r)

  let dep_dirs_deps r =
    let find_deps_in_root_dir r root_dir =
      let parent = Fpath.parent root_dir in
      let rem_parent f = Option.get (Fpath.strip_prefix parent f) in
      let rec loop acc = function
      | [] -> List.rev_map Fpath.strip_trailing_dir_sep acc
      | d :: ds ->
          let subs = B0_findex.dir_dirs r.index d in
          let deps = List.rev_map rem_parent subs in
          loop (List.rev_append deps acc) (List.rev_append subs ds)
      in
      loop [rem_parent root_dir] [root_dir]
    in
    let root_dirs = List.concat_map (B0_findex.dir_dirs r.index) (dep_dirs r)in
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
      let info, set_info = Fut.make () in
      r.cmi_by_file <- Fpath.Map.add cmi_file info r.cmi_by_file;
      begin
        ignore @@
        let* info = Brzo_ocaml_cmi.read r.m cmi_file in
        r.modref_cmi <-
          B0_ocaml.Modref.Map.add_to_list
            (Brzo_ocaml_cmi.modref info) info r.modref_cmi;
        set_info info;
        Fut.return ()
      end;
      info

  let find_cmi_side_cmx_file r cmi =
    let cmx = Fpath.(Brzo_ocaml_cmi.file cmi -+ ".cmx") in
    if not (Fpath.Set.mem cmx (B0_findex.files r.index)) then None else
    (B0_memo.ready_file r.m cmx; Some cmx)

  let find_cmi_files_for_modname r mname =
    let upper_cmi = Fmt.str "%s.cmi" mname in
    let lower_cmi = Fmt.str "%s.cmi" (String.Ascii.uncapitalize mname) in
    let find r n = B0_findex.find_filename r.index n in
    let cmi_files = List.rev_append (find r upper_cmi) (find r lower_cmi) in
    let cmi_files = restrict_results r cmi_files in
    B0_memo.ready_files r.m cmi_files; cmi_files

  let find_cmis_for_modname r mname =
    let cmis = find_cmi_files_for_modname r mname in
    Fut.of_list (List.map (cmi_obj r) cmis)

  let find_cmis_for_modref r ref =
    match B0_ocaml.Modref.Map.find_opt ref r.modref_cmi with
    | Some cmis -> Fut.return cmis
    | None ->
        let cmi_files =
          find_cmi_files_for_modname r (B0_ocaml.Modref.name ref)
        in
        let cmi_objs = List.map (cmi_obj r) cmi_files in
        let* infos = Fut.of_list cmi_objs in
        match B0_ocaml.Modref.Map.find_opt ref r.modref_cmi with
        | Some infos -> Fut.return infos
        | None ->
            B0_memo.fail r.m "%a: couldn't find a matching cmi: %a"
              B0_ocaml.Modref.pp ref
              Fmt.(list Fpath.pp_quoted) cmi_files

  let get_cobjs_info ~ext r dir = match Fpath.Map.find_opt dir r.cobjs with
  | Some info -> info
  | None ->
      let info, set_info = Fut.make () in
      r.cobjs <- Fpath.Map.add dir info r.cobjs;
      begin
        ignore @@
        let files = B0_findex.dir_files r.index dir in
        let cobjs = List.filter (Fpath.has_ext ext) files in
        let o =
          let base = Fpath.basename dir in
          let uniq =
            B0_hash.to_hex (B0_memo.hash_string r.m (Fpath.to_string dir))
          in
          Fpath.(r.memo_dir / Fmt.str "%s-%s%s.info" base uniq ext)
        in
        B0_memo.ready_files r.m cobjs;
        if ext = ".cmxa" then begin
          List.iter
            (fun o ->
               B0_memo.ready_file r.m (Fpath.set_ext ~multi:false ".a" o)) cobjs
        end;
        B0_ocaml.Cobj.write r.m ~cobjs ~o;
        let* cobjs = B0_ocaml.Cobj.read r.m o in
        let add_modref cobj def =
          r.modref_cobj <-
            B0_ocaml.Modref.Map.add_to_list def cobj r.modref_cobj
        in
        let add_modrefs cobj =
          B0_ocaml.Modref.Set.iter (add_modref cobj)
            (B0_ocaml.Cobj.defs cobj)
        in
        List.iter add_modrefs cobjs;
        set_info cobjs;
        Fut.return ()
      end;
      info

  let find_impl_for_modref r ~ext ref =
    let amb cobjs =
      let pext = ".p" ^ ext in (* TODO doc filter out profile objects *)
      let not_pext cobj = not (Fpath.has_ext pext (B0_ocaml.Cobj.file cobj)) in
      match List.filter not_pext cobjs with
      | [cobj] -> Fut.return (Some cobj)
      | cobjs ->
          (* FIXME constraints. *)
          B0_memo.fail r.m "@[<v>ambiguous resolution for %a:@,%a@]"
            B0_ocaml.Modref.pp ref (Fmt.list B0_ocaml.Cobj.pp) cobjs
    in
    match B0_ocaml.Modref.Map.find_opt ref r.modref_cobj with
    | Some [cobj] -> Fut.return (Some cobj)
    | Some cobjs -> amb cobjs
    | None ->
        Fut.bind (find_cmis_for_modref r ref) @@ function
        | [] ->
            B0_memo.fail r.m "Could not resolve %a to a cmi file"
              B0_ocaml.Modref.pp ref
        | cmis ->
            let rec loop r = function
            | cmi :: cmis ->
                let cmi_dir = Fpath.parent (Brzo_ocaml_cmi.file cmi) in
                let dirs = cmi_dir :: B0_findex.dir_dirs r.index cmi_dir in
                let rec go = function
                | [] -> loop r cmis
                | dir :: dirs ->
                    let* info = get_cobjs_info r ~ext dir in
                    match B0_ocaml.Modref.Map.find_opt ref r.modref_cobj with
                    | Some [cobj] -> Fut.return (Some cobj)
                    | Some cobjs -> amb cobjs
                    | None -> go dirs
                in
                go dirs
            | [] -> Fut.return None
            in
            loop r cmis

  let find_rec_impls_for_modrefs
      ?(deps = B0_ocaml.Cobj.link_deps) r ~ext mrefs
    =
    let rec loop b cobjs defined todo =
      match B0_ocaml.Modref.Set.choose_opt todo with
      | None ->
          let cobjs = B0_ocaml.Cobj.Set.elements cobjs in
          let cobjs, _ = B0_ocaml.Cobj.sort ~deps cobjs in
          Fut.return cobjs
      | Some ref ->
          let todo = B0_ocaml.Modref.Set.remove ref todo in
          match B0_ocaml.Modref.Set.mem ref defined with
          | true -> loop b cobjs defined todo
          | false ->
              Fut.bind (find_impl_for_modref r ~ext ref) @@ function
              | None ->
                  Log.debug begin fun m ->
                    m "No resolution for %a, assuming cmi only"
                      B0_ocaml.Modref.pp ref
                  end;
                  (* FIXME for toplevels this should be added to the include
                     set *)
                  let defined = B0_ocaml.Modref.Set.add ref defined in
                  loop b cobjs defined todo
              | Some cobj ->
                  let cobjs = B0_ocaml.Cobj.Set.add cobj cobjs in
                  let defined =
                    B0_ocaml.Modref.Set.union
                      (B0_ocaml.Cobj.defs cobj) defined
                  in
                  let new_refs =
                    B0_ocaml.Modref.Set.diff (deps cobj) defined
                  in
                  let todo = B0_ocaml.Modref.Set.union todo new_refs in
                  loop b cobjs defined todo
    in
    loop r B0_ocaml.Cobj.Set.empty B0_ocaml.Modref.Set.empty mrefs
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
