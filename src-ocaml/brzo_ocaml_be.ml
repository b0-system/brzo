(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00
open B00_ocaml
open Brzo_b0_ocaml

(* Resolver helpers

   N.B. what these functions do is described in english in manual.mld
   It may help to understand these lines. *)

type ambs = [`Ambs of (Mod_name.t * Fpath.t list) list ]

let resolve_cmis_mod_refs r cmis mod_refs k =
  let rec loop r cmis seen to_find = match Mod_ref.Set.choose to_find with
  | exception Not_found -> k cmis
  | dep ->
      let to_find = Mod_ref.Set.remove dep to_find in
      if Mod_ref.Set.mem dep seen then loop r cmis seen to_find else
      let seen = Mod_ref.Set.add dep seen in
      (* XXX For now let's see what it gives in practice to ignore here
         ambs and not found refs here. Wishfull thinking: I suspect if
         something bad happens the error will show up somewhere else in a
         meaningful way if that's a problem. *)
      Mod_resolver.find_cmis_for_mod_ref r dep @@ function
      | cmi :: _ ->
          let to_find = Mod_ref.Set.union (Brzo_ocaml_cmi.deps cmi) to_find in
          loop r (cmi :: cmis) seen to_find
      | [] -> loop r cmis seen to_find
  in
  let seen, to_find =
    let rec loop seen to_find = function
    | [] -> seen, Mod_ref.Set.diff to_find seen
    | cmi :: cmis ->
        let seen = Mod_ref.Set.add (Brzo_ocaml_cmi.mod_ref cmi) seen in
        let to_find = Mod_ref.Set.union to_find (Brzo_ocaml_cmi.deps cmi) in
        loop seen to_find cmis
    in
    loop Mod_ref.Set.empty mod_refs cmis
  in
  loop r cmis seen to_find

let resolution_cmis_dep_objs r ~code cmis mod_refs k =
  resolve_cmis_mod_refs r cmis mod_refs @@ fun cmis ->
  let add_dep_objs acc cmi =
    let acc = Brzo_ocaml_cmi.file cmi :: acc in
    if code = Cobj.Byte then acc else
    match Mod_resolver.find_cmi_side_cmx_file r cmi with
    | None -> acc | Some cmx -> cmx :: acc
  in
  k (List.fold_left add_dep_objs [] cmis)

let finish_impl_resolution r ~code cmis mod_refs ambs remain k =
  resolution_cmis_dep_objs r ~code cmis mod_refs @@
  fun objs -> k (objs, remain, (`Ambs ambs))

let resolve_external_impl_deps r ~code deps mod_refs k =
  let rec loop r changed cmis ambs resolved retry todo =
    match Mod_name.Set.choose todo with
    | exception Not_found ->
        let retry = Mod_name.Set.diff retry resolved in
        if Mod_name.Set.is_empty retry || not changed
        then (finish_impl_resolution r ~code cmis mod_refs ambs retry k)
        else (loop r false cmis [] resolved Mod_name.Set.empty retry)
    | dep ->
        let todo = Mod_name.Set.remove dep todo in
        if Mod_name.Set.mem dep resolved
        then loop r changed cmis ambs resolved retry todo else
        match Mod_resolver.find_cmi_files_for_mod_name r dep with
        | [] ->
            loop r changed cmis ambs resolved (Mod_name.Set.add dep retry) todo
        | [cmi] ->
            Memo.Fut.await (Mod_resolver.cmi_obj r cmi) @@ fun cmi_obj ->
            let cmi_names = Brzo_ocaml_cmi.mod_names cmi_obj in
            let resolved = String.Set.union resolved cmi_names in
            loop r true (cmi_obj :: cmis) ambs resolved retry todo
        | cmi_files ->
            let ambs = (dep, cmi_files) :: ambs in
            loop r changed cmis ambs resolved (Mod_name.Set.add dep retry) todo
  in
  loop r false [] [] Mod_name.Set.empty Mod_name.Set.empty deps

let resolve_impl_deps r ~code ~local_mods ~in_dir deps k =
  let prune_cmis_mod_names r deps reads k =
    let rec loop b deps mod_refs = function
    | [] -> k (deps, mod_refs)
    | obj :: reads ->
        if not (Fpath.has_ext ".cmi" obj) then loop b deps mod_refs reads else
        Memo.Fut.await (Mod_resolver.cmi_obj r obj) @@ fun cmi_obj ->
        let deps = String.Set.diff deps (Brzo_ocaml_cmi.mod_names cmi_obj) in
        let mod_refs = Mod_ref.Set.union mod_refs (Brzo_ocaml_cmi.deps cmi_obj) in
        loop b deps mod_refs reads
    in
    loop r deps Mod_ref.Set.empty reads
  in
  let local_mods, deps = Mod_src.find_local_deps local_mods deps in
  let add_mod acc m = Mod_src.as_impl_dep_files ~init:acc ~code ~in_dir m in
  let local_objs = List.fold_left add_mod [] local_mods in
  prune_cmis_mod_names r deps local_objs @@ fun (deps, mod_refs) ->
  resolve_external_impl_deps r ~code deps mod_refs @@
  fun (ext_objs, deps, ambs) -> k (local_objs, ext_objs, deps, ambs)

let resolve_intf_deps r ~local_mods ~in_dir deps k =
  (* XXX maybe we should really try to distinguish intf/impl paths.
     We are cheating for now. *)
  resolve_impl_deps r ~code:Cobj.Byte ~local_mods ~in_dir deps k

(* Handling ambiguous deps *)

let handle_amb_deps r file ~unresolved (`Ambs ambs) k = match ambs with
| [] -> k ()
| ambs ->
    let pp_unresolved ppf deps = (* FIXME maybe try to indicate installs *)
      let deps = Mod_name.Set.elements deps in
      if deps = [] then () else
      let mod_txt = match deps with [_] -> "Module" | _ -> "Modules" in
      Fmt.pf ppf
        "@[%s %a@ could@ not@ be@ resolved.@ \
         Maybe@ due@ to@ the@ following@ ambiguous@ resolutions.@]@."
        mod_txt (Fmt.list ~sep:Fmt.sp Mod_name.pp) deps
    in
    let pp_alt ppf cmi =
      let dep = Option.get (Mod_resolver.dep_of_file r cmi) in
      let dep = Fpath.to_string dep in
      Fmt.(code string) ppf
        (String.concat " " ["brzo"; "file"; "set"; "ocaml.deps.v[0]"; dep])
    in
    let pp_amb ppf (dep, cmis) =
      Fmt.pf ppf
        "@[<v>Module %a has multiple dependency resolutions.@,\
         Try one of the following dependency restrictions:@,@,@[<v>%a@]@,@]"
        Mod_name.pp dep Fmt.(list pp_alt) cmis
    in
    Memo.fail (Mod_resolver.memo r)
      "@[<v>File %a:@,%a%a@]" Fpath.pp_unquoted file
      pp_unresolved unresolved
      Fmt.(list pp_amb) ambs

(* Handling missing user specified dependencies. *)

let root_dep dep =
  match String.cut_left ~sep:Fpath.dir_sep (Fpath.to_string dep) with
  | None -> dep | Some (root, _) -> Fpath.v root

let pp_miss_deps_help ppf (suggest, opam) =
  let pp_dep ppf s = Fmt.(code string) ppf s in
  let pp_opam ppf deps =
    if deps = [] then () else
    let pp_cmd ppf l = Fmt.(code string) ppf (String.concat " " l) in
    let pp_dep ppf (root, dep) = match String.equal root dep with
    | true -> Fmt.pf ppf "%a" pp_dep root
    | false -> Fmt.pf ppf "%a (%a)" pp_dep root pp_dep dep
    in
    let dep_txt = match deps with [d] -> "Dependency" | _ -> "Dependencies" in
    let roots = List.map fst deps in
    Fmt.pf ppf "@[<v>@[%s@ %a@ not found.@]@,Try %a@]"
      dep_txt Fmt.(list ~sep:comma pp_dep)
      deps pp_cmd ("opam" :: "install" :: roots)
  in
  let pp_suggest ppf suggest =
    if suggest = [] then () else
    let kind = Fmt.any "dependency" in
    let pp_unknown = Fmt.unknown' ~kind pp_dep ~hint:Fmt.did_you_mean in
    Fmt.pf ppf "@[<v>%a@]" (Fmt.list (Fmt.box pp_unknown)) suggest
  in
  let sep = if suggest <> [] && opam <> [] then Fmt.cut else Fmt.nop in
  Fmt.pf ppf "@[<v>%a%a%a@]" pp_suggest suggest sep () pp_opam opam

let get_miss_deps_help r opam_pkgs miss_deps =
  let miss_help r opam_pkgs resolver_deps all_deps dep =
    let root = Fpath.to_string (root_dep dep) in
    let dep = Fpath.to_string dep in
    let root_in_resolver = List.mem root resolver_deps in
    let root_in_opam = List.mem root opam_pkgs in
    match root_in_opam && not root_in_resolver with
    | true -> `Opam (root, dep)
    | false ->
        match String.suggest all_deps dep with
        | [] ->
            if not root_in_resolver
            then `Suggest (dep, String.suggest all_deps root) else
            let is_prefix f = String.is_prefix root f in
            `Suggest (dep, List.filter is_prefix all_deps)
        | suggest -> `Suggest (dep, suggest)
  in
  let resolver_deps = Mod_resolver.dep_dirs_deps r in
  let resolver_deps = List.rev_map Fpath.to_string resolver_deps in
  let all_deps = String.uniquify (List.rev_append opam_pkgs resolver_deps) in
  let rec loop correct opam = function
  | [] -> correct, opam
  | dep :: deps ->
      match miss_help r opam_pkgs resolver_deps all_deps dep with
      | `Opam o -> loop correct (o :: opam) deps
      | `Suggest c -> loop (c :: correct) opam deps
  in
  loop [] [] miss_deps

let handle_miss_user_deps r (`Miss_deps miss_deps) k = match miss_deps with
| [] -> k ()
| miss_deps ->
    let m = Mod_resolver.memo r in
    Brzo_b0_opam.if_exists m (Brzo_b0_opam.list m `Available) @@ fun pkgs ->
    let opam_pkgs = match pkgs with None -> [] | Some pkgs -> pkgs in
    let help = get_miss_deps_help r opam_pkgs miss_deps in
    Memo.fail (Mod_resolver.memo r) "%a" pp_miss_deps_help help

(* Suggesting alternatives for unresolved *)

let suggest_pkgs_for_mod_name m ~pkgs =
  let normalize_char = function '-' -> '_' | c -> Char.Ascii.lowercase c in
  let normalize_id id = String.map normalize_char id in
  let rec normalize_pkgs ipkgs upkgs real_names = function
  | [] -> ipkgs, upkgs, real_names
  | (p, version) :: ps ->
      let np = normalize_id p in
      let ipkgs, upkgs = match version with
      | None -> ipkgs, np :: upkgs
      | Some _ -> np :: ipkgs, upkgs
      in
      normalize_pkgs ipkgs upkgs (String.Map.add np p real_names) ps
  in
  let installed_pkgs, uninstalled_pkgs, real_names =
    normalize_pkgs [] [] String.Map.empty pkgs
  in
  let get_real_name n = match String.Map.find n real_names with
  | exception Not_found -> assert false | n -> n
  in
  let return ns = List.sort compare (List.rev_map get_real_name ns) in
  let find_exact m pkgs =
    let rec loop m pkgs = match List.mem m pkgs with
    | true -> `Prefix_match (get_real_name m)
    | false ->
        match String.cut_right ~sep:"_" m with
        | Some (m, _) when m <> "" -> loop m pkgs
        | _ -> `None
    in
    loop m pkgs
  in
  let find_fuzzy m pkgs =
    let rec loop ~dist m pkgs = match String.suggest ~dist pkgs m with
    | [] ->
        begin match String.cut_right ~sep:"_" m with
        | Some (m, _) when m <> "" ->
            if List.mem m installed_pkgs then `None else
            loop ~dist:1 (* be less forgiving for prefixes *) m pkgs
        | _ -> `None
        end
    | pkgs -> `Fuzzy_prefix_match (return pkgs)
    in
    loop ~dist:2 m pkgs
  in
  let m = normalize_id m in
  match find_exact m uninstalled_pkgs with
  | `None -> find_fuzzy m uninstalled_pkgs
  | v -> v

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers

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