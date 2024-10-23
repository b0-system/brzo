(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax
open Brzo_b0_ocaml

(* Resolver helpers

   N.B. what these functions do is described in english in manual.mld
   It may help to understand these lines. *)

type ambs = [`Ambs of (B0_ocaml.Modname.t * Fpath.t list) list ]

let resolve_cmis_modrefs r cmis modrefs =
  let rec loop r cmis seen to_find =
    match B0_ocaml.Modref.Set.choose_opt to_find with
    | None -> Fut.return cmis
    | Some dep ->
        let to_find = B0_ocaml.Modref.Set.remove dep to_find in
        if B0_ocaml.Modref.Set.mem dep seen then loop r cmis seen to_find else
        let seen = B0_ocaml.Modref.Set.add dep seen in
        (* XXX For now let's see what it gives in practice to ignore here
           ambs and not found refs here. Wishfull thinking: I suspect if
           something bad happens the error will show up somewhere else in a
           meaningful way if that's a problem. *)
        Fut.bind (Mod_resolver.find_cmis_for_modref r dep) @@ function
        | cmi :: _ ->
            let to_find =
              B0_ocaml.Modref.Set.union (Brzo_ocaml_cmi.deps cmi) to_find
            in
            loop r (cmi :: cmis) seen to_find
        | [] -> loop r cmis seen to_find
  in
  let seen, to_find =
    let rec loop seen to_find = function
    | [] -> seen, B0_ocaml.Modref.Set.diff to_find seen
    | cmi :: cmis ->
        let seen = B0_ocaml.Modref.Set.add (Brzo_ocaml_cmi.modref cmi) seen in
        let to_find =
          B0_ocaml.Modref.Set.union to_find (Brzo_ocaml_cmi.deps cmi)
        in
        loop seen to_find cmis
    in
    loop B0_ocaml.Modref.Set.empty modrefs cmis
  in
  loop r cmis seen to_find

let resolution_cmis_dep_objs r ~code cmis modrefs =
  let* cmis = resolve_cmis_modrefs r cmis modrefs in
  let add_dep_objs acc cmi =
    let acc = Brzo_ocaml_cmi.file cmi :: acc in
    if code = `Byte then acc else
    match Mod_resolver.find_cmi_side_cmx_file r cmi with
    | None -> acc | Some cmx -> cmx :: acc
  in
  Fut.return (List.fold_left add_dep_objs [] cmis)

let finish_impl_resolution r ~code cmis modrefs ambs remain =
  let* objs = resolution_cmis_dep_objs r ~code cmis modrefs in
  Fut.return (objs, remain, (`Ambs ambs))

let resolve_external_impl_deps r ~code deps modrefs =
  let rec loop r changed cmis ambs resolved retry todo =
    match B0_ocaml.Modname.Set.choose_opt todo with
    | None ->
        let retry = B0_ocaml.Modname.Set.diff retry resolved in
        if B0_ocaml.Modname.Set.is_empty retry || not changed
        then (finish_impl_resolution r ~code cmis modrefs ambs retry)
        else (loop r false cmis [] resolved B0_ocaml.Modname.Set.empty retry)
    | Some dep ->
        let todo = B0_ocaml.Modname.Set.remove dep todo in
        if B0_ocaml.Modname.Set.mem dep resolved
        then loop r changed cmis ambs resolved retry todo else
        match Mod_resolver.find_cmi_files_for_modname r dep with
        | [] ->
            loop r changed cmis ambs resolved
              (B0_ocaml.Modname.Set.add dep retry) todo
        | [cmi] ->
            let* cmi_obj = Mod_resolver.cmi_obj r cmi in
            let cmi_names = Brzo_ocaml_cmi.modnames cmi_obj in
            let resolved = String.Set.union resolved cmi_names in
            loop r true (cmi_obj :: cmis) ambs resolved retry todo
        | cmi_files ->
            let ambs = (dep, cmi_files) :: ambs in
            loop r changed cmis ambs resolved
              (B0_ocaml.Modname.Set.add dep retry) todo
  in
  loop r false [] []
    B0_ocaml.Modname.Set.empty B0_ocaml.Modname.Set.empty deps

let resolve_impl_deps r ~code ~local_mods ~in_dir deps =
  let prune_cmis_modnames r deps reads =
    let rec loop b deps modrefs = function
    | [] -> Fut.return (deps, modrefs)
    | obj :: reads ->
        if not (Fpath.has_ext ".cmi" obj) then loop b deps modrefs reads else
        let* cmi_obj = Mod_resolver.cmi_obj r obj in
        let deps = String.Set.diff deps (Brzo_ocaml_cmi.modnames cmi_obj) in
        let modrefs =
          B0_ocaml.Modref.Set.union modrefs (Brzo_ocaml_cmi.deps cmi_obj)
        in
        loop b deps modrefs reads
    in
    loop r deps B0_ocaml.Modref.Set.empty reads
  in
  let local_mods, deps = B0_ocaml.Modsrc.find deps local_mods in
  let add_mod acc m = B0_ocaml.Modsrc.as_impl_dep_files ~init:acc ~code m in
  let local_objs = List.fold_left add_mod [] local_mods in
  let* deps, modrefs = prune_cmis_modnames r deps local_objs in
  let* ext_objs, deps, ambs = resolve_external_impl_deps r ~code deps modrefs in
  Fut.return (local_objs, ext_objs, deps, ambs)

let resolve_intf_deps r ~local_mods ~in_dir deps =
  (* XXX maybe we should really try to distinguish intf/impl paths.
     We are cheating for now. *)
  resolve_impl_deps r ~code:`Byte ~local_mods ~in_dir deps

(* Handling ambiguous deps *)

let handle_amb_deps r file ~unresolved (`Ambs ambs) = match ambs with
| [] -> Fut.return ()
| ambs ->
    let pp_unresolved ppf deps = (* FIXME maybe try to indicate installs *)
      let deps = B0_ocaml.Modname.Set.elements deps in
      if deps = [] then () else
      let mod_txt = match deps with [_] -> "Module" | _ -> "Modules" in
      Fmt.pf ppf
        "@[%s %a@ could@ not@ be@ resolved.@ \
         Maybe@ due@ to@ the@ following@ ambiguous@ resolutions.@]@."
        mod_txt (Fmt.list ~sep:Fmt.sp B0_ocaml.Modname.pp) deps
    in
    let pp_alt ppf cmi =
      let dep = Option.get (Mod_resolver.dep_of_file r cmi) in
      let dep = Fpath.to_string dep in
      Fmt.code ppf
        (String.concat " " ["brzo"; "file"; "set"; "ocaml.deps.v[0]"; dep])
    in
    let pp_amb ppf (dep, cmis) =
      Fmt.pf ppf
        "@[<v>Module %a has multiple dependency resolutions.@,\
         Try one of the following dependency restrictions:@,@,@[<v>%a@]@,@]"
        B0_ocaml.Modname.pp dep Fmt.(list pp_alt) cmis
    in
    B0_memo.fail (Mod_resolver.memo r)
      "@[<v>File %a:@,%a%a@]" Fpath.pp_unquoted file
      pp_unresolved unresolved
      Fmt.(list pp_amb) ambs

(* Handling missing user specified dependencies. *)

let root_dep dep =
  match String.cut_left ~sep:Fpath.dir_sep (Fpath.to_string dep) with
  | None -> dep | Some (root, _) -> Fpath.v root

let pp_miss_deps_help ppf (suggest, opam) =
  let pp_dep ppf s = Fmt.code ppf s in
  let pp_opam ppf deps =
    if deps = [] then () else
    let pp_cmd ppf l = Fmt.code ppf (String.concat " " l) in
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
        let dict yield = List.iter yield all_deps in
        match String.spellcheck dict dep with
        | [] ->
            if not root_in_resolver
            then `Suggest (dep, String.spellcheck dict root) else
            let starts_with f = String.starts_with ~prefix:root f in
            `Suggest (dep, List.filter starts_with all_deps)
        | suggest -> `Suggest (dep, suggest)
  in
  let resolver_deps = Mod_resolver.dep_dirs_deps r in
  let resolver_deps = List.rev_map Fpath.to_string resolver_deps in
  let all_deps = String.distinct (List.rev_append opam_pkgs resolver_deps) in
  let rec loop correct opam = function
  | [] -> correct, opam
  | dep :: deps ->
      match miss_help r opam_pkgs resolver_deps all_deps dep with
      | `Opam o -> loop correct (o :: opam) deps
      | `Suggest c -> loop (c :: correct) opam deps
  in
  loop [] [] miss_deps

let handle_miss_user_deps r (`Miss_deps miss_deps) = match miss_deps with
| [] -> Fut.return ()
| miss_deps ->
    let m = Mod_resolver.memo r in
    let* pkgs = Brzo_b0_opam.if_exists m (Brzo_b0_opam.list m `Available) in
    let opam_pkgs = match pkgs with None -> [] | Some pkgs -> pkgs in
    let help = get_miss_deps_help r opam_pkgs miss_deps in
    B0_memo.fail (Mod_resolver.memo r) "%a" pp_miss_deps_help help

(* Suggesting alternatives for unresolved *)

let suggest_pkgs_for_modname m ~pkgs =
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
  let find_fuzzy m pkgs = (* XXX shouldn't we simply adapt to size ? *)
    let rec loop ~max_dist m pkgs =
      match String.spellcheck ~max_dist (fun yield -> List.iter yield pkgs) m
      with
      | [] ->
          begin match String.cut_right ~sep:"_" m with
          | Some (m, _) when m <> "" ->
              if List.mem m installed_pkgs then `None else
              let max_dist = Fun.const 1 (* be less forgiving for prefixes *) in
              loop ~max_dist m pkgs
          | _ -> `None
          end
      | pkgs -> `Fuzzy_prefix_match (return pkgs)
    in
    loop ~max_dist:(Fun.const 2) m pkgs
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
