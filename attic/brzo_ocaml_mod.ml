(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type path = string list

(* XXX Confession: most of this was cargo-culted from the depend.ml of
   the compiler. *)

open Parsetree
open Location
open Longident

let rec deps_path ~is_open b f = function
| Lident s -> if String.Set.mem s b then f else (is_open, s) :: f
| Ldot (lid, _) -> deps_path ~is_open b f lid
| Lapply (i0, i1) -> deps_path ~is_open b (deps_path ~is_open b f i0) i1

let deps_lid b f lid = match lid.txt with
| Ldot (p, _) -> deps_path ~is_open:false b f p
| _ -> f

let rec deps_signature b f = function
| [] -> (b, f)
| i :: is -> let b, f = deps_signature_item b f i in deps_signature b f is

and deps_signature_item b f i = match i.psig_desc with
| Psig_value vd -> b, deps_type b f vd.pval_type
| Psig_type (_, ds) -> b, List.fold_left (deps_type_declaration b) f ds
| Psig_typext te -> b, deps_type_extension b f te
| Psig_exception pext -> b, deps_extension_constructor b f pext
| Psig_module m -> String.Set.add m.pmd_name.txt b, deps_modtype b f m.pmd_type
| Psig_recmodule ds ->
    let b' =
      let mnames = List.map (fun m -> m.pmd_name.txt) ds in
      List.fold_right String.Set.add mnames b
    in
    b', List.fold_left (deps_modtype b') f (List.map (fun m -> m.pmd_type) ds)
| Psig_modtype x ->
    b, (match x.pmtd_type with None -> f | Some t -> deps_modtype b f t)
| Psig_open od -> b, deps_path ~is_open:true b f od.popen_lid.txt
| Psig_include incl -> b, deps_modtype b f incl.pincl_mod
| Psig_class cs -> b, List.fold_left (deps_class_description b) f cs
| Psig_class_type cts ->
    b, List.fold_left (deps_class_type_declaration b) f cts
| Psig_attribute _ | Psig_extension _ -> b, f

and deps_types b f = function
| [] -> f
| t :: ts -> deps_types b (deps_type b f t) ts

and deps_type b f t = match t.ptyp_desc with
| Ptyp_any -> f
| Ptyp_var _ -> f
| Ptyp_arrow (_, t1, t2) -> deps_types b f [t1; t2]
| Ptyp_tuple ts -> deps_types b f ts
| Ptyp_constr (lid, ts) -> deps_types b (deps_lid b f lid) ts
| Ptyp_object (fs, _) ->
    let rec loop b f = function
    | [] -> f
    | Otag (_, _, t) :: ts | Oinherit t :: ts -> loop b (deps_type b f t) fs
    in
    loop b f fs
| Ptyp_class (lid, ts) -> deps_types b (deps_lid b f lid) ts
| Ptyp_alias (t, _) -> deps_type b f t
| Ptyp_variant (fs, _, _) ->
    let rec loop b f = function
    | [] -> f
    | Rtag (_, _, _, stl) :: fs -> loop b (deps_types b f stl) fs
    | Rinherit sty :: fs -> loop b (deps_type b f sty) fs
    in
    loop b f fs
| Ptyp_poly(_, t) -> deps_type b f t
| Ptyp_package (lid, l) ->
    deps_types b (deps_lid b f lid) (List.map (fun (_, e) -> e) l)
| Ptyp_extension _ -> f

and deps_type_declaration b f td =
  let add acc (t0, t1, _) = t1 :: t0 :: acc in
  let ts = List.rev (List.fold_left add [] td.ptype_cstrs) in
  let f = deps_types b f ts in
  let f = match td.ptype_manifest with None -> f | Some t -> deps_type b f t in
  match td.ptype_kind with
  | Ptype_abstract -> f
  | Ptype_variant cs ->
      let rec loop b f = function
      | [] -> f
      | c :: cs ->
          let f = deps_constructor_arguments b f c.pcd_args in
          let f = match c.pcd_res with None -> f | Some t -> deps_type b f t in
          loop b f cs
      in
      loop b f cs
  | Ptype_record ls -> deps_types b f (List.map (fun p -> p.pld_type) ls)
  | Ptype_open -> f

and deps_class_description b f c = deps_class_type b f c.pci_expr
and deps_class_type_declaration b f i = deps_class_type b f i.pci_expr
and deps_class_type b f ct = match ct.pcty_desc with
| Pcty_constr (lid, ts) -> deps_types b (deps_lid b f lid) ts
| Pcty_signature s ->
    List.fold_left (deps_class_type_field b) (deps_type b f s.pcsig_self)
      s.pcsig_fields
| Pcty_arrow (_, t, ct) -> deps_class_type b (deps_type b f t) ct
| Pcty_extension e -> f
| Pcty_open (_ovf, m, e) ->
    let f = deps_path ~is_open:true (* local *) b f m.txt in
    deps_class_type b f e


and deps_class_type_field b f pctf = match pctf.pctf_desc with
| Pctf_inherit cty -> deps_class_type b f cty
| Pctf_val(_, _, _, t) -> deps_type b f t
| Pctf_method(_, _, _, t) -> deps_type b f t
| Pctf_constraint(t0, t1) -> deps_type b (deps_type b f t0) t1
| Pctf_attribute _ -> f
| Pctf_extension _ -> f

and deps_type_extension b f te =
  List.fold_left (deps_extension_constructor b)
    (deps_lid b f te.ptyext_path) te.ptyext_constructors

and deps_extension_constructor b f pext = match pext.pext_kind with
| Pext_decl (args, rty) ->
    let f = deps_constructor_arguments b f args in
    (match rty with None -> f | Some t -> deps_type b f t)
| Pext_rebind lid -> deps_lid b f lid

and deps_constructor_arguments b f = function
| Pcstr_tuple l -> deps_types b f l
| Pcstr_record l -> deps_types b f (List.map (fun l -> l.pld_type) l)

and deps_modtype b f mt = match mt.pmty_desc with
| Pmty_ident lid -> deps_lid b f lid
| Pmty_alias lid -> deps_path ~is_open:false b f lid.txt
| Pmty_signature s -> snd (deps_signature b f s)
| Pmty_functor (id, t0, t1) ->
    let f = match t0 with None -> f | Some t -> deps_modtype b f t in
    deps_modtype (String.Set.add id.txt b) f t1
| Pmty_with(t, cstrl) ->
    let f = deps_modtype b f t in
    let add f = function
    | Pwith_type (_, td) -> deps_type_declaration b f td
    | Pwith_module (_, lid) -> deps_path ~is_open:false b f lid.txt
    | Pwith_typesubst (_, td) -> deps_type_declaration b f td
    | Pwith_modsubst (_, lid) -> deps_path ~is_open:false b f lid.txt
    in
    List.fold_left add f cstrl
| Pmty_typeof m -> deps_module b f m
| Pmty_extension _ -> f

and deps_module b f m =  match m.pmod_desc with
| Pmod_ident lid -> deps_path ~is_open:false b f lid.txt
| Pmod_structure s -> snd (deps_structure b f s)
| Pmod_functor(id, t, m) ->
    let f = match t with None -> f | Some t -> deps_modtype b f t in
    deps_module (String.Set.add id.txt b) f m
| Pmod_apply(m1, m2) -> deps_module b (deps_module b f m1) m2
| Pmod_constraint(m, t) -> deps_modtype b (deps_module b f m) t
| Pmod_unpack e -> deps_expr b f e
| Pmod_extension _ -> f

and deps_structure b f = function
| [] -> b, f
| i :: is -> let b, f = deps_structure_item b f i in deps_structure b f is

and deps_expr b f e = match e.pexp_desc with
| Pexp_ident lid -> deps_lid b f lid
| Pexp_constant _ -> f
| Pexp_let (rf, pel, e) ->
    let b, f = deps_bindings rf b f pel in
    deps_expr b f e
| Pexp_fun (_, opte, p, e) ->
    let f = match opte with None -> f | Some e -> deps_expr b f e in
    let b, f = deps_pattern b f p in
    deps_expr b f e
| Pexp_function pel -> List.fold_left (deps_case b) f pel
| Pexp_apply (e, es) ->
    let f = deps_expr b f e in
    List.fold_left (fun f (_, e) -> deps_expr b f e) f es
| Pexp_match (e, pel) -> List.fold_left (deps_case b) (deps_expr b f e) pel
| Pexp_try (e, pel) -> List.fold_left (deps_case b) (deps_expr b f e) pel
| Pexp_tuple es -> List.fold_left (deps_expr b) f es
| Pexp_construct (c, opte) ->
    let f = deps_lid b f c in
    (match opte with None -> f | Some e -> deps_expr b f e)
| Pexp_variant (_, opte) ->
    (match opte with None -> f | Some e -> deps_expr b f e)
| Pexp_record (lblel, opte) ->
    let add f (lbl, e) = deps_expr b (deps_lid b f lbl) e in
    let f = List.fold_left add f lblel in
    (match opte with None -> f | Some e -> deps_expr b f e)
| Pexp_field (e, fld) ->  deps_lid b (deps_expr b f e) fld
| Pexp_setfield (e1, fld, e2) ->
    deps_expr b (deps_lid b (deps_expr b f e1) fld) e2
| Pexp_array el -> List.fold_left (deps_expr b) f el
| Pexp_ifthenelse (e1, e2, o) ->
    let f = deps_expr b (deps_expr b f e1) e2 in
    (match o with None -> f | Some e -> deps_expr b f e)
| Pexp_sequence (e1, e2) ->  deps_expr b (deps_expr b f e1) e2
| Pexp_while (e1, e2) -> deps_expr b (deps_expr b f e1) e2
| Pexp_for ( _, e1, e2, _, e3) ->
    deps_expr b (deps_expr b (deps_expr b f e1) e2) e3
| Pexp_coerce (e, t0, t1) ->
    let f = deps_expr b f e in
    let f = match t0 with None -> f | Some t -> deps_type b f t in
    deps_type b f t1
| Pexp_constraint (e, t) -> deps_type b (deps_expr b f e) t
| Pexp_send (e, _) -> deps_expr b f e
| Pexp_new lid -> deps_lid b f lid
| Pexp_setinstvar (_, e) -> deps_expr b f e
| Pexp_override sel ->
    List.fold_left (deps_expr b) f (List.map (fun (_, e) -> e) sel)
| Pexp_letmodule (id, m, e) ->
    deps_expr (String.Set.add id.txt b) (deps_module b f m) e
| Pexp_letexception (_, e) -> deps_expr b f e
| Pexp_assert e -> deps_expr b f e
| Pexp_lazy e -> deps_expr b f e
| Pexp_poly (e, t) ->
    let f = deps_expr b f e in
    (match t with None -> f | Some t -> deps_type b f t)
| Pexp_object o ->
    let b, f = deps_pattern b f o.pcstr_self in
    List.fold_left (deps_class_field b) f o.pcstr_fields
| Pexp_newtype (_, e) -> deps_expr b f e
| Pexp_pack m -> deps_module b f m
| Pexp_open (_ovf, m, e) ->
    let f = deps_path ~is_open:true (* local *) b f m.txt in
    deps_expr b f e
| Pexp_extension _ -> f
| Pexp_unreachable -> f

and deps_structure_item b f i = match i.pstr_desc with
| Pstr_eval (e, _) -> b, deps_expr b f e
| Pstr_value (rf, pel) -> deps_bindings rf b f pel
| Pstr_primitive vd -> b, deps_type b f vd.pval_type
| Pstr_type (_, ds) -> b, List.fold_left (deps_type_declaration b) f ds
| Pstr_typext te -> b, deps_type_extension b f te
| Pstr_exception pext -> b, deps_extension_constructor b f pext
| Pstr_module x -> String.Set.add x.pmb_name.txt b, deps_module b f x.pmb_expr
| Pstr_recmodule bs ->
    let b =
      let add acc m = String.Set.add m.pmb_name.txt acc in
      List.fold_left add b bs
    in
    b, List.fold_left (fun f m -> deps_module b f m.pmb_expr) f bs
| Pstr_modtype x ->
    b, (match x.pmtd_type with None -> f | Some mty -> deps_modtype b f mty)
| Pstr_open od -> b, deps_path ~is_open:true b f od.popen_lid.txt
| Pstr_class cs -> b, List.fold_left (deps_class_declaration b) f cs
| Pstr_class_type ds -> b, List.fold_left (deps_class_type_declaration b) f ds
| Pstr_include inc -> b, deps_module b f inc.pincl_mod
| Pstr_attribute _ | Pstr_extension _ -> b, f

and deps_case b f c =
  let b, f = deps_pattern b f c.pc_lhs in
  let f = match c.pc_guard with None -> f | Some g -> deps_expr b f g in
  deps_expr b f c.pc_rhs

and deps_pattern b f p =  match p.ppat_desc with
| Ppat_any -> b, f
| Ppat_var _ -> b, f
| Ppat_alias (p, _) -> deps_pattern b f p
| Ppat_interval _
| Ppat_constant _ -> b, f
| Ppat_tuple pl ->
    let add (bacc, f) p =
      let b', f = deps_pattern b f p in
      String.Set.union bacc b', f
    in
    List.fold_left add (b, f) pl
| Ppat_construct (c, op) ->
    let f = deps_lid b f c in
    (match op with None -> b, f | Some p -> deps_pattern b f p)
| Ppat_record (pl, _) ->
    let add (bacc, f) (lbl, p) =
      let f = deps_lid b f lbl in
      let b', f = deps_pattern b f p in
      String.Set.union bacc b', f
    in
    List.fold_left add (b, f) pl
| Ppat_array pl ->
    let add (bacc, f) p =
      let b', f = deps_pattern b f p in
      String.Set.union bacc b', f
    in
    List.fold_left add (b, f) pl
| Ppat_or (p0, p1) ->
    let b0, f = deps_pattern b f p0 in
    let b1, f = deps_pattern b f p1 in
    String.Set.union b0 b1, f
| Ppat_constraint (p, t) ->
    let b, f = deps_pattern b f p in
    let f = deps_type b f t in
    b, f
| Ppat_variant (_, op) ->
    (match op with None -> b, f | Some p -> deps_pattern b f p)
| Ppat_type lid -> b, deps_lid b f lid
| Ppat_lazy p -> deps_pattern b f p
| Ppat_unpack id -> String.Set.add id.txt b, f
| Ppat_open (m, p) ->
  let f = deps_path ~is_open:true b f m.txt in
  deps_pattern b f p
| Ppat_exception p -> deps_pattern b f p
| Ppat_extension _ -> b, f

and deps_bindings recf b f pel =
  let add_pat (b, f) p = deps_pattern b f p.pvb_pat in
  let b', f = List.fold_left add_pat (b, f) pel in
  let b = if recf = Asttypes.Recursive then b' else b in
  let f = List.fold_left (fun f e -> deps_expr b f e.pvb_expr) f pel in
  b', f

and deps_class_expr b f ce = match ce.pcl_desc with
| Pcl_constr (l, ts) -> List.fold_left (deps_type b) (deps_lid b f l) ts
| Pcl_structure s ->
    let b, f = deps_pattern b f s.pcstr_self in
    List.fold_left (deps_class_field b) f s.pcstr_fields
| Pcl_fun (_, opte, pat, ce) ->
    let f = match opte with None -> f | Some e -> deps_expr b f e in
    let b, f = deps_pattern b f pat in deps_class_expr b f ce
| Pcl_apply (ce, es) ->
    List.fold_left (fun f (_, e) -> deps_expr b f e) (deps_class_expr b f ce) es
| Pcl_let (rf, pel, ce) ->
    let b, f = deps_bindings rf b f pel in
    deps_class_expr b f ce
| Pcl_constraint (ce, ct) -> deps_class_type b (deps_class_expr b f ce) ct
| Pcl_extension _ -> f
| Pcl_open (_, m, e) ->
    let f = deps_path ~is_open:true b f m.txt in
    deps_class_expr b f e

and deps_class_field b f fld = match fld.pcf_desc with
| Pcf_inherit (_, ce, _) -> deps_class_expr b f ce
| Pcf_val (_, _, Cfk_concrete (_, e))
| Pcf_method (_, _, Cfk_concrete (_, e)) -> deps_expr b f e
| Pcf_val (_, _, Cfk_virtual t)
| Pcf_method (_, _, Cfk_virtual t) -> deps_type b f t
| Pcf_constraint (t0, t1) -> deps_type b (deps_type b f t0) t1
| Pcf_initializer e -> deps_expr b f e
| Pcf_attribute _ | Pcf_extension _ -> f

and deps_class_declaration b f d = deps_class_expr b f d.pci_expr

let file_deps parse_ast deps_ast file =
  try
    Result.bind (Os.File.read file) @@ fun s ->
    let lexbuf = Lexing.from_string s in
    let ret (_, f) =
      let rec loop seen acc = function
      | [] -> List.rev acc
      | (is_open, n as m) :: ms ->
        match String.Set.mem n seen with
        | true -> loop seen acc ms
        | false -> loop (String.Set.add n seen) (m :: acc) ms
      in
      loop String.Set.empty [] (List.rev f)
    in
    Ok (ret @@ deps_ast String.Set.empty [] (parse_ast lexbuf))
  with
  | Syntaxerr.Error _ | Syntaxerr.Escape_error -> Ok []

let mli_deps f = file_deps Parse.interface deps_signature f
let ml_deps f = file_deps Parse.implementation deps_structure f
let cmi_defs f =
  try
    let i = Cmi_format.read_cmi (Fpath.to_string f) in
    let rec loop acc p = function
    | [] -> List.rev acc
    | [] :: todo -> loop acc (List.tl p) todo
    | (i :: is) :: todo ->
        match i with
        | Types.Sig_module (i, d, _) ->
            let id = Ident.name i in
            let p' = id :: p in
            let acc = List.rev p' :: acc in
            begin match d.Types.md_type with
            | Types.Mty_signature is' -> loop acc p' (is' :: is :: todo)
            | _ -> loop acc p (is :: todo)
            end
        | _ -> loop acc p (is :: todo)
    in
    let p = [i.Cmi_format.cmi_name] in
    Ok (loop [p] p [i.Cmi_format.cmi_sign])
  with
  | Cmi_format.Error e -> Error (Fmt.str "%a" Cmi_format.report_error e)
