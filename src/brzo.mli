(*---------------------------------------------------------------------------
   Copyright (c) 2018 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Basic definitions and domain helpers. *)

open B0_std

(** Memo helpers. *)
module Memo : sig
  val copy_file :
    B0_memo.t -> src_root:Fpath.t -> dst_root:Fpath.t ->
    Fpath.t -> unit
    (** [copy m ~src_root ~dst_root src] copies [src] to destination
        {!root_path}[ ~src_root ~dst_root src]. *)

  val ensure_exec_build :
    B0_memo.t -> srcs:B0_file_exts.map -> need_ext:Fpath.ext -> unit Fut.t
  (** [ensure_exe_build m ~srcs ~need_ext k] continues iff there's
      at least a file in [srcs] that has exention [need_ext] and fails
      with a message that no executable can be built otherwise. *)

  val run :
    with_log:Fpath.t option -> B0_memo.t -> (unit -> 'a Fut.t) ->
    ('a, unit) result
end

(** Program exits. *)
module Exit : sig

  val conf_error : Os.Exit.t
  (** [conf_error] indicates a brzo configuration error (e.g. in
      a BRZO file. *)

  val no_build_outcome : Os.Exit.t
  (** [no_build_outcome] indicates there's no build outcome on operations
      that request one. *)

  val no_such_sexp_path : Os.Exit.t
  (** [no_such_sexp_path] indicates that a specified s-expression path
      does not exist. *)

  val ok : Os.Exit.t
  (** [ok] is the zero exit code. *)

  val outcome_build_error : Os.Exit.t
  (** [outcome_build_error] indicates an outcome build error. *)

  val outcome_action_error : Os.Exit.t
  (** [outcome_action_error] indicates an outcome action error. *)

  val some_error : Os.Exit.t
  (** [some_error] indicates an indiscrimante error reported on stderr. *)

  val undefined_domain : Os.Exit.t
  (** [undefined_domain] indicates use of an undefined domain in BRZO file. *)

  (** Cmdliner documentation. *)
  module Info : sig
    val action_exec_exit : Cmdliner.Cmd.Exit.info
    val conf_error : Cmdliner.Cmd.Exit.info
    val no_build_outcome : Cmdliner.Cmd.Exit.info
    val outcome_action_error : Cmdliner.Cmd.Exit.info
    val outcome_build_error : Cmdliner.Cmd.Exit.info
    val some_error : Cmdliner.Cmd.Exit.info
    val undefined_domain : Cmdliner.Cmd.Exit.info
    val base_cmd : Cmdliner.Cmd.Exit.info list
    val domain_cmd : Cmdliner.Cmd.Exit.info list
  end
end

(** Pre-domain.

    A pre-domain is a domain with out its concrete
    {{!Brzo_outcome}outcome implementation} except for their names and
    documentation. There are some recursive knots that need to be untied. *)
module Pre_domain : sig

  (** {1:pre_outcome Pre-outcomes} *)

  type outcome
  (** The type for pre-outcomes. *)

  val outcome : name:string -> doc:string -> outcome
  (** [outcome ~name ~doc] is a pre-outcome with name [name] and
      cmdliner documentation string [doc]. *)

  val outcome_name : outcome -> string
  (** [outcome_name o] is the name of the pre-outcome [o]. *)

  val outcome_doc : outcome -> string
  (** [outcome_name o] is the documetnation string of the pre-outcome [o]. *)

  (** {1:pre_domains Pre-domains} *)

  (** The module type for pre-domain configurations. *)
  module type CONF = sig

    (** {1:conf Domain configuration} *)

    type t
    (** The type for domain specific configurations. *)

    val tid : t Type.Id.t
    (** [tid] is a type identifier for the configuration. *)

    val keys : String.Set.t
    (** [keys] is the set of keys that are allowed to be part of the
        domain specific BRZO file dictionary. *)

    val parse :
      (B0_sexp.Sexp.t * B0_sexp.Sexpq.path) ->
      (t, string) result
    (** [parse] is a function that parses the BRZO file domain
        specific dictionary into a configuration. *)

    val parse_with_cli :
      ((B0_sexp.Sexp.t * B0_sexp.Sexpq.path) ->
       (t, string) result) Cmdliner.Term.t
    (** [parse_with_cli] is a cmdliner term that has the command line
        interface for the configuration and a function that given the
        configuration s-expression dictionary for the domain should
        derive the configuration appropriately (basically it should do
        the job of merging cli and file specified configuration
        desires). *)

    val pp : t Fmt.t
    (** [pp] formats configurations. *)
  end

  (** The module type for pre-domains. *)
  module type T = sig

    (** {1:conf Configuration} *)

    (** Domain configuration. *)
    module Conf : CONF

    (** {1:def Domain definition} *)

    val name : string
    (** [name] is the domain name. Defines the domain sub-command name. *)

    val doc_name : string
    (** [doc_name] is the domain name for documentation. *)

    val fingerprint : B0_file_exts.t
    (** [fingerprint] are file extensions whose presence in the source
        files hints at domain selection. *)

    val pre_outcomes : outcome list
    (** [pre_outcomes] is the domain's list of outcomes. *)
  end

  type t = V : (module T with type Conf.t = 'a) -> t (** *)
  (** The type for pre-domains. *)

  val name : t -> string
  (** [id d] is the name of pre-domain [d]. *)

  val outcome_names : t -> String.Set.t
  (** [outcome_names d] is the set of outcomes provided by the
      pre-domain [d]. *)
end

(** S-expression toolbox. *)
module Sexp : sig

  val of_string :
    ?file:Fpath.t -> string -> (B0_sexp.Sexp.t, string) result
  (** [of_string] is like {!B0_sexp.Sexp.seq_of_string'} but
      with alternate error formatting. *)

  val of_file : Fpath.t -> (B0_sexp.Sexp.t * 'a list, string) result
  (** [of_file file] reads from [file] using {!of_string}. *)

  val query :
    'a B0_sexp.Sexpq.t ->
    (B0_sexp.Sexp.t * B0_sexp.Sexpq.path) ->
    ('a, string) result
  (** [query] is like {!B0_sexp.Sexp.query'} but with
      alternate error formatting. *)

  val fpath : Fpath.t B0_sexp.Sexpq.t
  (** [fpath] is queries atoms for file paths. *)
end

(** Configuration. *)
module Conf : sig

  (** {1:conf_bits Configuration bits} *)

  (** Configuration bits.

      Bundles a cli interface and a configuration file lookup.

      {b Note.} For Cmdliner proper support we would likely hide it in
      Cmdliner terms and let them handle the merge logic. Though that
      poses a few problems with e.g. configuration error report/recovery.
      Another problem is the recursion between specifying the configuration
      file lookup on the cli itself while using it terms themselves.
      Also lazy absents. *)
  module Bit : sig

    (** {1:bits Configuration bits} *)

    type 'a t
    (** The type for configuration bits of type ['a]. *)

    val with_cli_arg :
      ?docs:string -> ?docv:string -> string -> doc:string -> absent:'a ->
      conf:'a B0_sexp.Sexpq.t ->
      arg:(Cmdliner.Arg.info -> 'a option Cmdliner.Arg.t) ->  'a t
    (** [with_cli_arg n ~doc ~docs ~docv ~absent ~conf ~arg] is a
        configuration bit named by [n].
        {ul
        {- [arg] is used to parse the bit from the cli using an option
           argument named [n] documented with [doc] in section [docs] and
           metavariable [docv].}
        {- [conf] is used to lookup the bit value in a configuration
           dictionary with an optional key [n].}
        {- [absent] is used as a default value.}} *)

    val with_cli :
      string -> absent:'a -> conf:'a B0_sexp.Sexpq.t ->
      cli:'a option Cmdliner.Term.t -> 'a t
    (** [with_cli n ~absent ~conf ~cli] is a configuration bit named by [n]
        {ul
        {- [cli] is used to get the bit from the cli.}
        {- [conf] is used to lookup the bit value in a configuration dictionary
           with an optional key [n].}
        {- [basent] is used as a default value.}} *)

    val cli : 'a t -> 'a option Cmdliner.Term.t
    (** [cli b] is the command line interface for [b]. *)

    val conf : 'a t -> 'a B0_sexp.Sexpq.t
    (** [conf b] is the configuration {e key} query for [b]. If the key
        is not found [absent] is returned. *)

    val get : 'a t -> 'a option ->
      (B0_sexp.Sexp.t * B0_sexp.Sexpq.path) ->
      ('a, string) result
    (** [get b cli sexp] gets the configuration bit from [sexp] if [cli] is
        [None]. If it's not in the configuration then [absent] is returned.  *)

    val append :
      'a list t -> 'a list option ->
      (B0_sexp.Sexp.t * B0_sexp.Sexpq.path) ->
      ('a list, string) result
    (** [append b cli sexp] gets the configuration bit from [sexp] and
        appends them to [cli]. *)

  end

  (** {1:filename Default file names} *)

  val brzo_file_name : string
  (** [brzo_file_name] is ["BRZO"] the default BRZO file name. *)

  val brzo_dir_name : string
  (** [brzo_dir_name] is ["brzo"] the default brzo directory name
      in the b0 directory. *)

  (** {1:conf Configuration} *)

  type outcome_mode = [ `Action | `Build | `Conf | `Delete | `Normal | `Path ]
  (** The type for outcome modes. *)

  type domain =
  | Domain :
      ((module Pre_domain.T with type Conf.t = 'a)) * string * 'a ->
      domain (** *)
  (** The type for domain configurations. The pre-domain, its configured
      outcome name and its configuration value. *)

  type t
  (** The type for configurations. *)

  val v :
    action_args:string list -> background:bool -> b0_dir:Fpath.t ->
    brzo_file:Fpath.t option -> cache_dir:Fpath.t -> cwd:Fpath.t ->
    domain_name:string option -> domain_confs:domain list ->
    hash_fun:(module Hash.T) -> jobs:int -> log_file:Fpath.t ->
    log_level:Log.level -> no_pager:bool -> outcome_mode:outcome_mode ->
    output_outcome_path:bool -> pdf_viewer:Cmd.t option -> root:Fpath.t ->
    srcs_i:Fpath.Set.t -> srcs_x:Fpath.Set.t -> tty_cap:Tty.cap ->
    web_browser:Cmd.t option -> unit -> t
  (** [v] constructs a configuration with given attributes. See the
      accessors for semantics. *)

  val action_args : t -> string list
  (** [action_args] are the outcome action arguments. *)

  val background : t -> bool
  (** [background] is [true] if external viewers should be launched
      in the background. *)

  val brzo_file : t -> Fpath.t option
  (** [brzo_file] is the absolute path to the brzo file found at the
      root (if any). *)

  val b0_dir : t -> Fpath.t
  (** [b0_dir] is the absolute path to the b0 dir. *)

  val cache_dir : t -> Fpath.t
  (** [cache_dir] is the absolute path to the cache directory. *)

  val cwd : t -> Fpath.t
  (** [cwd] is the absolute path to the current working directoy. *)

  val domain_name : t -> string option
  (** [domain_name] is the name of the running domain (if any). *)

  val domain_confs : t -> domain list
  (** [domain_confs] is the list of domain specific configurations. *)

  val domain_conf :
    t -> (module Pre_domain.T with type Conf.t = 'a) -> (string * 'a) option
  (** [domain_conf c d] is the configuration for pre-domain
      [d] (if any). This is the domain's selected
      outcome and its configuration. *)

  val log_file : t -> Fpath.t
  (** [log_file] is the absolute path to the build log file. *)

  val log_level : t -> Log.level
  (** [log_level] is the desired log level. *)

  val hash_fun : t -> (module Hash.T)
  (** [hash_fun] is the hash function to use for build caching. *)

  val jobs : t -> int
  (** [jobs] is the maximal number of spawns allowed. *)

  val memo : t -> (B0_memo.t, string) result
  (** [memo] is the memoizer for the configuration. *)

  val no_pager : t -> bool
  (** [no_pager] indicates no paging is desired on stdout. *)

  val outcome_mode : t -> outcome_mode
  (** [outcome] is the build outcome mode. *)

  val output_outcome_path : t -> bool
  (** [output_outcome_path] is [true] if the outcome path should be
      output at the end of the build. *)

  val root : t -> Fpath.t
  (** [root] is absolute path to the brzo root. *)

  val pdf_viewer : t -> Cmd.t option
  (** [pdf_viewer] is the PDF viewer command. *)

  val srcs_i : t -> Fpath.Set.t
  (** [srcs_i] are absolute prefixes to include. *)

  val srcs_x : t -> Fpath.Set.t
  (** [srcs_x] are absolute prefixes to exclude. *)

  val srcs : t -> (B0_file_exts.map, string) result
  (** [srcs c] are the absolute source files path in configuration [c]
      sorted by file extension. *)

  val tty_cap : t -> Tty.cap
  (** [tty_cap c] is the terminal capability to assume for output. *)

  val web_browser : t -> Cmd.t option
  (** [web_browser] is the WWW browser command to use. *)

  val pp_auto : 'a Fmt.t -> 'a option Fmt.t
  (** [pp_auto pp] formats ["<auto>"] for [None] and the value with
      [pp] otherwise. *)

  val pp : t Fmt.t
  (** [pp] formats configurations. Does not format domain specific
      configuration. *)

  val pp_show : t Fmt.t
  (** [pp_show] formats a header and the configuration with {!pp}.  *)
end

(** Cli interaction. *)
module Cli : sig

  (** {1:manfrag Man pages fragments} *)

  val s_domains : string
  (** [s_domains] is a section that lists the domains. *)

  val s_outcomes : string
  (** [s_outcomes] is a manual section for outcome options. *)

  val s_outcome_mode : string
  (** [s_outcome_opts] is a manual section for outcome specific options. *)

  val s_outcome_opts : string
  (** [s_outcome_opts] is a manual section for outcome specific options. *)

  val man_see_manual : Cmdliner.Manpage.block
  (** [man_see_manual] is a cmdliner manual fragment enticing to read
      the manual. *)

  (** {1:conf Cli configuration} *)

  val conf :
    auto_cwd_root:bool -> use_brzo_file:bool -> domain:Pre_domain.t option ->
    all_domains:Pre_domain.t list -> Conf.t Cmdliner.Term.t
  (** [conf ~use_brzo_file ~domain ~all_domain] is a {!Cmdliner} term for
      configuration with:

      {ul
      {- [auto_cwd_root], allows the root to fall back to {!Conf.cwd}
         if the root can't be determined. This allows commands
         that work e.g. on the [_b0] directory to work after
         the [BRZO] file marking the root might have been deleted.}
      {- [use_brzo_file], if [true] the BRZO file is consulted.}
      {- [domain], if [Some d] the cli has the arguments for the
         pre-domain [d].}
      {- [all_domains] is the list of domains knows to [brzo] as
         pre-domains.}}

      Term evaluation performs a few side effects. In particular
      it setups colored output and logging via {!B0_std_ui.setup}. *)
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
