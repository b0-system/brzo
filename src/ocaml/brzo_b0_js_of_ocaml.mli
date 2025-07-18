(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** B0 js_of_ocaml support. *)

open B0_std

(** [js_of_ocaml] compilation. *)
module Js_of_ocaml : sig

  (** {1:jsoo Js_of_ocaml} *)

  val compile :
    ?args:Cmd.t -> B0_memo.t -> byte_exe:Fpath.t -> o:Fpath.t -> unit
  (** [compile m ~byte_exe ~o] compiles the byte code executable [byte_exe]
      to the JavaScript file [o]. *)


  val compile_toplevel :
    ?args:Cmd.t -> B0_memo.t -> byte_exe:Fpath.t -> mod_names:Fpath.t ->
    o:Fpath.t -> unit
  (** [compile m ~byte_exe ~mod_names ~o] compiles the byte code executable
      [byte_exe] to a toplevel in which the modules mentioned in [mod_names]
      are made visible. *)

  val link :
    ?args:Cmd.t -> B0_memo.t -> jss:Fpath.t list -> o:Fpath.t -> unit
  (** [link m ~jss ~o] links the JavaScript files [jss] into [o]. *)

  val tty_glue :
    exe:Fpath.t -> cwd:Fpath.t -> env:B0_std.Os.Env.t -> args:string list ->
    string

  val write_page :
    ?toplevel_css:bool ->
    ?generator:string -> ?lang:string -> ?scripts:string list ->
    ?styles:string list -> ?title:string ->
    B0_memo.t -> o:B0_std.Fpath.t ->
    unit
  (** [write_page m ~title ~o] writes to file [o] a full HTML document whose
      body contains only a {!B0_web.Htmlg.noscript} element that
      entices the user, in english, to enable javascript. [title]
      defaults to the basename of [o] without its extension, for the
      other arguments and more information see {!B0_web.Htmlg.basic_page}.

      [toplevel_css] if [true] adds a bit of styling for an [#ocaml]
      element that is supposed to hold a toplevel session (defaults to
      [false]). *)

  val toplevel_ui_src : string
  (** [toplevel_ui_src] a simple toplevel user interface written in OCaml.
      That can be compiled and linked when using {!compile_toplevel} to
      get a simple prompt. *)
end
