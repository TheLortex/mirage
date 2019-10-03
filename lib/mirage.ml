(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2018 Mindy Preston     <meetup@yomimono.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Rresult
open Astring
open Sexplib

module Key = Mirage_key
module Name = Functoria_app.Name
module Codegen = Functoria_app.Codegen

module Log = Mirage_impl_misc.Log
open Mirage_impl_misc

include Functoria

(** {2 Devices} *)

include Mirage_impl

(** Functoria devices *)

type info = Functoria_app.info
let noop = Functoria_app.noop
let info = Functoria_app.info
let app_info = Functoria_app.app_info ~type_modname:"Mirage_info" ()

let configure_makefile ~no_depext ~opam_name extra_content =
  let open Codegen in
  let file = Fpath.(v "Makefile") in
  with_output file (fun oc () ->
      let fmt = Format.formatter_of_out_channel oc in
      append fmt "# %s" (generated_header ());
      newline fmt;
      append fmt "-include Makefile.user";
      newline fmt;
      let depext = if no_depext then "" else "\n\t$(DEPEXT)" in
      append fmt "OPAM = opam\n\
                  DEPEXT ?= $(OPAM) pin add -k path --no-action --yes %s . &&\\\n\
                  \t$(OPAM) depext --yes --update %s ;\\\n\
                  \t$(OPAM) pin remove --no-action %s\n\
                  \n\
                  .PHONY: all depend depends clean build\n\
                  all:: build\n\
                  \n\
                  depend depends::%s\n\
                  \t$(OPAM) install -y --deps-only .\n\
                  \n\
                  build::\n\
                  \tmirage build\n\
                  \n\
                  clean::\n\
                  \tmirage clean\n
                  %s"
        opam_name opam_name opam_name depext extra_content;
      R.ok ())
    "Makefile"

let terminal () =
  let dumb = try Sys.getenv "TERM" = "dumb" with Not_found -> true in
  let isatty = try Unix.(isatty (descr_of_out_channel Pervasives.stdout)) with
    | Unix.Unix_error _ -> false
  in
  not dumb && isatty

let dune_filename = Fpath.(v "dune")
let dune_workspace_filename = Fpath.(v "dune-workspace")
let dune_project_filename = Fpath.(v "dune-project")

let target_module = function
  | `Unix   -> (module Mirage_backend.Unix : Mirage_backend.S)
  | `MacOSX -> (module Mirage_backend.Unix : Mirage_backend.S)
  | `Hvt    -> (module Mirage_backend.Hvt : Mirage_backend.S)
  | `Virtio -> (module Mirage_backend.Virtio : Mirage_backend.S)
  | `Muen   -> (module Mirage_backend.Muen : Mirage_backend.S)
  | `Genode -> (module Mirage_backend.Genode : Mirage_backend.S)
  | `Xen    -> (module Mirage_backend.Xen : Mirage_backend.S)
  | `Qubes  -> (module Mirage_backend.Qubes : Mirage_backend.S)
  | `Esp32  -> (module Mirage_backend.Esp32 : Mirage_backend.S)

let sxp_of_fmt fmt = Fmt.kstrf Sexplib.Sexp.of_string fmt

let configure_dune_workspace i =
  let ctx = Info.context i in
  let target = Key.(get ctx target) in
  let (module Target) = target_module target in
  let lang = sxp_of_fmt "(lang dune 1.11)"
  and variants_feature = sxp_of_fmt "(using library_variants 0.1)"
  and profile_release = sxp_of_fmt "(profile release)"
  and base_context = sxp_of_fmt "(context (default))"
  in
  let extra_contexts =
    match Target.extra_context with
    | Some i -> [i]
    | None -> []
  in
  (* write dune-workspace *)
  let rules = lang::profile_release::base_context::extra_contexts
  in
  Bos.OS.File.write_lines
    dune_workspace_filename
    (List.map (fun x -> (Sexp.to_string_hum x)^"\n") rules)
  >>= fun () ->
  (* write dune-project *)
  let rules = [lang; variants_feature]
  in
  Bos.OS.File.write_lines
    dune_project_filename
    (List.map (fun x -> (Sexp.to_string_hum x)^"\n") rules)


let target_file = function
  | Mirage_backend.Exe -> "main.exe"
  | Object -> "main.exe.o"

let s_output_mode = function
  | Mirage_backend.Exe -> "(native exe)"
  | Object -> "(native object)"

let configure_dune i =
  let ctx = Info.context i in
  let warn_error = Key.(get ctx warn_error) in
  let target = Key.(get ctx target)
  in
  let (module Target) = target_module target
  in
  let custom_runtime = Target.custom_runtime in
  let compilation_mode = Target.compilation_mode in
  let libs = Info.libraries i in
  let name = Info.name i in
  let binary_location = target_file compilation_mode in
  let cflags = ["-g";"-w";"+A-4-41-42-44";"-bin-annot";"-strict-sequence";
      "-principal";"-safe-string" ] @
      (if warn_error then ["-warn-error"; "+1..49"] else []) @
      (Target.ocaml_compilation_flags) @
      (if terminal () then ["-color"; "always"] else []) @
      (match custom_runtime with Some runtime -> ["-runtime-variant"; runtime] | _ -> [])
  and lflags = "-g"::Target.ocaml_link_flags
  in
  let s_libraries = String.concat ~sep:" " libs in
  let s_link_flags = String.concat ~sep:" " lflags in
  let s_compilation_flags = String.concat ~sep:" " cflags in
  let s_variants = Target.variant
  in
  let config = sxp_of_fmt {|
    (executable
      (name main)
      (modes %s)
      (libraries %s)
      (link_flags %s)
      (flags %s)
      (variants %s))
  |} (s_output_mode compilation_mode)
     s_libraries
     s_link_flags
     s_compilation_flags
     s_variants
  in
  let alias_name = Target.alias_name in
  Target.postbuild_rules ~alias_name ~name ~binary_location i >>= fun rules ->
  let rules = config::rules in
  Bos.OS.File.write_lines
    dune_filename
    (List.map (fun x -> (Sexp.to_string_hum x)^"\n") rules)


(* we made it, so we should clean it up *)
let clean_dune () = Bos.OS.File.delete dune_filename

let opam_file n = Fpath.(v n + "opam")

let configure_opam ~name info =
  let open Codegen in
  let file = opam_file name in
  with_output file (fun oc () ->
      let fmt = Format.formatter_of_out_channel oc in
      append fmt "# %s" (generated_header ());
      Info.opam ~name fmt info;
      append fmt "maintainer: \"dummy\"";
      append fmt "authors: \"dummy\"";
      append fmt "homepage: \"dummy\"";
      append fmt "bug-reports: \"dummy\"";
      append fmt "build: [ \"mirage\" \"build\" ]";
      append fmt "synopsis: \"This is a dummy\"";
      R.ok ())
    "opam file"

let opam_name name target =
  String.concat ~sep:"-" ["mirage"; "unikernel"; name; target]

let unikernel_opam_name name target =
  let target_str = Fmt.strf "%a" Key.pp_target target in
  opam_name name target_str

let configure i =
  let name = Info.name i in
  let root = Fpath.to_string (Info.build_dir i) in
  let ctx = Info.context i in
  let target = Key.(get ctx target)
  in
  let (module Target) = target_module target
  in
  Log.info (fun m -> m "Configuring for target: %a" Key.pp_target target);
  let opam_name = unikernel_opam_name name target in
  let target_debug = Key.(get ctx target_debug) in
  if target_debug && target <> `Hvt then
    Log.warn (fun m -> m "-g not supported for target: %a" Key.pp_target target);
  configure_dune i >>= fun () ->
  configure_dune_workspace i >>= fun () ->
  configure_opam ~name:opam_name i >>= fun () ->
  let no_depext = Key.(get ctx no_depext) in
  configure_makefile ~no_depext ~opam_name Target.extra_makefile >>= fun () ->
  Target.generate_extra_files i ~root ~name

let cross_compile target =
  let (module Target) = target_module target in
  Target.cross_compile

let compile target =
  let target_name = Fmt.strf "%a" Key.pp_target target in
  let cmd = match cross_compile target with
  | None ->         Bos.Cmd.(v "dune" % "build" % ("@"^target_name) )
  | Some backend -> Bos.Cmd.(v "dune" % "build" % ("@"^target_name) % "-x" % backend)
  in
  Log.info (fun m -> m "executing %a" Bos.Cmd.pp cmd);
  Bos.OS.Cmd.run cmd


let link _info _name _target _target_debug = Ok ()
let check_entropy libs =
  query_ocamlfind ~recursive:true libs >>= fun ps ->
  if List.mem "nocrypto" ps && not (Mirage_impl_random.is_entropy_enabled ()) then
    R.error_msg
      {___|The \"nocrypto\" library is loaded but entropy is not enabled!@ \
       Please enable the entropy by adding a dependency to the nocrypto \
       device. You can do so by adding ~deps:[abstract nocrypto] \
       to the arguments of Mirage.foreign.|___}
  else
    R.ok ()


let build i =
  let name = Info.name i in
  let ctx = Info.context i in
  let target = Key.(get ctx target) in
  let libs = Info.libraries i in
  let target_debug = Key.(get ctx target_debug) in
  check_entropy libs >>= fun () ->
  compile target >>= fun () ->
  link i name target target_debug >>| fun () ->
  Log.info (fun m -> m "Build succeeded")

let clean i =
  let name = Info.name i in
  let ctx = Info.context i in
  let target = Key.(get ctx target) in
  let (module Target) = target_module target in
  Target.clean ~name >>= fun () ->
  clean_dune () >>= fun () ->
  Bos.OS.File.delete Fpath.(v "Makefile") >>= fun () ->
  Bos.OS.File.delete (opam_file (unikernel_opam_name name `Hvt)) >>= fun () ->
  Bos.OS.File.delete (opam_file (unikernel_opam_name name `Unix)) >>= fun () ->
  Bos.OS.File.delete (opam_file (unikernel_opam_name name `Xen)) >>= fun () ->
  Bos.OS.File.delete (opam_file (unikernel_opam_name name `Qubes)) >>= fun () ->
  Bos.OS.File.delete (opam_file (unikernel_opam_name name `Muen)) >>= fun () ->
  Bos.OS.File.delete (opam_file (unikernel_opam_name name `MacOSX)) >>= fun () ->
  Bos.OS.File.delete (opam_file (unikernel_opam_name name `Genode)) >>= fun () ->
  Bos.OS.File.delete Fpath.(v name) >>= fun () ->
  Bos.OS.File.delete Fpath.(v name + "xen") >>= fun () ->
  Bos.OS.File.delete Fpath.(v name + "elf") >>= fun () ->
  Bos.OS.File.delete Fpath.(v name + "virtio") >>= fun () ->
  Bos.OS.File.delete Fpath.(v name + "muen") >>= fun () ->
  Bos.OS.File.delete Fpath.(v name + "hvt") >>= fun () ->
  Bos.OS.File.delete Fpath.(v name + "genode") >>= fun () ->
  Bos.OS.File.delete Fpath.(v "Makefile.solo5-hvt") >>= fun () ->
  Bos.OS.Dir.delete ~recurse:true Fpath.(v "_build-solo5-hvt") >>= fun () ->
  Bos.OS.File.delete Fpath.(v "solo5-hvt") >>= fun () ->
  (* The following deprecated names are kept here to allow "mirage clean" to
   * continue to work after an upgrade. *)
  Bos.OS.File.delete (opam_file (opam_name name "ukvm")) >>= fun () ->
  Bos.OS.File.delete Fpath.(v name + "ukvm") >>= fun () ->
  Bos.OS.File.delete Fpath.(v "Makefile.ukvm") >>= fun () ->
  Bos.OS.Dir.delete ~recurse:true Fpath.(v "_build-ukvm") >>= fun () ->
  Bos.OS.File.delete Fpath.(v "ukvm-bin")

module Project = struct
  let name = "mirage"
  let version = "%%VERSION%%"
  let prelude =
    "open Lwt.Infix\n\
     let return = Lwt.return\n\
     let run = OS.Main.run"

  (* The ocamlfind packages to use when compiling config.ml *)
  let packages = [package "mirage"]

  let ignore_dirs = []

  let create jobs = impl @@ object
      inherit base_configurable
      method ty = job
      method name = "mirage"
      method module_name = "Mirage_runtime"
      method! keys = [
        Key.(abstract target);
        Key.(abstract warn_error);
        Key.(abstract target_debug);
        Key.(abstract no_depext);
      ]
      method! packages =
        (* XXX: use %%VERSION_NUM%% here instead of hardcoding a version? *)
        let min = "4.0.0" and max = "4.1.0" in
        let common = [
          package ~build:true ~min:"4.07.1" "ocaml";
          package "lwt";
          package "mirage-os-shim";
          package ~min ~max "mirage-types-lwt";
          package ~min ~max "mirage-types";
          package ~min ~max "mirage-runtime" ;
          package ~build:true ~min ~max "mirage" ;
          package ~build:true "ocamlfind" ;
          package ~build:true "dune" ;
        ] in
        Key.match_ Key.(value target)
        (fun target ->
          let (module Target) = target_module target in
          Target.dependencies @ common
        )

      method! build = build
      method! configure = configure
      method! clean = clean
      method! connect _ _mod _names = "Lwt.return_unit"
      method! deps = List.map abstract jobs
    end

end

include Functoria_app.Make (Project)

(** {Custom registration} *)

let (++) acc x = match acc, x with
  | _ , None -> acc
  | None, Some x -> Some [x]
  | Some acc, Some x -> Some (acc @ [x])

(* TODO: ideally we'd combine these *)
let qrexec_init = match_impl Key.(value target) [
  `Qubes, Mirage_impl_qrexec.qrexec_qubes;
] ~default:Functoria_app.noop

let gui_init = match_impl Key.(value target) [
  `Qubes, Mirage_impl_gui.gui_qubes;
] ~default:Functoria_app.noop

let register
    ?(argv=default_argv) ?tracing ?(reporter=default_reporter ())
    ?keys ?packages
    name jobs =
  let argv = Some (Functoria_app.keys argv) in
  let reporter = if reporter == no_reporter then None else Some reporter in
  let qubes_init = Some [qrexec_init; gui_init] in
  let init = qubes_init ++ argv ++ reporter ++ tracing in
  register ?keys ?packages ?init name jobs
