open Functoria
open Mirage_backend_common
open Mirage_impl_misc
open Rresult

module Codegen = Functoria_app.Codegen

include Default_backend

let dependencies = [ package ~pin:"git+https://github.com/well-typed-lightbulbs/mirage-esp32" "mirage-esp32" ]

let alias_name = "esp32"

let cross_compile = Some "esp32"

let variant = "esp32"

let compilation_mode = Object

let extra_makefile = "\
  PKG_CONFIG_PATH := $(shell opam config var share)/pkgconfig\n\
  export PKG_CONFIG_PATH\n\
  IDF_PATH := $(strip $(shell PKG_CONFIG_PATH='$(PKG_CONFIG_PATH)' pkg-config esp32-idf --libs --static))\n\
  export IDF_PATH\n\
  main.native.o:\n\
  \tmirage build\n\
   _build-esp32/main/main.native.o: main.native.o\n\
  \tcp main.native.o _build-esp32/main/main.native.o\n\
  .PHONY: size size-components flash monitor\n\
  size: _build-esp32/main/main.native.o\n\
  \t$(MAKE) -C _build-esp32 size\n\
  size-components: _build-esp32/main/main.native.o\n\
  \t$(MAKE) -C _build-esp32 size-components\n\
  flash _build-esp32/main/main.native.o:\n\
  \t$(MAKE) -C _build-esp32 flash\n\
  monitor _build-esp32/main/main.native.o:\n\
  \t$(MAKE) -C _build-esp32 monitor\n\
  menuconfig:\n\
  \t$(MAKE) -C _build-esp32 menuconfig\n
"

let config_esp32 ~alias_name ~name:_ ~binary_location =
  let alias = sxp_of_fmt {|
    (alias
      (name %s)
      (enabled_if (= %%{context_name} "default.esp32"))
      (deps %s))
    |} alias_name "main.native.o"
  in
  let rule = sxp_of_fmt {|
    (rule
      (targets %s)
      (deps %s)
      (mode promote)
      (action (run ln -nfs %s %s)))
  |} "main.native.o" binary_location binary_location "main.native.o"
  in
  Ok [alias; rule]

let postbuild_rules _ = config_esp32

let configure_idf_directory () =
  Bos.OS.Dir.create Fpath.(v "_build-esp32/main") >>= fun _ ->
  let open Codegen in
  (* Create SDK makefile in _build-esp32/Makefile *)
  let file = Fpath.(v "_build-esp32/Makefile") in
  with_output file (fun oc () ->
      let fmt = Format.formatter_of_out_channel oc in
      append fmt "PROJECT_NAME := mirage\n\
                  include $(IDF_PATH)/make/project.mk\n";
      R.ok())
    "Makefile.user"
  >>= fun () ->
  (* Create custom build instructions in _build-esp32/main/component.mk *)
  let file = Fpath.(v "_build-esp32/main/component.mk") in
  with_output file (fun oc () ->
      let fmt = Format.formatter_of_out_channel oc in
      append fmt "COMPONENT_EXTRA_INCLUDES := $(shell ocamlc -where)\n\
                  COMPONENT_ADD_LDFLAGS := -l$(COMPONENT_NAME) -z muldefs\n\
                  COMPONENT_EXTRA_CLEAN := main.o startup.o startup-c.o\n\
                  COMPONENT_SRCDIRS := .\n\
\n\
                  startup.o: $(COMPONENT_PATH)/startup-c.c\n\
                   \t$(CC) $(CFLAGS) $(CPPFLAGS) $(addprefix -I ,$(COMPONENT_INCLUDES)) $(addprefix -I ,$(COMPONENT_EXTRA_INCLUDES)) -c $< -o $%@\n\
\n\
                  startup-c.o: startup.o $(COMPONENT_PATH)/main.native.o\n\
                    \t$(LD) -r startup.o $(COMPONENT_PATH)/main.native.o $(EXTLIBS) -o $%@\n\
                    \n\
                  .PHONY: build\n\
                  build: startup-c.o\n";
      R.ok())
    "component.mk"
  >>= fun () ->
  (* Create startup file in _build-esp32/main/startup-c.c *)
  let file = Fpath.(v "_build-esp32/main/startup-c.c") in
  with_output file (fun oc () ->
      let fmt = Format.formatter_of_out_channel oc in
      append fmt "#include <stdio.h>\n\
                  #include <errno.h>\n\
\n\
                  static char *argv[] = {\"mirage\", NULL};\n\
                  extern void caml_main(char **argv);\n\
\n\
                  int signal() {\n\
                      printf(\"signal: not implemented\\n\");\n\
                      errno = ENOSYS;\n\
                      return -1;\n\
                  }\n\
\n\
                  __uint64_t __bswap64(__uint64_t _x){return (__builtin_bswap64(_x));}\n\
\n\
                  void app_main()\n\
                  {\n\
                      caml_main(argv);\n\
                  }\n";
      R.ok())
    "startup-c.c"

let generate_extra_files _ ~root:_ ~name:_ = configure_idf_directory ()

let clean ~name:_ =
  Bos.OS.Dir.delete Fpath.(v "_build-esp32")
