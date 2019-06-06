open Functoria
open Mirage_backend_common

let dependencies = [ package ~min:"3.2.0" ~max:"4.0.0" "mirage-unix" ]

let alias_name = "unix"

let custom_runtime = None

let variant = "unix"

let compilation_mode = Exe

let ocaml_compilation_flags = ["-thread"]

let ocaml_link_flags = []

let extra_context = None

let generate_extra_files _ ~root:_ ~name:_ = Ok ()

let config_unix ~alias_name ~name ~binary_location =
  let alias = sxp_of_fmt {|
    (alias
      (name %s)
      (enabled_if (= %%{context_name} "default"))
      (deps %s))
    |} alias_name name
  in
  let rule = sxp_of_fmt {|
    (rule
      (targets %s)
      (deps %s)
      (mode promote)
      (action (run ln -nfs %s %s)))
  |} name binary_location binary_location name
  in
  Ok [alias; rule]

let postbuild_rules _ = config_unix

let clean ~name:_ = Ok ()