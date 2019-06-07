type compilation_mode = Exe | Object

let sxp_of_fmt fmt = Fmt.kstrf Sexplib.Sexp.of_string fmt

module Default_backend = struct

  let dependencies = []

  let custom_runtime = None

  let cross_compile = None

  let ocaml_compilation_flags = []

  let ocaml_link_flags = []

  let extra_context = None

  let extra_makefile = ""

  let generate_extra_files _ ~root:_ ~name:_ = Ok ()

  let clean ~name:_ = Ok ()

end