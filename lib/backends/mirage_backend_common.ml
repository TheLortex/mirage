type compilation_mode = Exe | Object

let sxp_of_fmt fmt = Fmt.kstrf Sexplib.Sexp.of_string fmt
