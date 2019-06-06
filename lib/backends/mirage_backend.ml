include Mirage_backend_common

module type S = sig

  val dependencies : Functoria.package list

  val alias_name : string

  val variant : string

  val custom_runtime : string option

  val compilation_mode : compilation_mode

  val ocaml_compilation_flags : string list

  val ocaml_link_flags : string list

  val extra_context : Sexplib.Sexp.t option

  val generate_extra_files : Functoria.Info.t -> root:string -> name:string -> (unit, [> Rresult.R.msg]) result

  val postbuild_rules : Functoria.Info.t -> alias_name:string -> name:string -> binary_location:string
    -> (Sexplib.Sexp.t list, [> Rresult.R.msg]) result

  val clean : name:string -> (unit, [> Rresult.R.msg]) result
end

module Hvt    : S = Mirage_backend_freestanding.Hvt
module Virtio : S = Mirage_backend_freestanding.Virtio
module Muen   : S = Mirage_backend_freestanding.Muen
module Genode : S = Mirage_backend_freestanding.Genode
module Xen    : S = Mirage_backend_xen
module Qubes  : S = Mirage_backend_xen
module Unix   : S = Mirage_backend_unix

