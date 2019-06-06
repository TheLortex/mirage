open Functoria
open Mirage_backend_common
open Astring
open Mirage_impl_misc
open Rresult

module Codegen = Functoria_app.Codegen
module Key = Mirage_key

module Common = struct

  let dependencies = [ package ~min:"0.5.0" ~max:"0.6.0" "mirage-solo5" ]

  let custom_runtime = None

  let variant = "freestanding"

  let compilation_mode = Object

  let ocaml_compilation_flags = []

  let ocaml_link_flags = ["(:include libs)"]

  let extra_context = Some (sxp_of_fmt {|
      (context (default
        (name mirage-freestanding)
        (host default)
        (env (_
          (c_flags (:include cflags))
        ))))
    |})

  let common_postbuild_rules =
    let rule_libs = sxp_of_fmt "(rule (copy %%{lib:ocaml-freestanding:libs} libs))" in
    let rule_ldflags = sxp_of_fmt "(rule (copy %%{lib:ocaml-freestanding:ldflags} ldflags))" in
    let rule_cflags = sxp_of_fmt "(rule (copy %%{lib:ocaml-freestanding:cflags} cflags))" in
    let rule_ld = sxp_of_fmt "(rule (copy %%{lib:ocaml-freestanding:ld} ld))" in
    let rule_libdir = sxp_of_fmt "(rule (copy %%{lib:ocaml-freestanding:libdir} libdir))"
    in
    [rule_ldflags; rule_cflags; rule_libs; rule_libdir; rule_ld]

  let generate_extra_files _ ~root:_ ~name:_ = Ok ()

  let clean ~name:_ = Ok ()
end


module Hvt = struct
  include Common

  let dependencies = package ~min:"0.4.0" ~max:"0.5.0" ~ocamlfind:[] "solo5-bindings-hvt" :: dependencies

  let alias_name = "hvt"

  let postbuild_rules i ~alias_name ~name ~binary_location =
    (* Generate target alias *)
    let alias = sxp_of_fmt {|
      (alias
        (name %s)
        (enabled_if (= %%{context_name} "mirage-freestanding"))
        (deps solo5-hvt %s))
    |} alias_name name
    in
    (* Generate solo5-hvt rules*)
    let ctx = Info.context i in
    let libs = Info.libraries i in
    let target_debug = Key.(get ctx target_debug) in
    let tender_mods =
      List.fold_left (fun acc -> function
          | "mirage-net-solo5" -> "net" :: acc
          | "mirage-block-solo5" -> "blk" :: acc
          | _ -> acc)
        [] libs @ (if target_debug then ["gdb"] else [])
    in
    let tender_mods = String.concat ~sep:" " tender_mods in
    let rule_solo5_makefile = sxp_of_fmt {|
        (rule
          (targets Makefile.solo5-hvt)
          (action (run solo5-hvt-configure %%{read:libdir} %s)))
      |} tender_mods
    in
    let rule_solo5_hvt = sxp_of_fmt {|
      (rule
        (targets solo5-hvt)
        (deps Makefile.solo5-hvt)
        (mode promote)
        (action (run make -f Makefile.solo5-hvt solo5-hvt)))
    |}
    in
    (* Generate unikernel linking rule*)
    let rule_unikernel = sxp_of_fmt {|
      (rule
        (targets %s)
        (mode promote)
        (deps %s)
        (action (run %%{read-lines:ld} %%{read-lines:ldflags} %s -o %s)))
    |} name
      binary_location
      binary_location name
    in
    Ok [alias; rule_solo5_makefile; rule_solo5_hvt; rule_unikernel]
end

module Common_not_hvt = struct
  include Common

  let postbuild_rules _ ~alias_name ~name ~binary_location =
    (* Generate target alias *)
    let alias = sxp_of_fmt {|
      (alias
        (name %s)
        (enabled_if (= %%{context_name} "mirage-freestanding"))
        (deps %s))
    |} alias_name name
    in
    (* Generate unikernel linking rule*)
    let rule_unikernel = sxp_of_fmt {|
      (rule
        (targets %s)
        (mode promote)
        (deps %s)
        (action (run %%{read-lines:ld} %%{read-lines:ldflags} %s -o %s)))
    |} name
      binary_location
      binary_location name
    in
    Ok [alias; rule_unikernel]
end

module Virtio = struct
  include Common_not_hvt
  let alias_name = "virtio"

  let dependencies = package ~min:"0.4.0" ~max:"0.5.0" ~ocamlfind:[] "solo5-bindings-virtio" :: dependencies

  let generate_extra_files _ ~root ~name =
    let open Codegen in
    let file = Fpath.(v (name ^  "_libvirt") + "xml") in
    with_output file
      (fun oc () ->
        let fmt = Format.formatter_of_out_channel oc in
        append fmt "<!-- %s -->" (generated_header ());
        append fmt "<domain type='kvm'>";
        append fmt "    <name>%s</name>" name;
        append fmt "    <memory unit='KiB'>262144</memory>";
        append fmt "    <currentMemory unit='KiB'>262144</currentMemory>";
        append fmt "    <vcpu placement='static'>1</vcpu>";
        append fmt "    <os>";
        append fmt "        <type arch='x86_64' machine='pc'>hvm</type>";
        append fmt "        <kernel>%s/%s.virtio</kernel>" root name;
        append fmt "        <!-- Command line arguments can be given if required:";
        append fmt "        <cmdline>-l *:debug</cmdline>";
        append fmt "        -->";
        append fmt "    </os>";
        append fmt "    <clock offset='utc' adjustment='reset'/>";
        append fmt "    <devices>";
        append fmt "        <emulator>/usr/bin/qemu-system-x86_64</emulator>";
        append fmt "        <!--";
        append fmt "        Disk/block configuration reference is here:";
        append fmt "        https://libvirt.org/formatdomain.html#elementsDisks";
        append fmt "        This example uses a raw file on the host as a block in the guest:";
        append fmt "        <disk type='file' device='disk'>";
        append fmt "            <driver name='qemu' type='raw'/>";
        append fmt "            <source file='/var/lib/libvirt/images/%s.img'/>" name;
        append fmt "            <target dev='vda' bus='virtio'/>";
        append fmt "        </disk>";
        append fmt "        -->";
        append fmt "        <!-- ";
        append fmt "        Network configuration reference is here:";
        append fmt "        https://libvirt.org/formatdomain.html#elementsNICS";
        append fmt "        This example adds a device in the 'default' libvirt bridge:";
        append fmt "        <interface type='bridge'>";
        append fmt "            <source bridge='virbr0'/>";
        append fmt "            <model type='virtio'/>";
        append fmt "            <alias name='0'/>";
        append fmt "        </interface>";
        append fmt "        -->";
        append fmt "        <serial type='pty'>";
        append fmt "            <target port='0'/>";
        append fmt "        </serial>";
        append fmt "        <console type='pty'>";
        append fmt "            <target type='serial' port='0'/>";
        append fmt "        </console>";
        append fmt "        <memballoon model='none'/>";
        append fmt "    </devices>";
        append fmt "</domain>";
        R.ok ())
      "libvirt.xml"

  let clean_main_libvirt_xml ~name =
    Bos.OS.File.delete Fpath.(v (name ^ "_libvirt") + "xml")

  let clean = clean_main_libvirt_xml
end

module Muen = struct
  include Common_not_hvt
  let alias_name = "muen"
  let dependencies = package ~min:"0.4.0" ~max:"0.5.0" ~ocamlfind:[] "solo5-bindings-muen" :: dependencies
end

module Genode = struct
  include Common_not_hvt
  let alias_name = "genode"
  let dependencies = package ~min:"0.4.0" ~max:"0.5.0" ~ocamlfind:[] "solo5-bindings-genode" :: dependencies
end
