open Functoria
open Mirage_backend_common
open Astring
open Mirage_impl_misc
open Rresult

module Codegen = Functoria_app.Codegen

let dependencies = [ package ~pin:"git+https://github.com/TheLortex/mirage-xen.git#impl-mirage-os-shim" "mirage-xen" ]

let alias_name = "xen"

let custom_runtime = Some "xen"

let variant = "xen"

let compilation_mode = Object

let ocaml_compilation_flags = []

let ocaml_link_flags = ["(:include libs)"]

let extra_context = Some (sxp_of_fmt {|
    (context (default
      (name mirage-xen)
      (host default)
      (env (_
        (c_flags (:include cflags))
      ))))
  |})


let configure_main_libvirt_xml ~root ~name =
  let open Codegen in
  let file = Fpath.(v (name ^  "_libvirt") + "xml") in
  with_output file
    (fun oc () ->
       let fmt = Format.formatter_of_out_channel oc in
       append fmt "<!-- %s -->" (generated_header ());
       append fmt "<domain type='xen'>";
       append fmt "    <name>%s</name>" name;
       append fmt "    <memory unit='KiB'>262144</memory>";
       append fmt "    <currentMemory unit='KiB'>262144</currentMemory>";
       append fmt "    <vcpu placement='static'>1</vcpu>";
       append fmt "    <os>";
       append fmt "        <type arch='armv7l' machine='xenpv'>linux</type>";
       append fmt "        <kernel>%s/%s.xen</kernel>" root name;
       append fmt "        <cmdline> </cmdline>";
       (* the libxl driver currently needs an empty cmdline to be able to
           start the domain on arm - due to this?
           http://lists.xen.org/archives/html/xen-devel/2014-02/msg02375.html *)
       append fmt "    </os>";
       append fmt "    <clock offset='utc' adjustment='reset'/>";
       append fmt "    <on_crash>preserve</on_crash>";
       append fmt "    <!-- ";
       append fmt "    You must define network and block interfaces manually.";
       append fmt "    See http://libvirt.org/drvxen.html for information about \
                   converting .xl-files to libvirt xml automatically.";
       append fmt "    -->";
       append fmt "    <devices>";
       append fmt "        <!--";
       append fmt "        The disk configuration is defined here:";
       append fmt "        http://libvirt.org/formatstorage.html.";
       append fmt "        An example would look like:";
       append fmt"         <disk type='block' device='disk'>";
       append fmt "            <driver name='phy'/>";
       append fmt "            <source dev='/dev/loop0'/>";
       append fmt "            <target dev='' bus='xen'/>";
       append fmt "        </disk>";
       append fmt "        -->";
       append fmt "        <!-- ";
       append fmt "        The network configuration is defined here:";
       append fmt "        http://libvirt.org/formatnetwork.html";
       append fmt "        An example would look like:";
       append fmt "        <interface type='bridge'>";
       append fmt "            <mac address='c0:ff:ee:c0:ff:ee'/>";
       append fmt "            <source bridge='br0'/>";
       append fmt "        </interface>";
       append fmt "        -->";
       append fmt "        <console type='pty'>";
       append fmt "            <target type='xen' port='0'/>";
       append fmt "        </console>";
       append fmt "    </devices>";
       append fmt "</domain>";
       R.ok ())
    "libvirt.xml"


(* We generate an example .xl with common defaults, and a generic
   .xl.in which has @VARIABLES@ which must be substituted by sed
   according to the preferences of the system administrator.

   The common defaults chosen for the .xl file will be based on values
   detected from the build host. We assume that the .xl file will
   mainly be used by developers where build and deployment are on the
   same host. Production users should use the .xl.in and perform the
   appropriate variable substition.
*)

let detected_bridge_name =
  (* Best-effort guess of a bridge name stem to use. Note this
     inspects the build host and will probably be wrong if the
     deployment host is different.  *)
  match
    List.fold_left (fun sofar x ->
        match sofar with
        (* This is Linux-specific *)
        | None when Sys.file_exists (Fmt.strf "/sys/class/net/%s0" x) -> Some x
        | None -> None
        | Some x -> Some x)
      None [ "xenbr"; "br"; "virbr" ]
  with
  | Some x -> x
  | None -> "br"


module Substitutions = struct
  type v =
    | Name
    | Kernel
    | Memory
    | Block of Mirage_impl_block.block_t
    | Network of string

  let string_of_v = function
    | Name -> "@NAME@"
    | Kernel -> "@KERNEL@"
    | Memory -> "@MEMORY@"
    | Block b -> Fmt.strf "@BLOCK:%s@" b.filename
    | Network n -> Fmt.strf "@NETWORK:%s@" n

  let lookup ts v =
    if List.mem_assoc v ts then
      List.assoc v ts
    else
      string_of_v v

  let defaults i =
    let blocks =
      List.map
        (fun b -> Block b, Fpath.(to_string ((Info.build_dir i) / b.filename)))
        (Hashtbl.fold (fun _ v acc -> v :: acc) Mirage_impl_block.all_blocks [])
    and networks =
      List.mapi (fun i n -> Network n, Fmt.strf "%s%d" detected_bridge_name i)
        !Mirage_impl_network.all_networks
    in [
      Name, (Info.name i);
      Kernel, Fpath.(to_string ((Info.build_dir i) / (Info.name i) + "xen"));
      Memory, "256";
    ] @ blocks @ networks
end


let configure_main_xl ?substitutions ext i =
  let open Substitutions in
  let substitutions = match substitutions with
    | Some x -> x
    | None -> defaults i in
  let file = Fpath.(v (Info.name i) + ext) in
  let open Codegen in
  with_output file (fun oc () ->
    let open Mirage_impl_block in
    let fmt = Format.formatter_of_out_channel oc in
    append fmt "# %s" (generated_header ()) ;
    newline fmt;
    append fmt "name = '%s'" (lookup substitutions Name);
    append fmt "kernel = '%s'" (lookup substitutions Kernel);
    append fmt "builder = 'linux'";
    append fmt "memory = %s" (lookup substitutions Memory);
    append fmt "on_crash = 'preserve'";
    newline fmt;
    let blocks = List.map
        (fun b ->
            (* We need the Linux version of the block number (this is a
              strange historical artifact) Taken from
              https://github.com/mirage/mirage-block-xen/blob/
              a64d152586c7ebc1d23c5adaa4ddd440b45a3a83/lib/device_number.ml#L128 *)
            let rec string_of_int26 x =
              let high, low = x / 26 - 1, x mod 26 + 1 in
              let high' = if high = -1 then "" else string_of_int26 high in
              let low' =
                String.v ~len:1
                  (fun _ -> char_of_int (low + (int_of_char 'a') - 1))
              in
              high' ^ low' in
            let vdev = Fmt.strf "xvd%s" (string_of_int26 b.number) in
            let path = lookup substitutions (Block b) in
            Fmt.strf "'format=raw, vdev=%s, access=rw, target=%s'" vdev path)
        (Hashtbl.fold (fun _ v acc -> v :: acc) all_blocks [])
    in
    append fmt "disk = [ %s ]" (String.concat ~sep:", " blocks);
    newline fmt;
    let networks = List.map (fun n ->
        Fmt.strf "'bridge=%s'" (lookup substitutions (Network n)))
        !Mirage_impl_network.all_networks
    in
    append fmt "# if your system uses openvswitch then either edit \
                /etc/xen/xl.conf and set";
    append fmt "#     vif.default.script=\"vif-openvswitch\"";
    append fmt "# or add \"script=vif-openvswitch,\" before the \"bridge=\" \
                below:";
    append fmt "vif = [ %s ]" (String.concat ~sep:", " networks);
    R.ok ())
  "xl file"

let configure_main_xe ~root ~name =
  let open Codegen in
  let file = Fpath.(v name + "xe") in
  with_output ~mode:0o755 file (fun oc () ->
    let fmt = Format.formatter_of_out_channel oc in
    let open Mirage_impl_block in
    append fmt "#!/bin/sh";
    append fmt "# %s" (generated_header ());
    newline fmt;
    append fmt "set -e";
    newline fmt;
    append fmt "# Dependency: xe";
    append fmt "command -v xe >/dev/null 2>&1 || { echo >&2 \"I require xe but \
                it's not installed.  Aborting.\"; exit 1; }";
    append fmt "# Dependency: xe-unikernel-upload";
    append fmt "command -v xe-unikernel-upload >/dev/null 2>&1 || { echo >&2 \"I \
                require xe-unikernel-upload but it's not installed.  Aborting.\"\
                ; exit 1; }";
    append fmt "# Dependency: a $HOME/.xe";
    append fmt "if [ ! -e $HOME/.xe ]; then";
    append fmt "  echo Please create a config file for xe in $HOME/.xe which \
                contains:";
    append fmt "  echo server='<IP or DNS name of the host running xapi>'";
    append fmt "  echo username=root";
    append fmt "  echo password=password";
    append fmt "  exit 1";
    append fmt "fi";
    newline fmt;
    append fmt "echo Uploading VDI containing unikernel";
    append fmt "VDI=$(xe-unikernel-upload --path %s/%s.xen)" root name;
    append fmt "echo VDI=$VDI";
    append fmt "echo Creating VM metadata";
    append fmt "VM=$(xe vm-create name-label=%s)" name;
    append fmt "echo VM=$VM";
    append fmt "xe vm-param-set uuid=$VM PV-bootloader=pygrub";
    append fmt "echo Adding network interface connected to xenbr0";
    append fmt "ETH0=$(xe network-list bridge=xenbr0 params=uuid --minimal)";
    append fmt "VIF=$(xe vif-create vm-uuid=$VM network-uuid=$ETH0 device=0)";
    append fmt "echo Atting block device and making it bootable";
    append fmt "VBD=$(xe vbd-create vm-uuid=$VM vdi-uuid=$VDI device=0)";
    append fmt "xe vbd-param-set uuid=$VBD bootable=true";
    append fmt "xe vbd-param-set uuid=$VBD other-config:owner=true";
    List.iter (fun b ->
        append fmt "echo Uploading data VDI %s" b.filename;
        append fmt "echo VDI=$VDI";
        append fmt "SIZE=$(stat --format '%%s' %s/%s)" root b.filename;
        append fmt "POOL=$(xe pool-list params=uuid --minimal)";
        append fmt "SR=$(xe pool-list uuid=$POOL params=default-SR --minimal)";
        append fmt "VDI=$(xe vdi-create type=user name-label='%s' \
                    virtual-size=$SIZE sr-uuid=$SR)" b.filename;
        append fmt "xe vdi-import uuid=$VDI filename=%s/%s" root b.filename;
        append fmt "VBD=$(xe vbd-create vm-uuid=$VM vdi-uuid=$VDI device=%d)"
          b.number;
        append fmt "xe vbd-param-set uuid=$VBD other-config:owner=true")
      (Hashtbl.fold (fun _ v acc -> v :: acc) all_blocks []);
    append fmt "echo Starting VM";
    append fmt "xe vm-start uuid=$VM";
    R.ok ())
  "xe file"

let generate_extra_files i ~root ~name =
  configure_main_xl "xl" i >>= fun () ->
  configure_main_xl ~substitutions:[] "xl.in" i >>= fun () ->
  configure_main_xe ~root ~name >>= fun () ->
  configure_main_libvirt_xml ~root ~name

(* On ARM:
        - we must convert the ELF image to an ARM boot executable zImage,
          while on x86 we leave it as it is.
        - we need to link libgcc.a (otherwise we get undefined references to:
          __aeabi_dcmpge, __aeabi_dadd, ...) *)
let config_xen_arm ~out ~linker_command ~binary_location =
  let elf = out ^ ".elf" in
  let libgcc_cmd = Bos.Cmd.(v "gcc" % "-print-libgcc-file-name") in
  Bos.OS.Cmd.(run_out libgcc_cmd |> out_string) >>= fun (libgcc, _) ->
  let rule_link = sxp_of_fmt {|
    (rule
      (targets %s)
      (deps %s)
      (action (run %s %s -o %s)))
    |} elf binary_location linker_command libgcc elf
  and rule_obj_copy = sxp_of_fmt {|
    (rule
      (mode promote)
      (targets %s)
      (deps %s)
      (action (run objcopy -O binary %s %s)))
  |} out elf elf out
  in
  Ok [rule_link; rule_obj_copy]

let config_xen_default ~out ~linker_command ~binary_location =
  let rule = sxp_of_fmt {|
    (rule
      (mode promote)
      (targets %s)
      (deps %s)
      (action (run %s -o %s)))
  |} out binary_location linker_command out
  in
  Ok [rule]

let config_xen ~alias_name ~name ~binary_location =
  let out = name ^ ".xen" in
  let alias = sxp_of_fmt {|
    (alias
      (name %s)
      (enabled_if (= %%{context_name} "mirage-xen"))
      (deps %s))
  |} alias_name out
  in
  let linker_command = "ld -d -static -nostdlib "^binary_location
  in
  let uname_cmd = Bos.Cmd.(v "uname" % "-m") in
  Bos.OS.Cmd.(run_out uname_cmd |> out_string) >>= fun (machine, _) ->
  match String.is_prefix ~affix:"arm" machine with
  | true -> config_xen_arm ~out ~linker_command ~binary_location
  | false -> config_xen_default ~out ~linker_command ~binary_location
  >>= fun rules ->
  let rule_libs = sxp_of_fmt "(rule (copy %%{lib:mirage-xen-ocaml:libs} libs))"
  in
  let rule_cflags = sxp_of_fmt "(rule (copy %%{lib:mirage-xen-ocaml:cflags} cflags))"
  in
  Ok (rule_cflags::rule_libs::alias::rules)

let postbuild_rules _ = config_xen

let clean_main_xl ~name ext = Bos.OS.File.delete Fpath.(v name + ext)
let clean_main_xe ~name = Bos.OS.File.delete Fpath.(v name + "xe")

let clean ~name =
  clean_main_xl ~name "xl" >>= fun () ->
  clean_main_xl ~name "xl.in" >>= fun () ->
  clean_main_xe ~name