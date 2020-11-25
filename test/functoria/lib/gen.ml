type t = { args : string; file : string }

let v file args = { args; file }

let cmd s = v s s

let gen t =
  Printf.printf
    {|
(rule
  (target %s)
  (action
    (with-stdout-to %s
    (with-stderr-to %s.err
      (run ./config.exe %s --dry-run)))))

(rule
  (alias runtest)
  (package functoria)
  (action (diff %s.expected %s)))

(rule
  (alias runtest)
  (package functoria)
  (action (diff %s.err.expected %s.err)))
|}
    t.file t.file t.file t.args t.file t.file t.file t.file

let () =
  List.iter gen
    [
      cmd "configure";
      cmd "build";
      cmd "clean";
      cmd "query";
      cmd "describe";
      cmd "help";
    ]
