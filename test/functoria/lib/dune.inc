
(rule
  (target configure)
  (action
    (with-stdout-to configure
    (with-stderr-to configure.err
      (run ./config.exe configure --dry-run)))))

(rule
  (alias runtest)
  (package functoria)
  (action (diff configure.expected configure)))

(rule
  (alias runtest)
  (package functoria)
  (action (diff configure.err.expected configure.err)))

(rule
  (target build)
  (action
    (with-stdout-to build
    (with-stderr-to build.err
      (run ./config.exe build --dry-run)))))

(rule
  (alias runtest)
  (package functoria)
  (action (diff build.expected build)))

(rule
  (alias runtest)
  (package functoria)
  (action (diff build.err.expected build.err)))

(rule
  (target clean)
  (action
    (with-stdout-to clean
    (with-stderr-to clean.err
      (run ./config.exe clean --dry-run)))))

(rule
  (alias runtest)
  (package functoria)
  (action (diff clean.expected clean)))

(rule
  (alias runtest)
  (package functoria)
  (action (diff clean.err.expected clean.err)))

(rule
  (target query)
  (action
    (with-stdout-to query
    (with-stderr-to query.err
      (run ./config.exe query --dry-run)))))

(rule
  (alias runtest)
  (package functoria)
  (action (diff query.expected query)))

(rule
  (alias runtest)
  (package functoria)
  (action (diff query.err.expected query.err)))

(rule
  (target describe)
  (action
    (with-stdout-to describe
    (with-stderr-to describe.err
      (run ./config.exe describe --dry-run)))))

(rule
  (alias runtest)
  (package functoria)
  (action (diff describe.expected describe)))

(rule
  (alias runtest)
  (package functoria)
  (action (diff describe.err.expected describe.err)))

(rule
  (target help)
  (action
    (with-stdout-to help
    (with-stderr-to help.err
      (run ./config.exe help --dry-run)))))

(rule
  (alias runtest)
  (package functoria)
  (action (diff help.expected help)))

(rule
  (alias runtest)
  (package functoria)
  (action (diff help.err.expected help.err)))
