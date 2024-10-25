Simple test
-----------

The `dune build` should generate the opam file

  $ cat >dune-project <<EOF
  > (lang dune 1.10)
  > (version 1.0.0)
  > (name cohttp)
  > (source (github mirage/ocaml-cohttp))
  > (license ISC)
  > (authors "Anil Madhavapeddy" "Rudi Grinberg")
  > 
  > (generate_opam_files true)
  > 
  > (package
  >  (name cohttp)
  >  (synopsis "An OCaml library for HTTP clients and servers")
  >  (description "A longer description")
  >  (depends
  >   (alcotest :with-test)
  >   (dune (and :build (> 1.5)))
  >   (foo (and :dev (> 1.5) (< 2.0)))
  >   (uri (>= 1.9.0))
  >   (uri (< 2.0.0))
  >   (fieldslib (> v0.12))
  >   (fieldslib (< v0.13))))
  > 
  > (package
  >  (name cohttp-async)
  >  (synopsis "HTTP client and server for the Async library")
  >  (description "\
  > A really long multi line description that spans across lines to
  > make sure that the rendering of long strings stays compatible.
  > ")
  >  (depends
  >   (cohttp (>= 1.0.2))
  >   (conduit-async (>= 1.0.3))
  >   (async (>= v0.10.0))
  >   (async (< v0.12))))
  > 
  > (package
  >  (name cohttp-lwt)
  >  (synopsis "HTTP client and server for the Lwt library")
  >  (description "
  > A really long description that is supposed to start with a
  > newline since it doesn't escape the line break.
  > ")
  >  (depends
  >   (cohttp (>= 1.0.2))
  >   (conduit-lwt (>= 1.0.3))
  >   (lwt (>= 5.0.0))
  >   (lwt (< 6.0.0))))
  > EOF

  $ dune build @install

  $ cat cohttp.opam
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  build: [
    ["dune" "subst"] {pinned}
    ["dune" "build" "-p" name "-j" jobs]
    ["dune" "runtest" "-p" name "-j" jobs] {with-test}
    ["dune" "build" "-p" name "@doc"] {with-doc}
  ]
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  bug-reports: "https://github.com/mirage/ocaml-cohttp/issues"
  homepage: "https://github.com/mirage/ocaml-cohttp"
  license: "ISC"
  version: "1.0.0"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  synopsis: "An OCaml library for HTTP clients and servers"
  description: "A longer description"
  depends: [
    "alcotest" {with-test}
    "dune" {build & > "1.5"}
    "foo" {dev & > "1.5" & < "2.0"}
    "uri" {>= "1.9.0"}
    "uri" {< "2.0.0"}
    "fieldslib" {> "v0.12"}
    "fieldslib" {< "v0.13"}
  ]

  $ cat cohttp-async.opam
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  build: [
    ["dune" "subst"] {pinned}
    ["dune" "build" "-p" name "-j" jobs]
    ["dune" "runtest" "-p" name "-j" jobs] {with-test}
    ["dune" "build" "-p" name "@doc"] {with-doc}
  ]
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  bug-reports: "https://github.com/mirage/ocaml-cohttp/issues"
  homepage: "https://github.com/mirage/ocaml-cohttp"
  license: "ISC"
  version: "1.0.0"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  synopsis: "HTTP client and server for the Async library"
  description: """
  A really long multi line description that spans across lines to
  make sure that the rendering of long strings stays compatible.
  """
  depends: [
    "cohttp" {>= "1.0.2"}
    "conduit-async" {>= "1.0.3"}
    "async" {>= "v0.10.0"}
    "async" {< "v0.12"}
  ]

  $ cat cohttp-lwt.opam
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  build: [
    ["dune" "subst"] {pinned}
    ["dune" "build" "-p" name "-j" jobs]
    ["dune" "runtest" "-p" name "-j" jobs] {with-test}
    ["dune" "build" "-p" name "@doc"] {with-doc}
  ]
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  bug-reports: "https://github.com/mirage/ocaml-cohttp/issues"
  homepage: "https://github.com/mirage/ocaml-cohttp"
  license: "ISC"
  version: "1.0.0"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  synopsis: "HTTP client and server for the Lwt library"
  description: """
  
  A really long description that is supposed to start with a
  newline since it doesn't escape the line break.
  """
  depends: [
    "cohttp" {>= "1.0.2"}
    "conduit-lwt" {>= "1.0.3"}
    "lwt" {>= "5.0.0"}
    "lwt" {< "6.0.0"}
  ]
