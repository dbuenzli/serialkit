open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let cmdliner = B0_ocaml.libname "cmdliner"
let serialk_text = B0_ocaml.libname "serialk.text"
let serialk_json = B0_ocaml.libname "serialk.json"
let serialk_sexp = B0_ocaml.libname "serialk.sexp"

(* Libraries *)

let mod_src m =
  let file m ext = Fpath.(v "src" / Fmt.str "%s%s" m ext) in
  [ `File (file m ".mli"); `File (file m ".ml") ]

let serialk_text_lib =
  let srcs = mod_src "serialk_text" in
  B0_ocaml.lib serialk_text ~doc:"Serialk text support" ~srcs ~requires:[]

let serialk_json_lib =
  let srcs = mod_src "serialk_json" in
  let requires = [serialk_text] in
  B0_ocaml.lib serialk_json ~doc:"Serialk JSON support" ~srcs ~requires

let serialk_sexp_lib =
  let srcs = mod_src "serialk_sexp" in
  let requires = [serialk_text] in
  B0_ocaml.lib serialk_sexp ~doc:"Serialk sexp support" ~srcs ~requires

(* Tools *)

let sexpsk_tool =
  let srcs = Fpath.[`File (v "test/sexpsk.ml")] in
  let requires = [cmdliner; serialk_text; serialk_sexp] in
  B0_ocaml.exe "sexpsk" ~doc:"sexpsk tool" ~srcs ~requires

(* Tests *)

let test_spec =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  let requires = [] in
  B0_ocaml.exe "test" ~doc:"Tests" ~srcs ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The serialk programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/serialk"
    |> add online_doc "https://erratique.ch/software/serialk/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/serialk.git"
    |> add issues "https://github.com/dbuenzli/serialk/issues"
    |> add description_tags
      ["codec"; "json"; "codec"; "query"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"serialk package" ~meta ~locked:true @@
  B0_unit.list ()
