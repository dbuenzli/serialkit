open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let cmdliner = B0_ocaml.libname "cmdliner"
let serialkit = B0_ocaml.libname "serialkit"

(* Libraries *)

let serialkit_lib =
  let srcs = [ `Dir (Fpath.v "src") ] in
  let name = "serialkit-lib" in
  B0_ocaml.lib ~name serialkit ~doc:"serialkit library" ~srcs ~requires:[]

(* Tools *)

let serialkit_tool =
  let srcs = Fpath.[`Dir (v "tool")] in
  let requires = [cmdliner; serialkit] in
  B0_ocaml.exe "serialkit" ~public:true ~doc:"serialkit tool" ~srcs ~requires

(* Tests *)

let test_spec =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  let requires = [serialkit] in
  B0_ocaml.exe "test" ~doc:"Tests" ~srcs ~requires

let test_toml =
  let srcs = Fpath.[`File (v "test/test_toml.ml")] in
  let requires = [serialkit] in
  B0_ocaml.exe "test-toml" ~doc:"TOML Tests" ~srcs ~requires

(* Expectation tests *)

let expect_serialk_runs ctx =
  let runs cmd = (* command, output suffix *)
    [ Cmd.(arg cmd % "locs"), ".locs"; ]
  in
  let test_run ctx serialk file (cmd, ext) =
    let cwd = B0_expect.base ctx and stdout = Fpath.(file -+ ext) in
    B0_expect.stdout ctx ~cwd ~stdout Cmd.(serialk %% cmd)
  in
  let test_file ctx serialk file =
    let cmd = String.subrange ~first:1 (Fpath.get_ext ~multi:false file) in
    List.iter (test_run ctx serialk file) (runs cmd)
  in
  let serialk = B0_expect.get_unit_exe_file_cmd ctx serialkit_tool in
  let test_files =
    let base_files = B0_expect.base_files ctx ~rel:true ~recurse:false in
    let input f = match Fpath.get_ext ~multi:true f with
    | ".json" | ".sexp" | ".toml" | ".cbor" | ".xml" -> true
    | _ -> false
    in
    List.filter input base_files
  in
  List.iter (test_file ctx serialk) test_files

let expect =
  B0_unit.of_action'
    "expect" ~units:[serialkit_tool] ~doc:"Test expectations" @@
  B0_expect.action_func ~base:(Fpath.v "test/expect") @@ fun ctx ->
  expect_serialk_runs ctx;
  ()

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The serialkit programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/serialkit"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/serialkit/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/serialkit.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/serialkit/issues"
    |> ~~ B0_meta.description_tags
      ["codec"; "json"; "sexp"; "toml"; "query"; "org:erratique"; ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> ~~ B0_opam.depends [
      "ocaml", {|>= "4.14.0"|};
      "ocamlfind", {|build|};
      "ocamlbuild", {|build|};
      "topkg", {|build & >= "1.1.0"|};
      "cmdliner", {|>= "1.1.0"|}]
    |> B0_meta.tag B0_release.tag
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"serialkit package" ~meta ~locked:true @@
  B0_unit.list ()
