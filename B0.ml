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

let serialk_tool =
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

let get_expect_exe exe = (* FIXME b0 *)
  B0_expect.result_to_abort @@
  let expect = Cmd.(arg "b0" % "--path" % "--" % exe) in
  Result.map Cmd.arg (Os.Cmd.run_out ~trim:true expect)

let expect_serialk_runs ctx =
  let runs cmd = (* command, output suffix *)
    [ Cmd.(arg cmd % "locs"), ".locs"; ]
  in
  let test_run ctx serialk file (cmd, ext) =
    let cwd = B0_expect.base ctx and stdout = Fpath.(file -+ ext) in
    B0_expect.stdout ctx ~cwd ~stdout Cmd.(serialk %% cmd)
  in
  let test_file ctx serialk file =
    let cmd = String.subrange ~first:1 (Fpath.get_ext file) in
    List.iter (test_run ctx serialk file) (runs cmd)
  in
  let serialk = get_expect_exe "serialkit" in
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
  B0_action.make "expect" ~doc:"Test expectations" @@
  B0_expect.action_func ~base:(Fpath.v "test/expect") @@ fun ctx ->
  expect_serialk_runs ctx;
  ()

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The serialkit programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/serialkit"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/serialkit/doc"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/serialkit.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/serialkit/issues"
    |> B0_meta.(add description_tags)
      ["codec"; "json"; "sexp"; "toml"; "query"; "org:erratique"; ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.make "default" ~doc:"serialkit package" ~meta ~locked:true @@
  B0_unit.list ()
