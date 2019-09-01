#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "serialk" @@ fun c ->
  Ok [ Pkg.mllib "src/serialk_text.mllib";
       Pkg.mllib "src/serialk_json.mllib";
       Pkg.mllib "src/serialk_sexp.mllib";
       Pkg.test "test/test";
       Pkg.test "test/sexpsk" ]
