#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "serialk" @@ fun c ->
  Ok [ Pkg.mllib "src/sk_tlex.mllib";
       Pkg.mllib "src/sk_json.mllib";
       Pkg.mllib "src/sk_sexp.mllib";
       Pkg.test "test/test"; ]
