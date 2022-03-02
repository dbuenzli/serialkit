#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "serialkit" @@ fun c ->
  Ok [ Pkg.mllib "src/serialkit.mllib";
       Pkg.test "test/test";
       Pkg.bin "tool/cmd_main" ~dst:"serialkit" ]
