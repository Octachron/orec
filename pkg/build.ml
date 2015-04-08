#!/usr/bin/env ocaml 
#directory "pkg"
#use "topkg.ml"

let () = 
  Pkg.describe "orec" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/orec";
    Pkg.lib ~exts:Exts.module_library "src/orec_modules";
    Pkg.doc "Readme.md"; 
]
