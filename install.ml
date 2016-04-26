#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"typerep"
  [ oasis_lib "typerep_lib"
  ; file "META" ~section:"lib"
  ]
