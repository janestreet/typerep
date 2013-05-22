(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | Before_options ->
    Options.make_links := false
  | After_rules ->
    let tag = "pa_typerep" and file = "syntax/with_typerep/pa_typerep_conv.cmo" in
    flag ["ocaml"; "compile"; tag] & S[A"-ppopt"; A file];
    flag ["ocaml"; "ocamldep"; tag] & S[A"-ppopt"; A file];
    flag ["ocaml"; "doc"; tag] & S[A"-ppopt"; A file];
    dep ["ocaml"; "ocamldep"; tag] [file]
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
