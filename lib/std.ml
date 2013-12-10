include Typerep_core.Std
include Typerep_sexp.Std
include Typerep_bin_io.Std
module Farray = Pre_core.Std.Farray
(*module Customrep_intf = Customrep_intf
module Customrep = Customrep*)
(* Mega-Hack because of applicative name comparison in ocaml first class module *)
module Lib_customrep_intf = Customrep_intf
let s0_to_s0 (module M : Lib_customrep_intf.S0) = (module M : Customrep_intf.S0)
let s1_to_s1 (module M : Lib_customrep_intf.S1) = (module M : Customrep_intf.S1)
let s2_to_s2 (module M : Lib_customrep_intf.S2) = (module M : Customrep_intf.S2)
module Customrep_intf = Customrep_intf
module Customrep : sig
  val register0 : (module Customrep_intf.S0) -> unit
  val register1 : (module Customrep_intf.S1) -> unit
  val register2 : (module Customrep_intf.S2) -> unit
end = struct
  include Customrep
  let register0 (module M : Customrep_intf.S0) =
    Customrep.register0 (s0_to_s0 (module M : Lib_customrep_intf.S0))
  let register1 (module M : Customrep_intf.S1) =
    Customrep.register1 (s1_to_s1 (module M : Lib_customrep_intf.S1))
  let register2 (module M : Customrep_intf.S2) =
    Customrep.register2 (s2_to_s2 (module M : Lib_customrep_intf.S2))
end
(* end of Mega-Hack *)
