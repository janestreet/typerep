(*
  This module is there to enforce that we register at the same time a customization of
  serialization. This takes care of registering the implementation in the right tables.
*)

val register0 : (module Customrep_intf.S0) -> unit
val register1 : (module Customrep_intf.S1) -> unit
val register2 : (module Customrep_intf.S2) -> unit
