(* *)

include (module type of Typerep_kernel.Type.Std.Typerep
  with type 'a t = 'a Typerep_kernel.Type.Rep.t
  and type 'a Named.t = 'a Typerep_kernel.Type.Rep.Named.t
  and type 'a Tuple.t = 'a Typerep_kernel.Type.Rep.Tuple.t
  and type 'a Record.t = 'a Typerep_kernel.Type.Rep.Record.t
  and type 'a Variant.t = 'a Typerep_kernel.Type.Rep.Variant.t
)
