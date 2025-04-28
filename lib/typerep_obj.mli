open! Base
open Std_internal

(** some utils related to the runtime of ocaml, used both at compile time (camlp4) and
    runtime. to be considered the same way than [Obj] (internal, unsafe, etc.) *)
val repr_of_poly_variant : [> ] -> int

val hash_variant : string -> int
val double_array_value : unit -> 'a

(** creates a dummy value for typereps for unboxed types. *)
val double_array_non_value : 'any. 'any Typerep.t_non_value -> unit -> 'any

val has_double_array_tag : 'a -> bool
