open Typerep_kernel.Std
open Typerep_core.Std
open Pre_core.Std

module Of_sexp : sig
  exception Type_mismatch of string * Sexp.t
  include Type_generic.S with type 'a t = Sexp.t -> 'a
end
module Sexp_of : Type_generic.S with type 'a t = 'a -> Sexp.t

val t_of_sexp : 'a Typerep.t -> [`generic of Sexp.t -> 'a]
val sexp_of_t : 'a Typerep.t -> [`generic of 'a -> Sexp.t]

module Make_sexpable(X:Typerepable.S0) : Sexpable.S
  with type t := X.t
val make_sexpable : 'a Typerep.t -> (module Sexpable.S with type t = 'a)

module Tagged : sig
  module Of_sexp : Tagged_generic.S with type 'a t = Sexp.t -> 'a
  module Sexp_of : Tagged_generic.S with type 'a t = 'a -> Sexp.t

  val t_of_sexp : Type_struct.t -> [ `generic of Sexp.t -> Tagged.t ]
  val sexp_of_t : Type_struct.t -> [ `generic of Tagged.t -> Sexp.t ]

  module Make_sexpable(X:sig val typestruct_of_t : Type_struct.t end) : Sexpable.S
    with type t := Tagged.t
  val make_sexpable : Type_struct.t -> (module Sexpable.S with type t = Tagged.t)
end
