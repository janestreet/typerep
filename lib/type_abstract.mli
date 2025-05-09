@@ portable

open! Base

(** Abstract types helpers.

    An abstract type in the sense of the typerep library is a type whose representation is
    unknown. Such a type has only a name that can be used to provide and register custom
    implementation of generics. This is typically a type obtained with the following
    syntax extension:

    {[
      type t with typerep(abstract)
    ]}

    The following functors are meant to be used by the code generator, however they could
    also be useful while writing low level typerep code manually. *)

module Make0 (X : sig
    type t

    include Named_intf.S0 with type t := t
  end) : Typerepable.S with type t := X.t

module Make1 (X : sig
    type 'a t

    include Named_intf.S1 with type 'a t := 'a t
  end) : Typerepable.S1 with type 'a t := 'a X.t

module Make2 (X : sig
    type ('a, 'b) t

    include Named_intf.S2 with type ('a, 'b) t := ('a, 'b) t
  end) : Typerepable.S2 with type ('a, 'b) t := ('a, 'b) X.t

module Make3 (X : sig
    type ('a, 'b, 'c) t

    include Named_intf.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  end) : Typerepable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t

module Make4 (X : sig
    type ('a, 'b, 'c, 'd) t

    include Named_intf.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
  end) : Typerepable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) X.t

module Make5 (X : sig
    type ('a, 'b, 'c, 'd, 'e) t

    include Named_intf.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
  end) : Typerepable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) X.t
