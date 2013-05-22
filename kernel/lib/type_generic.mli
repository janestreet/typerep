(** A computation is the type of an operation that can be applied to various different
    kind of types.  It is expressed as a type with one parameter:

    type 'a computation

    Examples of computation:

    type sexp_of_t = ('a -> Sexp.t) computation
    type type_struct = Type_struct.t computation

    The term [generic] is used to refer to a specific implementation of a computation
    whose concrete implementation is programmed using the type representation of values.

    For example, when one uses [with sexp] as a way to implement the [sexp_of_t]
    computation, the technique used is code generation at compile time.  Another approach
    is to define a generic function [sexp_of_t] that inspects the representation of the
    type at runtime.

    This module offers an abstraction over type rep in order to implement generics in a
    efficient way.

    Provided from a user enough pieces of implementation regarding a particular
    computation, this module returns essentially the following function:

    (** main function : get the computation from the typerep *)
    val of_typerep : 'a Type.Rep.t -> [ `generic of 'a computation ]

    that allows one to get the generic computation operating on a given type ['a].
*)

module Helper (A : Intf.S) (B : Intf.S) : sig
  type map = { map : 'a. 'a A.t -> 'a B.t }
  val map_variant : map -> 'a A.Variant.t -> 'a B.Variant.t
  val map_record : map -> 'a A.Record.t -> 'a B.Record.t
end

module type Named = sig
  type 'a computation
  module Context : sig
    type t
    val create : unit -> t
  end
  type 'a t
  val init : Context.t -> 'a Type_name.t -> 'a t
  val get_wip_computation : 'a t -> 'a computation
  val set_final_computation : 'a t -> 'a computation -> 'a computation
  val share : _ Type.Rep.t -> bool
end

module type Computation = sig
  type 'a t

  include Intf.S with type 'a t := 'a t

  val int : int t
  val int32 : int32 t
  val int64 : int64 t
  val char : char t
  val float : float t
  val string : string t
  val bool : bool t
  val unit : unit t
  val option : 'a t -> 'a option t
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
  val lazy_t : 'a t -> 'a lazy_t t
  val ref_ : 'a t -> 'a ref t
  val function_ : 'a t -> 'b t -> ('a -> 'b) t
  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  val record : 'a Record.t -> 'a t
  val variant : 'a Variant.t -> 'a t

  module Named : Named with type 'a computation := 'a t
end

module Make_named_for_closure (X : sig
  type 'a input
  type 'a output
  type 'a t = 'a input -> 'a output
end) : Named with type 'a computation := 'a X.t

module Ident : sig
  type t
end

(* Extending an existing generic *)
module type S = sig

  type 'a t
  type 'a computation = 'a t

  val ident : Ident.t

  (** generic_ident * typename or info *)
  exception Not_implemented of string * string

  (** register mechanism to customize the behavior of this generic *)
  include Type_generic_intf.S with type 'a t := 'a t

  val register0 : (module S0) -> unit
  val register1 : (module S1) -> unit
  val register2 : (module S2) -> unit
  val register3 : (module S3) -> unit
  val register4 : (module S4) -> unit
  val register5 : (module S5) -> unit

  (* special less scary type when the type has no parameters *)
  val register : 'a Type.Rep.t -> 'a computation -> unit

  (*
    Essentially because we cannot talk about a variable of kind * -> k
    val register1 : 'a 't Type.Rep.t -> ('a computation -> 'a 't computation) -> unit
    ...
  *)

  (** main function : compute the generic computation from the typerep *)
  val of_typerep : 'a Type.Rep.t -> [ `generic of 'a computation ]

  (** exported to build a computation on top of a previous one *)
  module Computation : Computation with type 'a t = 'a t
end

(*
  The [name] is used for debug information only in case of Broken_dependency.
  The [required] is to handle dependencies between generics at runtime.
*)
module Make (X : sig
  type 'a t
  val name : string
  val required : Ident.t list
  include Computation
  with type 'a t := 'a t
end) : S with type 'a t = 'a X.t
