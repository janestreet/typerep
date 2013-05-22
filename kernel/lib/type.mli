(** type-safe runtime type introspection *)

(** runtime type representations *)
module rec Rep : sig

  type _ t =
  | Int      : int t
  | Int32    : int32 t
  | Int64    : int64 t
  | Char     : char t
  | Float    : float t
  | String   : string t
  | Bool     : bool t
  | Unit     : unit t
  | Option   : 'a t -> 'a option t
  | List     : 'a t -> 'a list t
  | Array    : 'a t -> 'a array t
  | Lazy     : 'a t -> 'a Lazy.t t
  | Ref      : 'a t -> 'a ref t
  | Function : ('dom t * 'rng t) -> ('dom -> 'rng) t
  | Tuple    : 'a Rep.Tuple.t -> 'a t
  | Record   : 'a Rep.Record.t -> 'a t
  | Variant  : 'a Rep.Variant.t -> 'a t
    (** The [Named] constructor both allows for custom implementations of generics
        based on name and provides a way to represent recursive types, the lazy
        part dealing with cycles *)
  | Named    : ('a Rep.Named.t * 'a t Lazy.t option) -> 'a t

  module Named : sig
    module type T0 = sig
      type named
      type t
      val typename_of_named : named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, named) Type_equal.t
    end
    module type T1 = sig
      type 'a named
      type a val a : a Rep.t
      type t
      val typename_of_named : 'a Type_name.t -> 'a named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, a named) Type_equal.t
    end
    module type T2 = sig
      type ('a, 'b) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> ('a, 'b) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b) named) Type_equal.t
    end
    module type T3 = sig
      type ('a, 'b, 'c) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> ('a, 'b, 'c) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end
    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type d val d : d Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> 'd Type_name.t
        -> ('a, 'b, 'c, 'd) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end
    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type d val d : d Rep.t
      type e val e : e Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> 'd Type_name.t
        -> 'e Type_name.t
        -> ('a, 'b, 'c, 'd, 'e) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end
    type 'a t =
    | T0 of (module T0 with type t = 'a)
    | T1 of (module T1 with type t = 'a)
    | T2 of (module T2 with type t = 'a)
    | T3 of (module T3 with type t = 'a)
    | T4 of (module T4 with type t = 'a)
    | T5 of (module T5 with type t = 'a)

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Type_name.t
    val name : _ t -> string
  end

  module Tuple : sig
    type _ t =
    | T2 : ('a Rep.t * 'b Rep.t)
      -> ('a * 'b) t
    | T3 : ('a Rep.t * 'b Rep.t * 'c Rep.t)
      -> ('a * 'b * 'c) t
    | T4 : ('a Rep.t * 'b Rep.t * 'c Rep.t * 'd Rep.t)
      -> ('a * 'b * 'c * 'd) t
    | T5 : ('a Rep.t * 'b Rep.t * 'c Rep.t * 'd Rep.t * 'e Rep.t)
      -> ('a * 'b * 'c * 'd * 'e) t

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Type_name.t
  end

  include Intf.S with type 'a t := 'a t

  (** [same t t'] will return a proof a equality if [t] and [t'] are the same type.
      One can think of two types being the [same] as two types whose values could be for
      example put in a list together.
      It is worth noting that this function *does not* operate compatiblity diffs between
      two different types with the same structure. Example:
      {|
        module M1 = struct
          type t = {
            a : int;
            b : float;
          } with typerep
        end
        module M2 = struct
          type t = {
            a : int;
            b : float;
          } with typerep
        end
        TEST = not (same M1.typerep_of_t M2.typerep_of_t)

        type a = int with typerep
        type b = int with typerep
        TEST = same typerep_of_a typerep_of_b
      |}
      This is meant to recover type equality hidden by existential constructors.
      For a deeper introspection of the structure, [see Type_struct].

      Basically this function does structural equality for everything except variant
      types, record types, and named types with no lazy definition exposed. This last case
      is about types that are defined [with typerep(abstract)]
  *)
  val same : _ t -> _ t -> bool
  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val typename_of_t : 'a t -> 'a Type_name.t

  (** [head ty] is used to traverse the [Named] constructor. It might be used when one
      care to pattern match directly on the representation in a low level way rather than
      going through a full generic. [head t] is [t] if [t] is not of the form [Named _] *)
  val head : 'a t -> 'a t
end

module Std : sig
  (* used by generator [with typerep] *)

  module Typerep : sig
    include (module type of Rep
      with type 'a t = 'a Rep.t
      and type 'a Named.t = 'a Rep.Named.t
      and type 'a Tuple.t = 'a Rep.Tuple.t
      and type 'a Record.t = 'a Rep.Record.t
      and type 'a Variant.t = 'a Rep.Variant.t
    )
    module Intf : (module type of Intf)
  end

  (* basic *)
  val typerep_of_int     : int     Typerep.t
  val typerep_of_int32   : int32   Typerep.t
  val typerep_of_int64   : int64   Typerep.t
  val typerep_of_char    : char    Typerep.t
  val typerep_of_float   : float   Typerep.t
  val typerep_of_string  : string  Typerep.t
  val typerep_of_bool    : bool    Typerep.t
  val typerep_of_unit    : unit    Typerep.t

  (* variant with no argument *)
  type tuple0
  val value_tuple0 : tuple0
  val typerep_of_tuple0  : tuple0 Typerep.t
  val typename_of_tuple0 : tuple0 Type_name.t

  (* nested *)
  val typerep_of_option : 'a Typerep.t -> 'a option Typerep.t
  val typerep_of_list : 'a Typerep.t -> 'a list Typerep.t
  val typerep_of_array : 'a Typerep.t -> 'a array Typerep.t
  val typerep_of_lazy_t : 'a Typerep.t -> 'a lazy_t Typerep.t
  val typerep_of_ref : 'a Typerep.t -> 'a ref Typerep.t
  val typerep_of_function : 'a Typerep.t -> 'b Typerep.t -> ('a -> 'b) Typerep.t
  val typerep_of_tuple2 :
    'a Typerep.t -> 'b Typerep.t
    -> ('a * 'b) Typerep.t
  val typerep_of_tuple3 :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t
    -> ('a * 'b * 'c) Typerep.t
  val typerep_of_tuple4 :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t -> 'd Typerep.t
    -> ('a * 'b * 'c * 'd) Typerep.t
  val typerep_of_tuple5 :
    'a Typerep.t -> 'b Typerep.t -> 'c Typerep.t -> 'd Typerep.t -> 'e Typerep.t
    -> ('a * 'b * 'c * 'd * 'e) Typerep.t
end
