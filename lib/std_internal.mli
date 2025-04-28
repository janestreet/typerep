open! Base

(** type-safe runtime type introspection *)

(** runtime type representations *)
module rec Typerep : sig
  type value := [ `value ]
  type non_value := [ `non_value ]

  (** A typerep for a type of layout value or an unboxed number. *)
  type (_, _) t_any =
    | Int : (int, value) t_any
    | Int32 : (int32, value) t_any
    | Int64 : (int64, value) t_any
    | Nativeint : (nativeint, value) t_any
    | Char : (char, value) t_any
    | Float : (float, value) t_any
    | String : (string, value) t_any
    | Bytes : (bytes, value) t_any
    | Bool : (bool, value) t_any
    | Unit : (unit, value) t_any
    | Option : ('a, value) t_any -> ('a option, value) t_any
    | List : ('a, value) t_any -> ('a list, value) t_any
    | Array : ('a, value) t_any -> ('a array, value) t_any
    | Lazy : ('a, value) t_any -> ('a lazy_t, value) t_any
    | Ref : ('a, value) t_any -> ('a ref, value) t_any
    | Function :
        (('dom, value) t_any * ('rng, value) t_any)
        -> ('dom -> 'rng, value) t_any
    | Tuple : 'a Typerep.Tuple.t -> ('a, value) t_any
    | Record : 'a Typerep.Record.t -> ('a, value) t_any
    | Variant : 'a Typerep.Variant.t -> ('a, value) t_any
    (** The [Named] constructor both allows for custom implementations of generics based
        on name and provides a way to represent recursive types, the lazy part dealing
        with cycles *)
    | Named :
        ('a Typerep.Named.t * ('a, value) t_any Portable_lazy.t option)
        -> ('a, value) t_any
    (* The constructors [Int32_u], [Int64_u], [Nativeint_u], and [Float_u] below look
       pretty weird. It's necessary because the type parameter has layout [value], and
       [unit -> int32#] has layout [value] while [int32#] does not. Making the type
       parameter have layout [any] is not feasible at this point, so this hack will remain
       until it becomes feasible *)
    | Int32_u : (unit -> int32, non_value) t_any
    | Int64_u : (unit -> int64, non_value) t_any
    | Nativeint_u : (unit -> nativeint, non_value) t_any
    | Float_u : (unit -> float, non_value) t_any
  [@@unsafe_allow_any_mode_crossing]

  (** A typerep for a type of layout value. *)
  type 'a t = ('a, value) t_any

  (** A typerep for an unboxed number. *)
  type 'a t_non_value = (unit -> 'a, non_value) t_any

  type 'a any_packed = T : ('a, _) t_any -> 'a any_packed
  [@@unsafe_allow_any_mode_crossing]

  type packed = T : 'a t -> packed [@@unsafe_allow_any_mode_crossing]

  module Named : sig
    module type T0 = sig
      type named
      type t

      val typename_of_named : named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, named) Type_equal.t
    end

    module type T1 = sig
      type 'a named
      type a

      val a : a Typerep.t

      type t

      val typename_of_named : 'a Typename.t -> 'a named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, a named) Type_equal.t
    end

    module type T2 = sig
      type ('a, 'b) named
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type t

      val typename_of_named : 'a Typename.t -> 'b Typename.t -> ('a, 'b) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b) named) Type_equal.t
    end

    module type T3 = sig
      type ('a, 'b, 'c) named
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type t

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> ('a, 'b, 'c) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end

    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type d

      val d : d Typerep.t

      type t

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> ('a, 'b, 'c, 'd) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end

    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type d

      val d : d Typerep.t

      type e

      val e : e Typerep.t

      type t

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> 'e Typename.t
        -> ('a, 'b, 'c, 'd, 'e) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end

    type 'a t =
      | T0 of (module T0 with type t = 'a)
      | T1 of (module T1 with type t = 'a)
      | T2 of (module T2 with type t = 'a)
      | T3 of (module T3 with type t = 'a)
      | T4 of (module T4 with type t = 'a)
      | T5 of (module T5 with type t = 'a)
    [@@unsafe_allow_any_mode_crossing]

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Typename.t
    val name : _ t -> string
  end

  module Tuple : sig
    type _ t =
      | T2 : (('a, _) Typerep.t_any * ('b, _) Typerep.t_any) -> ('a * 'b) t
      | T3 :
          (('a, _) Typerep.t_any * ('b, _) Typerep.t_any * ('c, _) Typerep.t_any)
          -> ('a * 'b * 'c) t
      | T4 :
          (('a, _) Typerep.t_any
          * ('b, _) Typerep.t_any
          * ('c, _) Typerep.t_any
          * ('d, _) Typerep.t_any)
          -> ('a * 'b * 'c * 'd) t
      | T5 :
          (('a, _) Typerep.t_any
          * ('b, _) Typerep.t_any
          * ('c, _) Typerep.t_any
          * ('d, _) Typerep.t_any
          * ('e, _) Typerep.t_any)
          -> ('a * 'b * 'c * 'd * 'e) t

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Typename.t
  end

  include%template
    Variant_and_record_intf.S [@modality portable] with type 'a t := 'a any_packed

  (** [same t t'] will return a proof a equality if [t] and [t'] are the same type. One
      can think of two types being the [same] as two types whose values could be for
      example put in a list together. It is worth noting that this function *does not*
      operate compatiblity diffs between two different types with the same structure.
      Example:
      {[
        module M1 = struct
          type t =
            { a : int
            ; b : float
            }
          [@@deriving typerep]
        end

        module M2 = struct
          type t =
            { a : int
            ; b : float
            }
          [@@deriving typerep]
        end

        let _ = [%test_result: bool] ~expect:false (same M1.typerep_of_t M2.typerep_of_t)

        type a = int [@@deriving typerep]
        type b = int [@@deriving typerep]

        let _ = [%test_result: bool] ~expect:true (same typerep_of_a typerep_of_b)
      ]}
      This is meant to recover type equality hidden by existential constructors.

      Basically this function does structural equality for everything except variant
      types, record types, and named types with no lazy definition exposed. This last case
      is about types that are defined [[@@deriving typerep ~abstract]]. *)
  val same : _ t_any -> _ t_any -> bool

  val same_witness : ('a, _) t_any -> ('b, _) t_any -> ('a, 'b) Type_equal.t option
  val same_witness_exn : ('a, _) t_any -> ('b, _) t_any -> ('a, 'b) Type_equal.t
  val typename_of_t : ('a, _) t_any -> 'a Typename.t

  (** [head ty] is used to traverse the [Named] constructor. It might be used when one
      care to pattern match directly on the representation in a low level way rather than
      going through a full generic. [head t] is [t] if [t] is not of the form [Named _] *)
  val head : ('a, 'index) t_any -> ('a, 'index) t_any
end

(* basic *)
val typerep_of_int : int Typerep.t
val typerep_of_int32 : int32 Typerep.t
val typerep_of_int64 : int64 Typerep.t
val typerep_of_nativeint : nativeint Typerep.t
val typerep_of_int63 : Base.Int63.t Typerep.t
val typerep_of_char : char Typerep.t
val typerep_of_float : float Typerep.t
val typerep_of_string : string Typerep.t
val typerep_of_bytes : bytes Typerep.t
val typerep_of_bool : bool Typerep.t
val typerep_of_unit : unit Typerep.t
val typerep_of_int32_u : int32 Typerep.t_non_value
val typerep_of_int64_u : int64 Typerep.t_non_value
val typerep_of_nativeint_u : nativeint Typerep.t_non_value
val typerep_of_float_u : float Typerep.t_non_value

(* variant with no argument *)
type tuple0

val value_tuple0 : tuple0

(* nested *)
val typerep_of_option : 'a Typerep.t -> 'a option Typerep.t
val typerep_of_list : 'a Typerep.t -> 'a list Typerep.t
val typerep_of_array : 'a Typerep.t -> 'a array Typerep.t
val typerep_of_lazy_t : 'a Typerep.t -> 'a lazy_t Typerep.t
val typerep_of_ref : 'a Typerep.t -> 'a ref Typerep.t
val typerep_of_function : 'a Typerep.t -> 'b Typerep.t -> ('a -> 'b) Typerep.t
val typerep_of_tuple0 : tuple0 Typerep.t

val typerep_of_tuple2
  :  ('a, _) Typerep.t_any
  -> ('b, _) Typerep.t_any
  -> ('a * 'b) Typerep.t

val typerep_of_tuple3
  :  ('a, _) Typerep.t_any
  -> ('b, _) Typerep.t_any
  -> ('c, _) Typerep.t_any
  -> ('a * 'b * 'c) Typerep.t

val typerep_of_tuple4
  :  ('a, _) Typerep.t_any
  -> ('b, _) Typerep.t_any
  -> ('c, _) Typerep.t_any
  -> ('d, _) Typerep.t_any
  -> ('a * 'b * 'c * 'd) Typerep.t

val typerep_of_tuple5
  :  ('a, _) Typerep.t_any
  -> ('b, _) Typerep.t_any
  -> ('c, _) Typerep.t_any
  -> ('d, _) Typerep.t_any
  -> ('e, _) Typerep.t_any
  -> ('a * 'b * 'c * 'd * 'e) Typerep.t

val typename_of_int : int Typename.t
val typename_of_int32 : int32 Typename.t
val typename_of_int32_u : (unit -> int32) Typename.t
val typename_of_int64 : int64 Typename.t
val typename_of_int64_u : (unit -> int64) Typename.t
val typename_of_nativeint : nativeint Typename.t
val typename_of_nativeint_u : (unit -> nativeint) Typename.t
val typename_of_int63 : Base.Int63.t Typename.t
val typename_of_char : char Typename.t
val typename_of_float : float Typename.t
val typename_of_float_u : (unit -> float) Typename.t
val typename_of_string : string Typename.t
val typename_of_bytes : bytes Typename.t
val typename_of_bool : bool Typename.t
val typename_of_unit : unit Typename.t
val typename_of_option : 'a Typename.t -> 'a option Typename.t
val typename_of_list : 'a Typename.t -> 'a list Typename.t
val typename_of_array : 'a Typename.t -> 'a array Typename.t
val typename_of_lazy_t : 'a Typename.t -> 'a lazy_t Typename.t
val typename_of_ref : 'a Typename.t -> 'a ref Typename.t
val typename_of_function : 'a Typename.t -> 'b Typename.t -> ('a -> 'b) Typename.t
val typename_of_tuple0 : tuple0 Typename.t
val typename_of_tuple2 : 'a Typename.t -> 'b Typename.t -> ('a * 'b) Typename.t

val typename_of_tuple3
  :  'a Typename.t
  -> 'b Typename.t
  -> 'c Typename.t
  -> ('a * 'b * 'c) Typename.t

val typename_of_tuple4
  :  'a Typename.t
  -> 'b Typename.t
  -> 'c Typename.t
  -> 'd Typename.t
  -> ('a * 'b * 'c * 'd) Typename.t

val typename_of_tuple5
  :  'a Typename.t
  -> 'b Typename.t
  -> 'c Typename.t
  -> 'd Typename.t
  -> 'e Typename.t
  -> ('a * 'b * 'c * 'd * 'e) Typename.t
