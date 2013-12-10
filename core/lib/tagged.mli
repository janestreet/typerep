open Typerep_kernel.Std
open Pre_core.Std

type t with sexp_of
type untyped = t

val t_of_int        : int -> t
val t_of_int32      : int32 -> t
val t_of_int64      : int64 -> t
val t_of_nativeint  : nativeint -> t
val t_of_char       : char -> t
val t_of_float      : float -> t
val t_of_string     : string -> t
val t_of_bool       : bool -> t
val t_of_unit       : unit -> t
val t_of_option     : ('a -> t) -> 'a option -> t
val t_of_list       : ('a -> t) -> 'a list -> t
val t_of_array      : ('a -> t) -> 'a array -> t

(* forces the lazy, raises the same exceptions as [Lazy.force] *)
val t_of_lazy_t : ('a -> t) -> 'a lazy_t -> t
val t_of_ref    : ('a -> t) -> 'a ref -> t

val t_of_option' : t option -> t
val t_of_list'   : t list -> t
val t_of_array'  : t array -> t
val t_of_ref'    : t ref -> t

(* forces the lazy, raises the same exceptions as [Lazy.force] *)
val t_of_lazy_t' : t lazy_t -> t

val t_of_tuple'  : t Farray.t -> t
val t_of_tuple2' : t * t -> t
val t_of_tuple3' : t * t * t -> t
val t_of_tuple4' : t * t * t * t -> t
val t_of_tuple5' : t * t * t * t * t -> t

exception Unexpected of (t * string) with sexp

val int_of_t        : t -> int
val int32_of_t      : t -> int32
val int64_of_t      : t -> int64
val nativeint_of_t  : t -> nativeint
val char_of_t       : t -> char
val float_of_t      : t -> float
val string_of_t     : t -> string
val bool_of_t       : t -> bool
val unit_of_t       : t -> unit
val option_of_t     : (t -> 'a) -> t -> 'a option
val list_of_t       : (t -> 'a) -> t -> 'a list
val array_of_t      : (t -> 'a) -> t -> 'a array
val lazy_t_of_t     : (t -> 'a) -> t -> 'a lazy_t
val ref_of_t        : (t -> 'a) -> t -> 'a ref

val option_of_t' : t -> t option
val list_of_t'   : t -> t list
val array_of_t'  : t -> t array
val ref_of_t'    : t -> t ref
val lazy_t_of_t' : t -> t lazy_t

val tuple_of_t'  : t -> t Farray.t
val tuple2_of_t' : t -> t * t
val tuple3_of_t' : t -> t * t * t
val tuple4_of_t' : t -> t * t * t * t
val tuple5_of_t' : t -> t * t * t * t * t

module Of_typed : Type_generic.S with type 'a t = 'a -> t
module Typed_of : Type_generic.S with type 'a t = t -> 'a

module Record : sig
  (* use only the name from the given array to check the fields name of the untyped *)
  val unpack_with_fields_check :
    (Type_struct.Field.t * 'a) Farray.t -> t -> (Type_struct.Field.t * t) Farray.t
  val unpack : t -> Type_struct.Record_infos.t * (Type_struct.Field.t * t) Farray.t
  val pack : Type_struct.Record_infos.t -> (Type_struct.Field.t * t) Farray.t -> t
  val field_by_name : t -> string -> t
end

module Variant : sig
  module Make(X : sig
    val infos : Type_struct.Variant_infos.t
    val branches : (Type_struct.Variant.t * Type_struct.t Farray.t) Farray.t
  end) : sig
    val get_tag_of_untyped : t -> [ `index of int ] * t
    val pack : Type_struct.Variant.t -> args:t Farray.t -> t
  end
  val to_args : t -> arity:int -> t Farray.t
  val of_args : t Farray.t -> t

  (* the two following functions work for option as well *)
  val match_with_repr : t -> repr:int -> t Farray.t option
  val unpack : t -> [ `repr of int ] * t Farray.t
  val unpack_name : t -> [ `name of string ] * t Farray.t
end

(* magic operations *)

(* works on bloc only *)
val field : int -> t -> t
