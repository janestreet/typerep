(*
  Structural representation of [Type.Rep.t]. Serializable.
*)
open Typerep_kernel.Std
open Pre_core.Std

module Name : sig
  type t = int with sexp, bin_io
  include Hashable.S with type key := t
  val make_fresh : unit -> (unit -> t)
end

module Variant : sig
  module Kind : sig
    type t =
    | Polymorphic
    | Usual
    with sexp, bin_io
    val is_polymorphic : t -> bool
    val equal : t -> t -> bool
  end
  type t = {
    label : string;
    index : int;
    ocaml_repr : int;
  } with sexp_of
  val label : t -> string
  val index : t -> int
  val ocaml_repr : t -> int
  module Option : sig
    val some : t
    val none : t
  end
end

module Variant_infos : sig
  type t = {
    kind : Variant.Kind.t;
  } with sexp_of
  val equal : t -> t -> bool
end

module Field : sig
  type t = {
    label : string;
    index : int;
  } with sexp_of
  val label : t -> string
  val index : t -> int
end

module Record_infos : sig
  type t = {
    has_double_array_tag : bool;
  } with sexp_of
  val equal : t -> t -> bool
end

type t =
| Int
| Int32
| Int64
| Nativeint
| Char
| Float
| String
| Bool
| Unit
| Option of t
| List of t
| Array of t
| Lazy of t
| Ref of t
| Tuple of t Farray.t
| Record of Record_infos.t * (Field.t * t) Farray.t
| Variant of Variant_infos.t * (Variant.t * t Farray.t) Farray.t
| Named of Name.t * t option
with sexp_of
(*
  if needed, add other constructors
| Function of t list -> t
*)

type type_struct = t

val get_variant_by_repr :
  Variant_infos.t -> (Variant.t * t Farray.t) Farray.t -> int
  -> (Variant.t * t Farray.t) option

val get_variant_by_label :
  Variant_infos.t -> (Variant.t * t Farray.t) Farray.t -> string
  -> (Variant.t * t Farray.t) option

val variant_args_of_type_struct : arity:int -> t -> t Farray.t
val type_struct_of_variant_args : t Farray.t -> t

val option_as_variant : some:t -> Variant_infos.t * (Variant.t * t Farray.t) Farray.t

(*
  return a fresh type_struct that is not equivalent to any other type_struct,
  even with previous type_struct returned by calls to [incompatible]
*)
val incompatible : unit -> t

(*
  reset names from 0
*)
val alpha_conversion : t -> t

(*
  eliminate unused named introduced in the representation
*)
val remove_dead_links : t -> t

(*
  tel if the structure uses a Named constructor, probably meaning that the type is
  recursive or use some kind of sharing
*)
val has_named : t -> bool

(**
   re put at least one Named let binding for each named used in the structure so
   that the structure is self contained, and not depend on table anymore. This is
   needed especially if the structure has to go outside and be given to some other
   part of the world
*)
val standalone_exn : readonly:t Name.Table.t -> t -> t

(*
  hash consing using named indexes
*)
val reduce : t -> t

(**
   This compare is insensible to alpha conversion of cycle indexes,
   as well as reduction.
   The polymorphic compare may be wrong for equivalent structures.
   See also [alpha_conversion] or [reduce] if needed.

   This function considers equivalent polymorphic variants with different order
   of fields, as long as the order of cases is the only difference.
*)
val are_equivalent : t -> t -> bool

(**
   merge two types by regrouping the polymorphic variants, if this can be done.
   if this cannot be done, this raises with a reason for the error, in the hope
   that this information could be seen by an human, like a compiler type error message.
*)
val least_upper_bound_exn : t -> t -> t

val is_polymorphic_variant : t -> bool

module type Typestructable = sig
  type t
  val typestruct_of_t : type_struct
end

module Generic : Type_generic.S with type 'a t = t
val of_typerep : 'a Typerep.t -> t

(**
   This generates new uniq Names for the type present in the structure. In particular,
   behavior overrides done via the generic registering mechanism will not occur when
   working out of the returned type_rep.
*)
val to_typerep : t -> Typerep.packed

(* shorthand: returns the sexp of the struct computed from the rep *)
val sexp_of_typerep : _ Typerep.t -> Sexp.t

(**
  [Diff] offers a way to know given two type structures if their bin_io representation
  would be compatible. If not, the difference between the structure is computed and
  can be given as part as an error message.

  [Diff.t] is *NOT* stable. It does not support [with bin_io] on purpose,
  not any guarantee is given about the type of diffs : it may arbitrarily change
  in any further version. Consider that serializing it as an [Sexp.t] with the
  only aim being to log it somewhere for a human to read is the *only* reasonable
  thing to do with a [Diff.t] over the wire.
*)
module Diff : sig

  type t with sexp_of

  val is_empty : t -> bool

  val compute : type_struct -> type_struct -> t

  val is_bin_prot_subtype : subtype:type_struct -> supertype:type_struct -> bool

  (* filter the diff to talk only about the changes that actually break
     the compatibility. you want to apply this filter for a human to look
     at the diffs, especially if the type is complex *)
  val incompatible_changes : t -> t
end

module Versioned : sig
  type t
  with bin_io, sexp, typerep

  module Version : sig
    (* sexp syntax for config files : 'v1', 'v2', 'v3', etc. *)
    type t with bin_io, sexp, typerep
    val v1 : t
    val v2 : t
    val v3 : t
    val v4 : t
  end

  val version : t -> Version.t

  val unserialize : t -> type_struct

  exception Not_downgradable of Sexp.t with sexp
  (** may raise iif the current value is not_downgradable. (use of new feature) *)
  val serialize : version:Version.t -> type_struct -> t

  val change_version : version:Version.t -> t -> t

  (* On purpose there is no function with signature:
     [current_version : Version.t]
     or
     [val to_last_version : current -> t]
     because it would be a pain to roll an executable using such a function. *)

  module Diff : sig
    val compute : t -> t -> Diff.t
    val is_bin_prot_subtype : subtype:t -> supertype:t -> bool
  end

  val is_polymorphic_variant : t -> bool

  val least_upper_bound_exn : t -> t -> t

  val to_typerep : t -> Typerep.packed
  val of_typerep : version:Version.t -> _ Typerep.t -> t
end

class traverse : object
  method iter : t -> unit
  method map : t -> t
end

(* helper for recursive types encoded with Named *)
module Named_utils(X:sig
  type t with sexp_of
  class traverse : object
    method iter : t -> unit
    method map : t -> t
  end
  val match_named : t -> [ `Named of Name.t * t option | `Other of t ]
  val cons_named : Name.t -> t option -> t
end) : sig
  val has_named : X.t -> bool
  val remove_dead_links : X.t -> X.t
  val alpha_conversion : X.t -> X.t
  val standalone_exn : readonly:X.t Name.Table.t -> X.t -> X.t
end

type 'a typed_t = t

(* to be used in test/benchmarks only *)
val recreate_dynamically_typerep_for_test : 'a Typerep.t -> 'a Typerep.t
