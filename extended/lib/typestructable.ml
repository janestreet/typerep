open Typerep_lib.Std


(*
  Of_typerepable is defined only for fully instantiated types only (S0)
  Other interfaces (S1, ..., S5) are defined to customize the generics
  [sexprep] and [binrep]
*)

module type S0 = Type_struct.Typestructable

module type S1 = sig
  type 'a t
  val typestruct_of_t :
    Type_struct.t
    -> Type_struct.t
end

module type S2 = sig
  type ('a, 'b) t
  val typestruct_of_t :
    Type_struct.t -> Type_struct.t
    -> Type_struct.t
end

module type S3 = sig
  type ('a, 'b, 'c) t
  val typestruct_of_t :
    Type_struct.t -> Type_struct.t -> Type_struct.t
    -> Type_struct.t
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t
  val typestruct_of_t :
    Type_struct.t
    -> Type_struct.t
    -> Type_struct.t
    -> Type_struct.t
    -> Type_struct.t
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val typestruct_of_t :
    Type_struct.t
    -> Type_struct.t
    -> Type_struct.t
    -> Type_struct.t
    -> Type_struct.t
    -> Type_struct.t
end

module Of_typerepable(X:Typerepable.S0) : S0 with type t := X.t = struct
  let typestruct_of_t = Type_struct.of_typerep X.typerep_of_t
end
