(** Pa_type_rep: Preprocessing Module for automatic type representation *)
open Camlp4
open PreCast

module Array : sig
  include module type of StdLabels.Array
  val mapi : f:(int -> 'a -> 'b) -> 'a array -> 'b array
  val reduce_left : f:('a -> 'a -> 'a) -> 'a array -> 'a option
end

module List : sig
  include module type of StdLabels.List
  val filter_map : f:('a -> 'b option) -> 'a list -> 'b list
end

module Tuple : sig
  val expr : Ast.loc -> int -> f:(int -> Ast.expr) -> Ast.expr
  val patt : Ast.loc -> int -> f:(int -> Ast.patt) -> Ast.patt
  val ctyp : Ast.loc -> int -> f:(int -> Ast.ctyp) -> Ast.ctyp
end

module Field_case : sig
  type t = {
    label : string;
    ctyp : Ast.ctyp;
    index : int;
  }
end

module Variant_case : sig
  type t = {
    label : string;
    ctyp : Ast.ctyp option;
    poly : bool;
    arity : int;
    patt : Ast.patt;
    expr : Ast.expr;
    index : int;
    arity_index : int;
  }

  val ocaml_repr : Ast.loc -> t -> Ast.expr
end

module Branches : sig
  type t =
  | Fields of Field_case.t array
  | Variants of Variant_case.t array

  val length : t -> int

  val compute : Ast.ctyp -> t
end
