(** Pa_type_rep: Preprocessing Module for automatic type representation *)
open Camlp4
open PreCast

module Tuple : sig
  val expr : Ast.loc -> Ast.expr list -> Ast.expr
  val patt : Ast.loc -> Ast.patt list -> Ast.patt
  val ctyp : Ast.loc -> Ast.ctyp list -> Ast.ctyp
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
    index : int;
    arity_index : int;
  }

  (** expr and patt for the constructor *)
  val expr       : loc:Ast.loc -> t -> Ast.expr
  val patt       : loc:Ast.loc -> t -> Ast.patt
  val ocaml_repr : loc:Ast.loc -> t -> Ast.expr
end

module Branches : sig
  val fields   : Ast.ctyp -> Field_case.t list
  val variants : Ast.ctyp -> Variant_case.t list
end
