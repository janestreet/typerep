(*
  Type.Rep.Named generation helpers
*)

module Make0 (X : Named_intf.S0) : sig
  val named : X.t Type.Rep.Named.t
  val typename_of_t : X.t Type_name.t
end

module Make1 (X : Named_intf.S1) : sig
  val named : 'a Type.Rep.t -> 'a X.t Type.Rep.Named.t
  val typename_of_t : 'a Type_name.t -> 'a X.t Type_name.t
end

module Make2 (X : Named_intf.S2) : sig
  val named : 'a Type.Rep.t -> 'b Type.Rep.t -> ('a, 'b) X.t Type.Rep.Named.t
  val typename_of_t : 'a Type_name.t -> 'b Type_name.t -> ('a, 'b) X.t Type_name.t
end

module Make3 (X : Named_intf.S3) : sig
  val named :
    'a Type.Rep.t -> 'b Type.Rep.t -> 'c Type.Rep.t
    -> ('a, 'b, 'c) X.t Type.Rep.Named.t
  val typename_of_t :
    'a Type_name.t -> 'b Type_name.t -> 'c Type_name.t
    -> ('a, 'b, 'c) X.t Type_name.t
end

module Make4 (X : Named_intf.S4) : sig
  val named :
    'a Type.Rep.t -> 'b Type.Rep.t -> 'c Type.Rep.t -> 'd Type.Rep.t
    -> ('a, 'b, 'c, 'd) X.t Type.Rep.Named.t
  val typename_of_t :
    'a Type_name.t -> 'b Type_name.t -> 'c Type_name.t -> 'd Type_name.t
    -> ('a, 'b, 'c, 'd) X.t Type_name.t
end

module Make5 (X : Named_intf.S5) : sig
  val named :
    'a Type.Rep.t -> 'b Type.Rep.t -> 'c Type.Rep.t -> 'd Type.Rep.t -> 'e Type.Rep.t
    -> ('a, 'b, 'c, 'd, 'e) X.t Type.Rep.Named.t
  val typename_of_t :
    'a Type_name.t -> 'b Type_name.t -> 'c Type_name.t -> 'd Type_name.t -> 'e Type_name.t
    -> ('a, 'b, 'c, 'd, 'e) X.t Type_name.t
end
