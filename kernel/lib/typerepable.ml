module type S0 = sig
  type t

  val typerep_of_t : t Type.Rep.t
  val typename_of_t : t Type_name.t
end

module type S1 = sig
  type 'a t

  val typerep_of_t : 'a Type.Rep.t -> 'a t Type.Rep.t
  val typename_of_t : 'a Type_name.t -> 'a t Type_name.t
end

module type S2 = sig
  type ('a, 'b) t

  val typerep_of_t : 'a Type.Rep.t -> 'b Type.Rep.t -> ('a, 'b) t Type.Rep.t
  val typename_of_t : 'a Type_name.t -> 'b Type_name.t -> ('a, 'b) t Type_name.t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val typerep_of_t :
    'a Type.Rep.t -> 'b Type.Rep.t -> 'c Type.Rep.t
    -> ('a, 'b, 'c) t Type.Rep.t
  val typename_of_t :
    'a Type_name.t -> 'b Type_name.t -> 'c Type_name.t
    -> ('a, 'b, 'c) t Type_name.t
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val typerep_of_t :
    'a Type.Rep.t -> 'b Type.Rep.t -> 'c Type.Rep.t -> 'd Type.Rep.t
    -> ('a, 'b, 'c, 'd) t Type.Rep.t
  val typename_of_t :
    'a Type_name.t -> 'b Type_name.t -> 'c Type_name.t -> 'd Type_name.t
    -> ('a, 'b, 'c, 'd) t Type_name.t
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  val typerep_of_t :
    'a Type.Rep.t -> 'b Type.Rep.t -> 'c Type.Rep.t -> 'd Type.Rep.t -> 'e Type.Rep.t
    -> ('a, 'b, 'c, 'd, 'e) t Type.Rep.t
  val typename_of_t :
    'a Type_name.t -> 'b Type_name.t -> 'c Type_name.t -> 'd Type_name.t -> 'e Type_name.t
    -> ('a, 'b, 'c, 'd, 'e) t Type_name.t
end

type t = Rep : 'a Type.Rep.t -> t
