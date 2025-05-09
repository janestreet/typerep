open! Base

module type S0 = sig
  type t : any

  val name : string
end

[%%template
[@@@kind.default k = (any, any_non_null, value)]

module type S1 = sig
  type ('a : k) t : any

  val name : string
end

module type S2 = sig
  type ('a : k, 'b : k) t : any

  val name : string
end

module type S3 = sig
  type ('a : k, 'b : k, 'c : k) t : any

  val name : string
end

module type S4 = sig
  type ('a : k, 'b : k, 'c : k, 'd : k) t : any

  val name : string
end

module type S5 = sig
  type ('a : k, 'b : k, 'c : k, 'd : k, 'e : k) t : any

  val name : string
end]
