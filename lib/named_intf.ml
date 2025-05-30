open! Base

module type S0 = sig
  type t

  val name : string
end

[%%template
[@@@kind.default k = (any, any_non_null, value)]

module type S1 = sig
  type 'a t

  val name : string
end

module type S2 = sig
  type ('a, 'b) t

  val name : string
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val name : string
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val name : string
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  val name : string
end]
