open Core.Std
open Typerep_experimental.Std

module M1 : sig
  type t =
  | A of int
  | B of float
  val typerep_of_t : t Typerep.t
end

module M2 : sig
  type t = [
  | `A of int
  | `B of float
  ]
  val typerep_of_t : t Typerep.t
end

module M3 : sig
  type t =
  | M1 of M1.t
  | M2 of M2.t
  val typerep_of_t : t Typerep.t
end

module P1 : sig
  type 'a t = [
  | `A of 'a
  | `B of float
  ]
  val typerep_of_t : 'a Typerep.t -> 'a t Typerep.t
end

module I1 : sig
  type t = A of [ `A ]
  val typerep_of_t : t Typerep.t
end
