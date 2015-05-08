open Core.Std
open Typerep_experimental.Std

module M1 : sig
  type t = {
    a : int;
    b : float;
  }
  val typerep_of_t : t Typerep.t
end

module M2 : sig
  type t = {
    a : int * string;
    b : float;
  }
  val typerep_of_t : t Typerep.t
end

module M3 : sig
  type t = {
    m1 : M1.t;
    m2 : M2.t;
  }
  val typerep_of_t : t Typerep.t
end

module P1 : sig
  type 'a t = {
    a : 'a;
    b : float;
  }
  val typerep_of_t : 'a Typerep.t -> 'a t Typerep.t
end
