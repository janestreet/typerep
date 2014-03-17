open Core.Std
open Typerep_experimental.Std

module M1 : sig
  type t = int * string
  val typerep_of_t : t Typerep.t
end

module M2 : sig
  type t = int * string * float
  val typerep_of_t : t Typerep.t
end

module M3 : sig
  type t = M1.t * M2.t
  val typerep_of_t : t Typerep.t
end
