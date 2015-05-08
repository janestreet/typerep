open Core.Std
open Typerep_experimental.Std

module Transaction_type : sig
  module V1 : sig
    type t =
    | Trade
    | Order
    val typerep_of_t : t Typerep.t
  end
  module V2 : sig
    type t =
    | Trade
    | Order
    | Journal of string * string
    val typerep_of_t : t Typerep.t
  end
end

module V1 : sig
  type t = {
    transaction_type : Transaction_type.V1.t;
    username : string;
  }
  val typerep_of_t : t Typerep.t
end

module V2 : sig
  type t = {
    transaction_type : Transaction_type.V2.t;
    username : string;
    tags : (string * string) list;
  }
  val typerep_of_t : t Typerep.t
end

type t =
| V1 of V1.t
| V2 of V2.t

val typerep_of_t : t Typerep.t

