@@ portable

open! Base

(** runtime representation of the name of type ['a]. Useful for representing types with a
    nominal notion of equality *)

type ('a : any) t : value mod contended portable
type ('a : any) typename = 'a t

val create : ('a : any). ?name:string -> unit -> 'a t
val static : unit t

(** nominal type equality test *)
val same : ('a : any) ('b : any). 'a t -> 'b t -> bool

val same_witness : ('a : any) ('b : any). 'a t -> 'b t -> ('a, 'b) Type_equal.t option
val same_witness_exn : ('a : any) ('b : any). 'a t -> 'b t -> ('a, 'b) Type_equal.t

(** a runtime representation of fully applied type ['a] *)
module Key : sig
  type t : value mod contended portable

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

val key : ('a : any). 'a t -> Key.t

(** an untyped runtime representation of non applied type *)
module Uid : sig
  type t : value mod contended portable

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val name : t -> string
  val sexp_of_t : t -> Sexp.t
end

val uid : ('a : any). 'a t -> Uid.t
val name : ('a : any). 'a t -> string

module type S0 = sig @@ portable
  type t : any

  val typename_of_t : t typename
end

[%%template:
[@@@kind.default k = (any, any_non_null, value)]

module type S1 = sig @@ portable
  type ('a : k) t : any

  val typename_of_t : ('a : k). 'a typename -> 'a t typename
end

module type S2 = sig @@ portable
  type ('a : k, 'b : k) t : any

  val typename_of_t : ('a : k) ('b : k). 'a typename -> 'b typename -> ('a, 'b) t typename
end

module type S3 = sig @@ portable
  type ('a : k, 'b : k, 'c : k) t : any

  val typename_of_t
    : ('a : k) ('b : k) ('c : k).
    'a typename -> 'b typename -> 'c typename -> ('a, 'b, 'c) t typename
end

module type S4 = sig @@ portable
  type ('a : k, 'b : k, 'c : k, 'd : k) t : any

  val typename_of_t
    : ('a : k) ('b : k) ('c : k) ('d : k).
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> ('a, 'b, 'c, 'd) t typename
end

module type S5 = sig @@ portable
  type ('a : k, 'b : k, 'c : k, 'd : k, 'e : k) t : any

  val typename_of_t
    : ('a : k) ('b : k) ('c : k) ('d : k) ('e : k).
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> 'e typename
    -> ('a, 'b, 'c, 'd, 'e) t typename
end]

module Make0 (X : Named_intf.S0) : S0 with type t := X.t

[%%template:
[@@@kind.default k = (any, any_non_null, value)]

module Make1 (X : Named_intf.S1 [@kind k]) : S1 [@kind k] with type ('a : k) t := 'a X.t

module Make2 (X : Named_intf.S2 [@kind k]) :
  S2 [@kind k] with type ('a : k, 'b : k) t := ('a, 'b) X.t

module Make3 (X : Named_intf.S3 [@kind k]) :
  S3 [@kind k] with type ('a : k, 'b : k, 'c : k) t := ('a, 'b, 'c) X.t

module Make4 (X : Named_intf.S4 [@kind k]) :
  S4 [@kind k] with type ('a : k, 'b : k, 'c : k, 'd : k) t := ('a, 'b, 'c, 'd) X.t

module Make5 (X : Named_intf.S5 [@kind k]) :
  S5
  [@kind k]
  with type ('a : k, 'b : k, 'c : k, 'd : k, 'e : k) t := ('a, 'b, 'c, 'd, 'e) X.t]

module Table (X : sig
    type ('a : any) t
  end) : sig
  type t

  val create : int -> t
  val mem : ('a : any). t -> 'a typename -> bool
  val set : ('a : any). t -> 'a typename -> 'a X.t -> unit
  val find : ('a : any). t -> 'a typename -> 'a X.t option
end

(* witness of equality between non applied types *)

module Same_witness_exn_1 (A : S1) (B : S1) : sig
  type t = { eq : 'a. ('a A.t, 'a B.t) Type_equal.t }

  val witness : t
end

module Same_witness_exn_2 (A : S2) (B : S2) : sig
  type t = { eq : 'a 'b. (('a, 'b) A.t, ('a, 'b) B.t) Type_equal.t }

  val witness : t
end

module Same_witness_exn_3 (A : S3) (B : S3) : sig
  type t = { eq : 'a 'b 'c. (('a, 'b, 'c) A.t, ('a, 'b, 'c) B.t) Type_equal.t }

  val witness : t
end

module Same_witness_exn_4 (A : S4) (B : S4) : sig
  type t = { eq : 'a 'b 'c 'd. (('a, 'b, 'c, 'd) A.t, ('a, 'b, 'c, 'd) B.t) Type_equal.t }

  val witness : t
end

module Same_witness_exn_5 (A : S5) (B : S5) : sig
  type t =
    { eq :
        'a 'b 'c 'd 'e. (('a, 'b, 'c, 'd, 'e) A.t, ('a, 'b, 'c, 'd, 'e) B.t) Type_equal.t
    }

  val witness : t
end
