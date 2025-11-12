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

module Make0 (X : Named_intf.S0) : S0 with type t := X.t

[%%template:
[@@@kind.default.explicit ka = (any, any mod separable, value, value_or_null, float64)]

module type S1 = sig @@ portable
  type ('a : ka) t : any

  val typename_of_t : ('a : ka). 'a typename -> 'a t typename
end

module Make1 (X : Named_intf.S1 [@kind.explicit ka]) :
  S1 [@kind.explicit ka] with type ('a : ka) t := 'a X.t

[@@@kind.default.explicit kb = (ka, value)]

module type S2 = sig @@ portable
  type ('a : ka, 'b : kb) t : any

  val typename_of_t
    : ('a : ka) ('b : kb).
    'a typename -> 'b typename -> ('a, 'b) t typename
end

module Make2 (X : Named_intf.S2 [@kind.explicit ka kb]) :
  S2 [@kind.explicit ka kb] with type ('a : ka, 'b : kb) t := ('a, 'b) X.t

[@@@kind.default.explicit kc = (ka, value)]

module type S3 = sig @@ portable
  type ('a : ka, 'b : kb, 'c : kc) t : any

  val typename_of_t
    : ('a : ka) ('b : kb) ('c : kc).
    'a typename -> 'b typename -> 'c typename -> ('a, 'b, 'c) t typename
end

module Make3 (X : Named_intf.S3 [@kind.explicit ka kb kc]) :
  S3 [@kind.explicit ka kb kc] with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) X.t

[@@@kind.default.explicit kd = (ka, value)]

module type S4 = sig @@ portable
  type ('a : ka, 'b : kb, 'c : kc, 'd : kd) t : any

  val typename_of_t
    : ('a : ka) ('b : kb) ('c : kc) ('d : kd).
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> ('a, 'b, 'c, 'd) t typename
end

module Make4 (X : Named_intf.S4 [@kind.explicit ka kb kc kd]) :
  S4
  [@kind.explicit ka kb kc kd]
  with type ('a : ka, 'b : kb, 'c : kc, 'd : kd) t := ('a, 'b, 'c, 'd) X.t

[@@@kind.default.explicit ke = (ka, value)]

module type S5 = sig @@ portable
  type ('a : ka, 'b : kb, 'c : kc, 'd : kd, 'e : ke) t : any

  val typename_of_t
    : ('a : ka) ('b : kb) ('c : kc) ('d : kd) ('e : ke).
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> 'e typename
    -> ('a, 'b, 'c, 'd, 'e) t typename
end

module Make5 (X : Named_intf.S5 [@kind.explicit ka kb kc kd ke]) :
  S5
  [@kind.explicit ka kb kc kd ke]
  with type ('a : ka, 'b : kb, 'c : kc, 'd : kd, 'e : ke) t := ('a, 'b, 'c, 'd, 'e) X.t]

[%%template:
module type S1 = S1 [@kind.explicit value]
module type S2 = S2 [@kind.explicit value value]
module type S3 = S3 [@kind.explicit value value value]
module type S4 = S4 [@kind.explicit value value value value]
module type S5 = S5 [@kind.explicit value value value value value]]

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
