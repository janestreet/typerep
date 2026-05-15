open! Base

(** runtime representation of the name of type ['a]. Useful for representing types with a
    nominal notion of equality *)

type 'a t
type 'a typename = 'a t

val create : 'a. ?name:string -> unit -> 'a t
val static : unit t

(** nominal type equality test *)
val same : 'a 'b. 'a t -> 'b t -> bool

val same_witness : 'a 'b. 'a t -> 'b t -> ('a, 'b) Type_equal.t option
val same_witness_exn : 'a 'b. 'a t -> 'b t -> ('a, 'b) Type_equal.t

(** a runtime representation of fully applied type ['a] *)
module Key : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val sexp_of_t : t -> Sexp.t
end

val key : 'a. 'a t -> Key.t

(** an untyped runtime representation of non applied type *)
module Uid : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val name : t -> string
  val sexp_of_t : t -> Sexp.t
end

val uid : 'a. 'a t -> Uid.t

(** For debugging it is better to call [Key.sexp_of_t] as that'll include all the [name]s
    of the type parameters of ['a], if any. *)
val name : 'a. 'a t -> string

module Tuple_l : sig
  type 'a typename := 'a t

  module Internal_use_only : sig
    module Boxed : sig
      module Element : sig
        type t = T : string option * 'a typename -> t
      end

      type t = Element.t list

      (** Generate a typename for the element at some index within a particular labeled
          tuple type. For example, [typename_of_element typename_of_t 1] produces a
          typename which uniquely identifies [label:string] within the [t] described
          above.

          This function is deterministic and injective. *)
      val typename_of_index : _ typename -> int -> _ typename

      (** Generate a typename for the labeled tuple containing these elements. For
          example, [type t = int * label:string] has the elements
          [[ T (None, typename_of_int); T (Some "label", typename_of_string) ]].

          This function is deterministic and injective. *)
      val typename_of_t : t -> _ typename
    end

    module Unboxed : sig
      module Element : sig
        type _ t = T : 'a. string option * 'a typename -> 'a t
      end

      type _ t =
        | T2 : 'tuple 'a 'b. 'a Element.t * 'b Element.t -> 'tuple t
        | T3 : 'tuple 'a 'b 'c. 'a Element.t * 'b Element.t * 'c Element.t -> 'tuple t
        | T4 :
            'tuple 'a 'b 'c 'd.
            'a Element.t * 'b Element.t * 'c Element.t * 'd Element.t
            -> 'tuple t
        | T5 :
            'tuple 'a 'b 'c 'd 'e.
            'a Element.t * 'b Element.t * 'c Element.t * 'd Element.t * 'e Element.t
            -> 'tuple t

      (** Generate a typename for the labeled tuple containing these elements. For
          example, [type t = #(int * label:string)] has the elements
          [ T2 (T (None, typename_of_int), T (Some "label", typename_of_string)) ].

          This function is deterministic and injective. *)
      val typename_of_t : 'a. 'a t -> 'a typename
    end
  end
end

module type S0 = sig
  type t

  val typename_of_t : t typename
end

module Make0 (X : Named_intf.S0) : S0 with type t := X.t

[%%template:
[@@@kind.default.explicit
  ka = (any, any mod separable, value, value_or_null, float64, immediate64_or_null)]

module type S1 = sig
  type 'a t

  val typename_of_t : 'a. 'a typename -> 'a t typename
end

module Make1 (X : Named_intf.S1 [@kind.explicit ka]) :
  S1 [@kind.explicit ka] with type 'a t := 'a X.t

[@@@kind.default.explicit kb = (ka, value)]

module type S2 = sig
  type ('a, 'b) t

  val typename_of_t : 'a 'b. 'a typename -> 'b typename -> ('a, 'b) t typename
end

module Make2 (X : Named_intf.S2 [@kind.explicit ka kb]) :
  S2 [@kind.explicit ka kb] with type ('a, 'b) t := ('a, 'b) X.t

[@@@kind.default.explicit kc = (ka, value)]

module type S3 = sig
  type ('a, 'b, 'c) t

  val typename_of_t
    : 'a 'b 'c.
    'a typename -> 'b typename -> 'c typename -> ('a, 'b, 'c) t typename
end

module Make3 (X : Named_intf.S3 [@kind.explicit ka kb kc]) :
  S3 [@kind.explicit ka kb kc] with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t

[@@@kind.default.explicit kd = (ka, value)]

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val typename_of_t
    : 'a 'b 'c 'd.
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> ('a, 'b, 'c, 'd) t typename
end

module Make4 (X : Named_intf.S4 [@kind.explicit ka kb kc kd]) :
  S4 [@kind.explicit ka kb kc kd] with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) X.t

[@@@kind.default.explicit ke = (ka, value)]

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  val typename_of_t
    : 'a 'b 'c 'd 'e.
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
  with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) X.t]

[%%template:
module type S1 = S1 [@kind.explicit value]
module type S2 = S2 [@kind.explicit value value]
module type S3 = S3 [@kind.explicit value value value]
module type S4 = S4 [@kind.explicit value value value value]
module type S5 = S5 [@kind.explicit value value value value value]]

module Table (X : sig
    type 'a t
  end) : sig
  type t

  val create : int -> t
  val mem : 'a. t -> 'a typename -> bool
  val set : 'a. t -> 'a typename -> 'a X.t -> unit
  val find : 'a. t -> 'a typename -> 'a X.t option
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
