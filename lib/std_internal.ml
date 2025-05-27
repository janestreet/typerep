type ('a : any_non_null) builtin_array = 'a array

open! Base

module Name_of = struct
  let typename_of_int =
    let module M =
      Typename.Make0 (struct
        type t = int

        let name = "int"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_int32 =
    let module M =
      Typename.Make0 (struct
        type t = int32

        let name = "int32"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_int32_u =
    let module M =
      Typename.Make0 (struct
        type t = int32#

        let name = "int32#"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_int64 =
    let module M =
      Typename.Make0 (struct
        type t = int64

        let name = "int64"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_int64_u =
    let module M =
      Typename.Make0 (struct
        type t = int64#

        let name = "int64#"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_nativeint =
    let module M =
      Typename.Make0 (struct
        type t = nativeint

        let name = "nativeint"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_nativeint_u =
    let module M =
      Typename.Make0 (struct
        type t = nativeint#

        let name = "nativeint#"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_char =
    let module M =
      Typename.Make0 (struct
        type t = char

        let name = "char"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_float =
    let module M =
      Typename.Make0 (struct
        type t = float

        let name = "float"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_float_u =
    let module M =
      Typename.Make0 (struct
        type t = float#

        let name = "float#"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_string =
    let module M =
      Typename.Make0 (struct
        type t = string

        let name = "string"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_bytes =
    let module M =
      Typename.Make0 (struct
        type t = bytes

        let name = "bytes"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_bool =
    let module M =
      Typename.Make0 (struct
        type t = bool

        let name = "bool"
      end)
    in
    M.typename_of_t
  ;;

  let typename_of_unit =
    let module M =
      Typename.Make0 (struct
        type t = unit

        let name = "unit"
      end)
    in
    M.typename_of_t
  ;;

  module M_option = Typename.Make1 (struct
      type 'a t = 'a option

      let name = "option"
    end)

  let typename_of_option = M_option.typename_of_t

  module M_list = Typename.Make1 (struct
      type 'a t = 'a list

      let name = "list"
    end)

  let typename_of_list = M_list.typename_of_t

  module%template M_array = Typename.Make1 [@kind any_non_null] (struct
      type ('a : any_non_null) t = 'a builtin_array

      let name = "array"
    end)

  let typename_of_array = M_array.typename_of_t

  module M_lazy_t = Typename.Make1 (struct
      type 'a t = 'a lazy_t

      let name = "lazy_t"
    end)

  let typename_of_lazy_t = M_lazy_t.typename_of_t

  module M_ref = Typename.Make1 (struct
      type 'a t = 'a ref

      let name = "ref"
    end)

  let typename_of_ref = M_ref.typename_of_t

  module%template M_function = Typename.Make2 [@kind any] (struct
      type ('a : any, 'b : any) t = 'a -> 'b

      let name = "function"
    end)

  let typename_of_function = M_function.typename_of_t

  type tuple0 = unit

  module M_tuple0 = Typename.Make0 (struct
      type t = tuple0

      let name = "tuple0"
    end)

  let typename_of_tuple0 = M_tuple0.typename_of_t

  module M_tuple2 = Typename.Make2 (struct
      type ('a, 'b) t = 'a * 'b

      let name = "tuple2"
    end)

  let typename_of_tuple2 = M_tuple2.typename_of_t

  module M_tuple3 = Typename.Make3 (struct
      type ('a, 'b, 'c) t = 'a * 'b * 'c

      let name = "tuple3"
    end)

  let typename_of_tuple3 = M_tuple3.typename_of_t

  module M_tuple4 = Typename.Make4 (struct
      type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

      let name = "tuple4"
    end)

  let typename_of_tuple4 = M_tuple4.typename_of_t

  module M_tuple5 = Typename.Make5 (struct
      type ('a, 'b, 'c, 'd, 'e) t = 'a * 'b * 'c * 'd * 'e

      let name = "tuple5"
    end)

  let typename_of_tuple5 = M_tuple5.typename_of_t

  module%template M_tuple2_u = Typename.Make2 [@kind any] (struct
      type ('a : any, 'b : any) t = #('a * 'b)

      let name = "tuple2_u"
    end)

  let typename_of_tuple2_u = M_tuple2_u.typename_of_t

  module%template M_tuple3_u = Typename.Make3 [@kind any] (struct
      type ('a : any, 'b : any, 'c : any) t = #('a * 'b * 'c)

      let name = "tuple3_u"
    end)

  let typename_of_tuple3_u = M_tuple3_u.typename_of_t

  module%template M_tuple4_u = Typename.Make4 [@kind any] (struct
      type ('a : any, 'b : any, 'c : any, 'd : any) t = #('a * 'b * 'c * 'd)

      let name = "tuple4_u"
    end)

  let typename_of_tuple4_u = M_tuple4_u.typename_of_t

  module%template M_tuple5_u = Typename.Make5 [@kind any] (struct
      type ('a : any, 'b : any, 'c : any, 'd : any, 'e : any) t =
        #('a * 'b * 'c * 'd * 'e)

      let name = "tuple5_u"
    end)

  let typename_of_tuple5_u = M_tuple5_u.typename_of_t

  module M_or_null = Typename.Make1 (struct
      type 'a t = 'a or_null

      let name = "or_null"
    end)

  let typename_of_or_null = M_or_null.typename_of_t
end

module rec Typerep : sig @@ portable
  type (_ : any) t : value mod contended portable =
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Nativeint : nativeint t
    | Char : char t
    | Float : float t
    | String : string t
    | Bytes : bytes t
    | Bool : bool t
    | Unit : unit t
    | Option : 'a t -> 'a option t
    | Or_null : 'a t -> 'a or_null t
    | List : 'a t -> 'a list t
    | Array : ('a : any_non_null). 'a t -> 'a builtin_array t
    | Lazy : 'a t -> 'a lazy_t t
    | Ref : 'a t -> 'a ref t
    | Function : ('dom : any) ('rng : any). ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple : 'a Typerep.Tuple.t -> 'a t
    | Record : 'a Typerep.Record.t -> 'a t
    | Variant : 'a Typerep.Variant.t -> 'a t
    | Named :
        ('a : any).
        ('a Typerep.Named.t * ('a t Portable_lazy.t, 'a Typerep.Kind.t) Either.t)
        -> 'a t
    | Int32_u : int32# t
    | Int64_u : int64# t
    | Nativeint_u : nativeint# t
    | Float_u : float# t
    | Tuple_u : ('a : any). 'a Typerep.Tuple_u.t -> 'a t
  [@@unsafe_allow_any_mode_crossing]

  type packed : value mod contended portable = T : 'a t -> packed
  [@@unsafe_allow_any_mode_crossing]

  module Named : sig
    module type T0 = sig @@ portable
      type named : any
      type t : any

      val typename_of_named : named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, named) Type_equal.t
    end

    module type T1 = sig @@ portable
      type 'a named : any
      type a

      val a : a Typerep.t

      type t : any

      val typename_of_named : 'a Typename.t -> 'a named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, a named) Type_equal.t
    end

    module type T2 = sig @@ portable
      type ('a, 'b) named : any
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type t : any

      val typename_of_named : 'a Typename.t -> 'b Typename.t -> ('a, 'b) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b) named) Type_equal.t
    end

    module type T3 = sig @@ portable
      type ('a, 'b, 'c) named : any
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type t : any

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> ('a, 'b, 'c) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end

    module type T4 = sig @@ portable
      type ('a, 'b, 'c, 'd) named : any
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type d

      val d : d Typerep.t

      type t : any

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> ('a, 'b, 'c, 'd) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end

    module type T5 = sig @@ portable
      type ('a, 'b, 'c, 'd, 'e) named : any
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type d

      val d : d Typerep.t

      type e

      val e : e Typerep.t

      type t : any

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> 'e Typename.t
        -> ('a, 'b, 'c, 'd, 'e) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end

    (* there the module is necessary because we need to deal with a type [t] with
       parameters whose kind is not representable as a type variable: ['a 't], even with
       a gadt. *)
    type ('a : any) t : value mod contended portable =
      | T0 of (module T0 with type t = 'a)
      | T1 of (module T1 with type t = 'a)
      | T2 of (module T2 with type t = 'a)
      | T3 of (module T3 with type t = 'a)
      | T4 of (module T4 with type t = 'a)
      | T5 of (module T5 with type t = 'a)
    [@@unsafe_allow_any_mode_crossing]

    val arity : ('a : any). 'a t -> int
    val typename_of_t : ('a : any). 'a t -> 'a Typename.t
    val name : ('a : any). 'a t -> string
  end

  module Tuple : sig
    (* these constructors could be plunged at toplevel of Typerep.t, however it is less
       verbose that way *)
    type _ t =
      | T2 : ('a Typerep.t * 'b Typerep.t) -> ('a * 'b) t
      | T3 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t) -> ('a * 'b * 'c) t
      | T4 :
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
          -> ('a * 'b * 'c * 'd) t
      | T5 :
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
          -> ('a * 'b * 'c * 'd * 'e) t

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Typename.t
  end

  module Tuple_u : sig
    type (_ : any) t =
      | T2 : ('a : any) ('b : any). ('a Typerep.t * 'b Typerep.t) -> #('a * 'b) t
      | T3 :
          ('a : any) ('b : any) ('c : any).
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t)
          -> #('a * 'b * 'c) t
      | T4 :
          ('a : any) ('b : any) ('c : any) ('d : any).
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
          -> #('a * 'b * 'c * 'd) t
      | T5 :
          ('a : any) ('b : any) ('c : any) ('d : any) ('e : any).
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
          -> #('a * 'b * 'c * 'd * 'e) t

    val arity : ('a : any). 'a t -> int
    val typename_of_t : ('a : any). 'a t -> 'a Typename.t
  end

  include%template
    Variant_and_record_intf.S [@modality portable] with type ('a : any) t := 'a t

  module Kind : sig
    type ('a : any) t =
      | Value : ('a : value). 'a t
      | Value_or_null : ('a : value_or_null). 'a t
      | Bits32 : ('a : bits32). 'a t
      | Bits64 : ('a : bits64). 'a t
      | Word : ('a : word). 'a t
      | Float64 : ('a : float64). 'a t
      | Tuple2_u : ('a : any) ('b : any). 'a t * 'b t -> #('a * 'b) t
      | Tuple3_u :
          ('a : any) ('b : any) ('c : any).
          'a t * 'b t * 'c t
          -> #('a * 'b * 'c) t
      | Tuple4_u :
          ('a : any) ('b : any) ('c : any) ('d : any).
          'a t * 'b t * 'c t * 'd t
          -> #('a * 'b * 'c * 'd) t
      | Tuple5_u :
          ('a : any) ('b : any) ('c : any) ('d : any) ('e : any).
          'a t * 'b t * 'c t * 'd t * 'e t
          -> #('a * 'b * 'c * 'd * 'e) t
  end

  val same : ('a : any) ('b : any). 'a t -> 'b t -> bool
  val same_witness : ('a : any) ('b : any). 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : ('a : any) ('b : any). 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val typename_of_t : ('a : any). 'a t -> 'a Typename.t
  val head : ('a : any). 'a t -> 'a t
  val kind : ('a : any). 'a t -> 'a Kind.t
end = struct
  type (_ : any) t : value mod contended portable =
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Nativeint : nativeint t
    | Char : char t
    | Float : float t
    | String : string t
    | Bytes : bytes t
    | Bool : bool t
    | Unit : unit t
    | Option : 'a t -> 'a option t
    | Or_null : 'a t -> 'a or_null t
    | List : 'a t -> 'a list t
    | Array : ('a : any_non_null). 'a t -> 'a builtin_array t
    | Lazy : 'a t -> 'a lazy_t t
    | Ref : 'a t -> 'a ref t
    | Function : ('dom : any) ('rng : any). ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple : 'a Typerep.Tuple.t -> 'a t
    | Record : 'a Typerep.Record.t -> 'a t
    | Variant : 'a Typerep.Variant.t -> 'a t
    | Named :
        ('a : any).
        ('a Typerep.Named.t * ('a t Portable_lazy.t, 'a Typerep.Kind.t) Either.t)
        -> 'a t
    | Int32_u : int32# t
    | Int64_u : int64# t
    | Nativeint_u : nativeint# t
    | Float_u : float# t
    | Tuple_u : ('a : any). 'a Typerep.Tuple_u.t -> 'a t
  [@@unsafe_allow_any_mode_crossing]

  type packed : value mod contended portable = T : 'a t -> packed
  [@@unsafe_allow_any_mode_crossing]

  module Named = struct
    module type T0 = sig @@ portable
      type named : any
      type t : any

      val typename_of_named : named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, named) Type_equal.t
    end

    module type T1 = sig @@ portable
      type 'a named : any
      type a

      val a : a Typerep.t

      type t : any

      val typename_of_named : 'a Typename.t -> 'a named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, a named) Type_equal.t
    end

    module type T2 = sig @@ portable
      type ('a, 'b) named : any
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type t : any

      val typename_of_named : 'a Typename.t -> 'b Typename.t -> ('a, 'b) named Typename.t
      val typename_of_t : t Typename.t
      val witness : (t, (a, b) named) Type_equal.t
    end

    module type T3 = sig @@ portable
      type ('a, 'b, 'c) named : any
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type t : any

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> ('a, 'b, 'c) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end

    module type T4 = sig @@ portable
      type ('a, 'b, 'c, 'd) named : any
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type d

      val d : d Typerep.t

      type t : any

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> ('a, 'b, 'c, 'd) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end

    module type T5 = sig @@ portable
      type ('a, 'b, 'c, 'd, 'e) named : any
      type a

      val a : a Typerep.t

      type b

      val b : b Typerep.t

      type c

      val c : c Typerep.t

      type d

      val d : d Typerep.t

      type e

      val e : e Typerep.t

      type t : any

      val typename_of_named
        :  'a Typename.t
        -> 'b Typename.t
        -> 'c Typename.t
        -> 'd Typename.t
        -> 'e Typename.t
        -> ('a, 'b, 'c, 'd, 'e) named Typename.t

      val typename_of_t : t Typename.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end

    (* there the module is necessary because we need to deal with a type [t] with
       parameters whose kind is not representable as a type variable: ['a 't], even with
       a gadt. *)
    type ('a : any) t : value mod contended portable =
      | T0 of (module T0 with type t = 'a)
      | T1 of (module T1 with type t = 'a)
      | T2 of (module T2 with type t = 'a)
      | T3 of (module T3 with type t = 'a)
      | T4 of (module T4 with type t = 'a)
      | T5 of (module T5 with type t = 'a)
    [@@unsafe_allow_any_mode_crossing]

    let arity = function
      | T0 _ -> 0
      | T1 _ -> 1
      | T2 _ -> 2
      | T3 _ -> 3
      | T4 _ -> 4
      | T5 _ -> 5
    ;;

    let typename_of_t (type a : any) = function
      | T0 rep ->
        let module T = (val rep : T0 with type t = a) in
        T.typename_of_t
      | T1 rep ->
        let module T = (val rep : T1 with type t = a) in
        T.typename_of_t
      | T2 rep ->
        let module T = (val rep : T2 with type t = a) in
        T.typename_of_t
      | T3 rep ->
        let module T = (val rep : T3 with type t = a) in
        T.typename_of_t
      | T4 rep ->
        let module T = (val rep : T4 with type t = a) in
        T.typename_of_t
      | T5 rep ->
        let module T = (val rep : T5 with type t = a) in
        T.typename_of_t
    ;;

    let name rep = Typename.Uid.name (Typename.uid (typename_of_t rep))
  end

  module Tuple = struct
    (* these constructors could be plunged at toplevel of Typerep.t, however it is less
       verbose this way *)
    type _ t =
      | T2 : ('a Typerep.t * 'b Typerep.t) -> ('a * 'b) t
      | T3 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t) -> ('a * 'b * 'c) t
      | T4 :
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
          -> ('a * 'b * 'c * 'd) t
      | T5 :
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
          -> ('a * 'b * 'c * 'd * 'e) t

    let arity : type a. a t -> int = function
      | T2 _ -> 2
      | T3 _ -> 3
      | T4 _ -> 4
      | T5 _ -> 5
    ;;

    let typename_of_t : type a. a t -> a Typename.t = function
      | T2 (a, b) ->
        Name_of.typename_of_tuple2 (Typerep.typename_of_t a) (Typerep.typename_of_t b)
      | T3 (a, b, c) ->
        Name_of.typename_of_tuple3
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
      | T4 (a, b, c, d) ->
        Name_of.typename_of_tuple4
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
          (Typerep.typename_of_t d)
      | T5 (a, b, c, d, e) ->
        Name_of.typename_of_tuple5
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
          (Typerep.typename_of_t d)
          (Typerep.typename_of_t e)
    ;;
  end

  module Tuple_u = struct
    (* these constructors could be plunged at toplevel of Typerep.t, however it is less
       verbose this way *)
    type (_ : any) t =
      | T2 : ('a : any) ('b : any). ('a Typerep.t * 'b Typerep.t) -> #('a * 'b) t
      | T3 :
          ('a : any) ('b : any) ('c : any).
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t)
          -> #('a * 'b * 'c) t
      | T4 :
          ('a : any) ('b : any) ('c : any) ('d : any).
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
          -> #('a * 'b * 'c * 'd) t
      | T5 :
          ('a : any) ('b : any) ('c : any) ('d : any) ('e : any).
          ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
          -> #('a * 'b * 'c * 'd * 'e) t

    let arity : type (a : any). a t -> int = function
      | T2 _ -> 2
      | T3 _ -> 3
      | T4 _ -> 4
      | T5 _ -> 5
    ;;

    let typename_of_t : type (a : any). a t -> a Typename.t = function
      | T2 (a, b) ->
        Name_of.typename_of_tuple2_u (Typerep.typename_of_t a) (Typerep.typename_of_t b)
      | T3 (a, b, c) ->
        Name_of.typename_of_tuple3_u
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
      | T4 (a, b, c, d) ->
        Name_of.typename_of_tuple4_u
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
          (Typerep.typename_of_t d)
      | T5 (a, b, c, d, e) ->
        Name_of.typename_of_tuple5_u
          (Typerep.typename_of_t a)
          (Typerep.typename_of_t b)
          (Typerep.typename_of_t c)
          (Typerep.typename_of_t d)
          (Typerep.typename_of_t e)
    ;;
  end

  include%template Variant_and_record_intf.M [@modality portable] (struct
      type nonrec ('a : any) t = 'a t
    end)

  module Kind = struct
    type ('a : any) t =
      | Value : ('a : value). 'a t
      | Value_or_null : ('a : value_or_null). 'a t
      | Bits32 : ('a : bits32). 'a t
      | Bits64 : ('a : bits64). 'a t
      | Word : ('a : word). 'a t
      | Float64 : ('a : float64). 'a t
      | Tuple2_u : ('a : any) ('b : any). 'a t * 'b t -> #('a * 'b) t
      | Tuple3_u :
          ('a : any) ('b : any) ('c : any).
          'a t * 'b t * 'c t
          -> #('a * 'b * 'c) t
      | Tuple4_u :
          ('a : any) ('b : any) ('c : any) ('d : any).
          'a t * 'b t * 'c t * 'd t
          -> #('a * 'b * 'c * 'd) t
      | Tuple5_u :
          ('a : any) ('b : any) ('c : any) ('d : any) ('e : any).
          'a t * 'b t * 'c t * 'd t * 'e t
          -> #('a * 'b * 'c * 'd * 'e) t
  end

  let rec typename_of_t : type (a : any). a t -> a Typename.t = function
    | Int -> Name_of.typename_of_int
    | Int32 -> Name_of.typename_of_int32
    | Int32_u -> Name_of.typename_of_int32_u
    | Int64 -> Name_of.typename_of_int64
    | Int64_u -> Name_of.typename_of_int64_u
    | Nativeint -> Name_of.typename_of_nativeint
    | Nativeint_u -> Name_of.typename_of_nativeint_u
    | Char -> Name_of.typename_of_char
    | Float -> Name_of.typename_of_float
    | Float_u -> Name_of.typename_of_float_u
    | String -> Name_of.typename_of_string
    | Bytes -> Name_of.typename_of_bytes
    | Bool -> Name_of.typename_of_bool
    | Unit -> Name_of.typename_of_unit
    | Option rep -> Name_of.typename_of_option (typename_of_t rep)
    | Or_null rep -> Name_of.typename_of_or_null (typename_of_t rep)
    | List rep -> Name_of.typename_of_list (typename_of_t rep)
    | Array rep -> Name_of.typename_of_array (typename_of_t rep)
    | Lazy rep -> Name_of.typename_of_lazy_t (typename_of_t rep)
    | Ref rep -> Name_of.typename_of_ref (typename_of_t rep)
    | Function (dom, rng) ->
      Name_of.typename_of_function (typename_of_t dom) (typename_of_t rng)
    | Tuple rep -> Typerep.Tuple.typename_of_t rep
    | Tuple_u rep -> Typerep.Tuple_u.typename_of_t rep
    | Record rep -> Typerep.Record.typename_of_t rep
    | Variant rep -> Typerep.Variant.typename_of_t rep
    | Named (name, _) -> Named.typename_of_t name
  ;;

  let rec same_witness
    : type (a : any) (b : any). a t -> b t -> (a, b) Type_equal.t option
    =
    fun t1 t2 ->
    let module E = Type_equal in
    match t1, t2 with
    | Named (name1, r1), Named (name2, r2) ->
      (match
         Typename.same_witness (Named.typename_of_t name1) (Named.typename_of_t name2)
       with
       | Some E.T as x -> x
       | None ->
         (match r1, r2 with
          | First t1, First t2 ->
            same_witness (Portable_lazy.force t1) (Portable_lazy.force t2)
          | First t1, Second _ -> same_witness (Portable_lazy.force t1) t2
          | Second _, First t2 -> same_witness t1 (Portable_lazy.force t2)
          | Second _, Second _ -> None))
    | Named (_, r1), t2 ->
      (match r1 with
       | First t1 -> same_witness (Portable_lazy.force t1) t2
       | Second _ -> None)
    | t1, Named (_, r2) ->
      (match r2 with
       | First t2 -> same_witness t1 (Portable_lazy.force t2)
       | Second _ -> None)
    | Int, Int -> Some E.T
    | Int32, Int32 -> Some E.T
    | Int32_u, Int32_u -> Some E.T
    | Int64, Int64 -> Some E.T
    | Int64_u, Int64_u -> Some E.T
    | Nativeint, Nativeint -> Some E.T
    | Nativeint_u, Nativeint_u -> Some E.T
    | Char, Char -> Some E.T
    | Float, Float -> Some E.T
    | Float_u, Float_u -> Some E.T
    | String, String -> Some E.T
    | Bytes, Bytes -> Some E.T
    | Bool, Bool -> Some E.T
    | Unit, Unit -> Some E.T
    | Option r1, Option r2 ->
      (match same_witness r1 r2 with
       | None as x -> x
       | Some E.T as x -> x)
    | Or_null r1, Or_null r2 ->
      (match same_witness r1 r2 with
       | None as x -> x
       | Some E.T as x -> x)
    | List r1, List r2 ->
      (match same_witness r1 r2 with
       | None as x -> x
       | Some E.T as x -> x)
    | Array r1, Array r2 ->
      (match same_witness r1 r2 with
       | None as x -> x
       | Some E.T as x -> x)
    | Lazy r1, Lazy r2 ->
      (match same_witness r1 r2 with
       | None as x -> x
       | Some E.T as x -> x)
    | Ref r1, Ref r2 ->
      (match same_witness r1 r2 with
       | None as x -> x
       | Some E.T as x -> x)
    | Function (dom1, rng1), Function (dom2, rng2) ->
      (match same_witness dom1 dom2, same_witness rng1 rng2 with
       | Some E.T, Some E.T -> Some E.T
       | None, _ | _, None -> None)
    | Tuple t1, Tuple t2 ->
      let module T = Typerep.Tuple in
      (match t1, t2 with
       | T.T2 (a1, b1), T.T2 (a2, b2) ->
         (match same_witness a1 a2, same_witness b1 b2 with
          | Some E.T, Some E.T -> Some E.T
          | None, _ | _, None -> None)
       | T.T3 (a1, b1, c1), T.T3 (a2, b2, c2) ->
         (match same_witness a1 a2, same_witness b1 b2, same_witness c1 c2 with
          | Some E.T, Some E.T, Some E.T -> Some E.T
          | None, _, _ | _, None, _ | _, _, None -> None)
       | T.T4 (a1, b1, c1, d1), T.T4 (a2, b2, c2, d2) ->
         (match
            same_witness a1 a2, same_witness b1 b2, same_witness c1 c2, same_witness d1 d2
          with
          | Some E.T, Some E.T, Some E.T, Some E.T -> Some E.T
          | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None -> None)
       | T.T5 (a1, b1, c1, d1, e1), T.T5 (a2, b2, c2, d2, e2) ->
         (match
            ( same_witness a1 a2
            , same_witness b1 b2
            , same_witness c1 c2
            , same_witness d1 d2
            , same_witness e1 e2 )
          with
          | Some E.T, Some E.T, Some E.T, Some E.T, Some E.T -> Some E.T
          | None, _, _, _, _
          | _, None, _, _, _
          | _, _, None, _, _
          | _, _, _, None, _
          | _, _, _, _, None -> None)
       | T.T2 _, _ -> None
       | T.T3 _, _ -> None
       | T.T4 _, _ -> None
       | T.T5 _, _ -> None)
    | Tuple_u t1, Tuple_u t2 ->
      let module T = Typerep.Tuple_u in
      (match t1, t2 with
       | T.T2 (a1, b1), T.T2 (a2, b2) ->
         (match same_witness a1 a2, same_witness b1 b2 with
          | Some E.T, Some E.T -> Some E.T
          | None, _ | _, None -> None)
       | T.T3 (a1, b1, c1), T.T3 (a2, b2, c2) ->
         (match same_witness a1 a2, same_witness b1 b2, same_witness c1 c2 with
          | Some E.T, Some E.T, Some E.T -> Some E.T
          | None, _, _ | _, None, _ | _, _, None -> None)
       | T.T4 (a1, b1, c1, d1), T.T4 (a2, b2, c2, d2) ->
         (match
            same_witness a1 a2, same_witness b1 b2, same_witness c1 c2, same_witness d1 d2
          with
          | Some E.T, Some E.T, Some E.T, Some E.T -> Some E.T
          | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None -> None)
       | T.T5 (a1, b1, c1, d1, e1), T.T5 (a2, b2, c2, d2, e2) ->
         (match
            ( same_witness a1 a2
            , same_witness b1 b2
            , same_witness c1 c2
            , same_witness d1 d2
            , same_witness e1 e2 )
          with
          | Some E.T, Some E.T, Some E.T, Some E.T, Some E.T -> Some E.T
          | None, _, _, _, _
          | _, None, _, _, _
          | _, _, None, _, _
          | _, _, _, None, _
          | _, _, _, _, None -> None)
       | T.T2 _, _ -> None
       | T.T3 _, _ -> None
       | T.T4 _, _ -> None
       | T.T5 _, _ -> None)
    | Record r1, Record r2 ->
      Typename.same_witness
        (Typerep.Record.typename_of_t r1)
        (Typerep.Record.typename_of_t r2)
    | Variant r1, Variant r2 ->
      Typename.same_witness
        (Typerep.Variant.typename_of_t r1)
        (Typerep.Variant.typename_of_t r2)
    | Int, _ -> None
    | Int32, _ -> None
    | Int32_u, _ -> None
    | Int64, _ -> None
    | Int64_u, _ -> None
    | Nativeint, _ -> None
    | Nativeint_u, _ -> None
    | Char, _ -> None
    | Float, _ -> None
    | Float_u, _ -> None
    | String, _ -> None
    | Bytes, _ -> None
    | Bool, _ -> None
    | Unit, _ -> None
    | Option _, _ -> None
    | Or_null _, _ -> None
    | List _, _ -> None
    | Array _, _ -> None
    | Lazy _, _ -> None
    | Ref _, _ -> None
    | Function _, _ -> None
    | Tuple _, _ -> None
    | Tuple_u _, _ -> None
    | Record _, _ -> None
    | Variant _, _ -> None
  ;;

  let same a b = Option.is_some (same_witness a b)

  let same_witness_exn a b =
    match same_witness a b with
    | Some proof -> proof
    | None -> assert false
  ;;

  let rec head : type (a : any). a t -> a t = function
    | Typerep.Named (_, First t) -> head (Portable_lazy.force t)
    | t -> t
  ;;

  let rec kind : type (a : any). a t -> a Typerep.Kind.t = function
    | Int -> Value
    | Int32 -> Value
    | Int64 -> Value
    | Nativeint -> Value
    | Char -> Value
    | Float -> Value
    | String -> Value
    | Bytes -> Value
    | Bool -> Value
    | Unit -> Value
    | Option _ -> Value
    | Or_null _ -> Value_or_null
    | List _ -> Value
    | Array _ -> Value
    | Lazy _ -> Value
    | Ref _ -> Value
    | Function _ -> Value
    | Tuple _ -> Value
    | Record _ -> Value
    | Variant _ -> Value
    | Named (_, First t) -> kind (Portable_lazy.force t)
    | Named (_, Second kind) -> kind
    | Int32_u -> Bits32
    | Int64_u -> Bits64
    | Nativeint_u -> Word
    | Float_u -> Float64
    | Tuple_u (T2 (t1, t2)) -> Tuple2_u (kind t1, kind t2)
    | Tuple_u (T3 (t1, t2, t3)) -> Tuple3_u (kind t1, kind t2, kind t3)
    | Tuple_u (T4 (t1, t2, t3, t4)) -> Tuple4_u (kind t1, kind t2, kind t3, kind t4)
    | Tuple_u (T5 (t1, t2, t3, t4, t5)) ->
      Tuple5_u (kind t1, kind t2, kind t3, kind t4, kind t5)
  ;;
end

let typerep_of_int = Typerep.Int
let typerep_of_int32 = Typerep.Int32
let typerep_of_int32_u = Typerep.Int32_u
let typerep_of_int64 = Typerep.Int64
let typerep_of_int64_u = Typerep.Int64_u
let typerep_of_nativeint = Typerep.Nativeint
let typerep_of_nativeint_u = Typerep.Nativeint_u
let typerep_of_char = Typerep.Char
let typerep_of_float = Typerep.Float
let typerep_of_float_u = Typerep.Float_u
let typerep_of_string = Typerep.String
let typerep_of_bytes = Typerep.Bytes
let typerep_of_bool = Typerep.Bool
let typerep_of_unit = Typerep.Unit
let typerep_of_option rep = Typerep.Option rep
let typerep_of_or_null rep = Typerep.Or_null rep
let typerep_of_list rep = Typerep.List rep
let typerep_of_array rep = Typerep.Array rep
let typerep_of_lazy_t rep = Typerep.Lazy rep
let typerep_of_ref rep = Typerep.Ref rep
let typerep_of_function dom rng = Typerep.Function (dom, rng)
let typerep_of_tuple0 = Typerep.Unit
let typerep_of_tuple2 a b = Typerep.Tuple (Typerep.Tuple.T2 (a, b))
let typerep_of_tuple3 a b c = Typerep.Tuple (Typerep.Tuple.T3 (a, b, c))
let typerep_of_tuple4 a b c d = Typerep.Tuple (Typerep.Tuple.T4 (a, b, c, d))
let typerep_of_tuple5 a b c d e = Typerep.Tuple (Typerep.Tuple.T5 (a, b, c, d, e))
let typerep_of_tuple2_u a b = Typerep.Tuple_u (Typerep.Tuple_u.T2 (a, b))
let typerep_of_tuple3_u a b c = Typerep.Tuple_u (Typerep.Tuple_u.T3 (a, b, c))
let typerep_of_tuple4_u a b c d = Typerep.Tuple_u (Typerep.Tuple_u.T4 (a, b, c, d))
let typerep_of_tuple5_u a b c d e = Typerep.Tuple_u (Typerep.Tuple_u.T5 (a, b, c, d, e))

include Name_of

let value_tuple0 = ()

let typerep_of_int63, typename_of_int63 =
  let typerep_and_typename_of_int63_repr
    : type a b. (a, b) Base.Int63.Private.Repr.t -> a Typerep.t * a Typename.t
    = function
    | Base.Int63.Private.Repr.Int -> typerep_of_int, typename_of_int
    | Base.Int63.Private.Repr.Int64 -> typerep_of_int64, typename_of_int64
  in
  typerep_and_typename_of_int63_repr Base.Int63.Private.repr
;;
