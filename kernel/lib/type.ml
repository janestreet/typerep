module rec Rep : sig

  type _ t =
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Char : char t
    | Float : float t
    | String : string t
    | Bool : bool t
    | Unit : unit t
    | Option : 'a t -> 'a option t
    | List : 'a t -> 'a list t
    | Array : 'a t -> 'a array t
    | Lazy : 'a t -> 'a Lazy.t t
    | Ref : 'a t -> 'a ref t
    | Function : ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple : 'a Rep.Tuple.t -> 'a t
    | Record : 'a Rep.Record.t -> 'a t
    | Variant : 'a Rep.Variant.t -> 'a t
    | Named : ('a Rep.Named.t * 'a t Lazy.t option) -> 'a t

  module Named : sig
    module type T0 = sig
      type named
      type t
      val typename_of_named : named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, named) Type_equal.t
    end
    module type T1 = sig
      type 'a named
      type a val a : a Rep.t
      type t
      val typename_of_named : 'a Type_name.t -> 'a named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, a named) Type_equal.t
    end
    module type T2 = sig
      type ('a, 'b) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> ('a, 'b) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b) named) Type_equal.t
    end
    module type T3 = sig
      type ('a, 'b, 'c) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> ('a, 'b, 'c) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end
    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type d val d : d Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> 'd Type_name.t
        -> ('a, 'b, 'c, 'd) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end
    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type d val d : d Rep.t
      type e val e : e Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> 'd Type_name.t
        -> 'e Type_name.t
        -> ('a, 'b, 'c, 'd, 'e) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end
    (* there the module is necessary because we need to deal with a type [t] with
       parameters whose kind is not representable as a type variable: ['a 't], even with
       a gadt. *)
    type 'a t =
    | T0 of (module T0 with type t = 'a)
    | T1 of (module T1 with type t = 'a)
    | T2 of (module T2 with type t = 'a)
    | T3 of (module T3 with type t = 'a)
    | T4 of (module T4 with type t = 'a)
    | T5 of (module T5 with type t = 'a)

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Type_name.t
    val name : _ t -> string
  end

  module Tuple : sig
    (* these constructors could be plunged at toplevel of Rep.t, however it is less
       verbose that way *)
    type _ t =
    | T2 : ('a Rep.t * 'b Rep.t)
      -> ('a * 'b) t
    | T3 : ('a Rep.t * 'b Rep.t * 'c Rep.t)
      -> ('a * 'b * 'c) t
    | T4 : ('a Rep.t * 'b Rep.t * 'c Rep.t * 'd Rep.t)
      -> ('a * 'b * 'c * 'd) t
    | T5 : ('a Rep.t * 'b Rep.t * 'c Rep.t * 'd Rep.t * 'e Rep.t)
      -> ('a * 'b * 'c * 'd * 'e) t

    val arity : _ t -> int
    val typename_of_t : 'a t -> 'a Type_name.t
  end

  include Intf.S with type 'a t := 'a Rep.t

  val same : _ t -> _ t -> bool
  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
  val typename_of_t : 'a t -> 'a Type_name.t
  val head : 'a t -> 'a t
end = struct

  module Name_of = struct
    module M_int = Type_name.Make0(struct type t = int let name = "int" end)
    let int = M_int.typename_of_t

    module M_int32 = Type_name.Make0(struct type t = int32 let name = "int32" end)
    let int32 = M_int32.typename_of_t

    module M_int64 = Type_name.Make0(struct type t = int64 let name = "int64" end)
    let int64 = M_int64.typename_of_t

    module M_char = Type_name.Make0(struct type t = char let name = "char" end)
    let char = M_char.typename_of_t

    module M_float = Type_name.Make0(struct type t = float let name = "float" end)
    let float = M_float.typename_of_t

    module M_string = Type_name.Make0(struct type t = string let name = "string" end)
    let string = M_string.typename_of_t

    module M_bool = Type_name.Make0(struct type t = bool let name = "bool" end)
    let bool = M_bool.typename_of_t

    module M_unit = Type_name.Make0(struct type t = unit let name = "unit" end)
    let unit = M_unit.typename_of_t

    module M_option = Type_name.Make1(struct type 'a t = 'a option let name = "option" end)
    let option = M_option.typename_of_t

    module M_list = Type_name.Make1(struct type 'a t = 'a list let name = "list" end)
    let list = M_list.typename_of_t

    module M_array = Type_name.Make1(struct type 'a t = 'a array let name = "array" end)
    let array = M_array.typename_of_t

    module M_lazy_t = Type_name.Make1(struct type 'a t = 'a lazy_t let name = "lazy_t" end)
    let lazy_t = M_lazy_t.typename_of_t

    module M_ref = Type_name.Make1(struct type 'a t = 'a ref let name = "ref" end)
    let ref_ = M_ref.typename_of_t

    module M_function = Type_name.Make2(struct
      type ('a, 'b) t = 'a -> 'b
      let name = "function"
    end)
    let function_ = M_function.typename_of_t

    module M_tuple2 = Type_name.Make2(struct
      type ('a, 'b) t = 'a * 'b
      let name = "tuple2"
    end)
    let tuple2 = M_tuple2.typename_of_t

    module M_tuple3 = Type_name.Make3(struct
      type ('a, 'b, 'c) t = 'a * 'b * 'c
      let name = "tuple3"
    end)
    let tuple3 = M_tuple3.typename_of_t

    module M_tuple4 = Type_name.Make4(struct
      type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd
      let name = "tuple4"
    end)
    let tuple4 = M_tuple4.typename_of_t

    module M_tuple5 = Type_name.Make5(struct
      type ('a, 'b, 'c, 'd, 'e) t = 'a * 'b * 'c *'d * 'e
      let name = "tuple5"
    end)
    let tuple5 = M_tuple5.typename_of_t
  end

  type _ t =
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Char : char t
    | Float : float t
    | String : string t
    | Bool : bool t
    | Unit : unit t
    | Option : 'a t -> 'a option t
    | List : 'a t -> 'a list t
    | Array : 'a t -> 'a array t
    | Lazy : 'a t -> 'a Lazy.t t
    | Ref : 'a t -> 'a ref t
    | Function : ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple : 'a Rep.Tuple.t -> 'a t
    | Record : 'a Rep.Record.t -> 'a t
    | Variant : 'a Rep.Variant.t -> 'a t
    | Named : ('a Rep.Named.t * 'a t Lazy.t option) -> 'a t

  module Named = struct
    module type T0 = sig
      type named
      type t
      val typename_of_named : named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, named) Type_equal.t
    end
    module type T1 = sig
      type 'a named
      type a val a : a Rep.t
      type t
      val typename_of_named : 'a Type_name.t -> 'a named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, a named) Type_equal.t
    end
    module type T2 = sig
      type ('a, 'b) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> ('a, 'b) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b) named) Type_equal.t
    end
    module type T3 = sig
      type ('a, 'b, 'c) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> ('a, 'b, 'c) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c) named) Type_equal.t
    end
    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type d val d : d Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> 'd Type_name.t
        -> ('a, 'b, 'c, 'd) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end
    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a val a : a Rep.t
      type b val b : b Rep.t
      type c val c : c Rep.t
      type d val d : d Rep.t
      type e val e : e Rep.t
      type t
      val typename_of_named :
        'a Type_name.t
        -> 'b Type_name.t
        -> 'c Type_name.t
        -> 'd Type_name.t
        -> 'e Type_name.t
        -> ('a, 'b, 'c, 'd, 'e) named Type_name.t
      val typename_of_t : t Type_name.t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end
    (* there the module is necessary because we need to deal with a type [t] with
       parameters whose kind is not representable as a type variable: ['a 't], even with
       a gadt. *)
    type 'a t =
    | T0 of (module T0 with type t = 'a)
    | T1 of (module T1 with type t = 'a)
    | T2 of (module T2 with type t = 'a)
    | T3 of (module T3 with type t = 'a)
    | T4 of (module T4 with type t = 'a)
    | T5 of (module T5 with type t = 'a)

    let arity = function
      | T0 _ -> 0
      | T1 _ -> 1
      | T2 _ -> 2
      | T3 _ -> 3
      | T4 _ -> 4
      | T5 _ -> 5

    let typename_of_t (type a) = function
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

    let name rep =
      Type_name.Uid.name (Type_name.uid (typename_of_t rep))
  end

  module Tuple = struct
    (* these constructors could be plunged at toplevel of Rep.t, however it is less
       verbose this way *)
    type _ t =
    | T2 : ('a Rep.t * 'b Rep.t)
      -> ('a * 'b) t
    | T3 : ('a Rep.t * 'b Rep.t * 'c Rep.t)
      -> ('a * 'b * 'c) t
    | T4 : ('a Rep.t * 'b Rep.t * 'c Rep.t * 'd Rep.t)
      -> ('a * 'b * 'c * 'd) t
    | T5 : ('a Rep.t * 'b Rep.t * 'c Rep.t * 'd Rep.t * 'e Rep.t)
      -> ('a * 'b * 'c * 'd * 'e) t

    let arity : type a. a t -> int = function
      | Rep.Tuple.T2 _ -> 2
      | Rep.Tuple.T3 _ -> 3
      | Rep.Tuple.T4 _ -> 4
      | Rep.Tuple.T5 _ -> 5

    let typename_of_t : type a. a t -> a Type_name.t = function
      | T2 (a, b) ->
        Name_of.tuple2
          (Rep.typename_of_t a)
          (Rep.typename_of_t b)
      | T3 (a, b, c) ->
        Name_of.tuple3
          (Rep.typename_of_t a)
          (Rep.typename_of_t b)
          (Rep.typename_of_t c)
      | T4 (a, b, c, d) ->
        Name_of.tuple4
          (Rep.typename_of_t a)
          (Rep.typename_of_t b)
          (Rep.typename_of_t c)
          (Rep.typename_of_t d)
      | T5 (a, b, c, d, e) ->
        Name_of.tuple5
          (Rep.typename_of_t a)
          (Rep.typename_of_t b)
          (Rep.typename_of_t c)
          (Rep.typename_of_t d)
          (Rep.typename_of_t e)
  end

  include Intf.M (struct type 'a rep = 'a t type 'a t = 'a rep end)

  let rec typename_of_t : type a. a t -> a Type_name.t = function
    | Int        -> Name_of.int
    | Int32      -> Name_of.int32
    | Int64      -> Name_of.int64
    | Char       -> Name_of.char
    | Float      -> Name_of.float
    | String     -> Name_of.string
    | Bool       -> Name_of.bool
    | Unit       -> Name_of.unit

    | Option rep -> Name_of.option (typename_of_t rep)
    | List rep   -> Name_of.list (typename_of_t rep)
    | Array rep  -> Name_of.array (typename_of_t rep)
    | Lazy rep   -> Name_of.lazy_t (typename_of_t rep)
    | Ref rep    -> Name_of.ref_ (typename_of_t rep)

    | Function (dom, rng) -> Name_of.function_ (typename_of_t dom) (typename_of_t rng)

    | Tuple rep -> Rep.Tuple.typename_of_t rep

    | Record rep -> Rep.Record.typename_of_t rep
    | Variant rep -> Rep.Variant.typename_of_t rep

    | Named (name, _) -> Named.typename_of_t name

  (* mbarbin: I have considered matching on the pair but I prefer the current
     implementation *)
  let rec same_witness : type a b. a t -> b t -> (a, b) Type_equal.t option = fun t1 t2 ->
    match t1 with
    | Named (name1, r1) -> begin
      match t2 with
      | Named (name2, r2) -> begin
        match Type_name.same_witness
          (Named.typename_of_t name1)
          (Named.typename_of_t name2)
        with
        | None -> begin
          match r1, r2 with
          | Some (lazy r1), Some (lazy r2) -> same_witness r1 r2
          | Some (lazy r1), None -> same_witness r1 t2
          | None, Some (lazy r2) -> same_witness t1 r2
          | None, None -> None
        end
        | some -> some
      end
      | _ -> begin
        match r1 with
        | Some (lazy t1) -> same_witness t1 t2
        | None -> None
      end
    end
    | Int -> begin
      match t2 with
      | Int -> Some Type_equal.refl
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Int32 -> begin
      match t2 with
      | Int32 -> Some Type_equal.refl
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Int64 -> begin
      match t2 with
      | Int64 -> Some Type_equal.refl
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Char -> begin
      match t2 with
      | Char -> Some Type_equal.refl
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Float -> begin
      match t2 with
      | Float -> Some Type_equal.refl
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | String -> begin
      match t2 with
      | String -> Some Type_equal.refl
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Bool -> begin
      match t2 with
      | Bool -> Some Type_equal.refl
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Unit -> begin
      match t2 with
      | Unit -> Some Type_equal.refl
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Option r1 -> begin
      match t2 with
      | Option r2 -> begin
        match same_witness r1 r2 with
        | None -> None
        | Some Type_equal.T -> Some Type_equal.T
      end
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | List r1 -> begin
      match t2 with
      | List r2 -> begin
        match same_witness r1 r2 with
        | None -> None
        | Some Type_equal.T -> Some Type_equal.T
      end
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Array r1 -> begin
      match t2 with
      | Array r2 -> begin
        match same_witness r1 r2 with
        | None -> None
        | Some Type_equal.T -> Some Type_equal.T
      end
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Lazy r1 -> begin
      match t2 with
      | Lazy r2 -> begin
        match same_witness r1 r2 with
        | None -> None
        | Some Type_equal.T -> Some Type_equal.T
      end
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Ref r1 -> begin
      match t2 with
      | Ref r2 -> begin
        match same_witness r1 r2 with
        | None -> None
        | Some Type_equal.T -> Some Type_equal.T
      end
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Function (dom1, rng1) -> begin
      match t2 with
      | Function (dom2, rng2) -> begin
        match (same_witness dom1 dom2, same_witness rng1 rng2) with
        | (None, _) | (_, None) -> None
        | (Some Type_equal.T, Some Type_equal.T) ->
          Some Type_equal.T
      end
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Tuple tuple -> begin
      match tuple with
      | Rep.Tuple.T2 (a1, b1) -> begin
        match t2 with
        | Tuple (Rep.Tuple.T2 (a2, b2)) -> begin
          match (same_witness a1 a2, same_witness b1 b2) with
          | (None, _) | (_, None) -> None
          | (Some Type_equal.T,
             Some Type_equal.T)
            -> Some Type_equal.T
        end
        | Named (_, Some (lazy t2)) -> same_witness t1 t2
        | _ -> None
      end
      | Rep.Tuple.T3 (a1, b1, c1) -> begin
        match t2 with
        | Tuple (Rep.Tuple.T3 (a2, b2, c2)) -> begin
          match (same_witness a1 a2, same_witness b1 b2, same_witness c1 c2) with
          | (None, _, _) | (_, None, _) | (_, _, None) -> None
          | (Some Type_equal.T,
             Some Type_equal.T,
             Some Type_equal.T)
            -> Some Type_equal.T
        end
        | Named (_, Some (lazy t2)) -> same_witness t1 t2
        | _ -> None
      end
      | Rep.Tuple.T4 (a1, b1, c1, d1) -> begin
        match t2 with
        | Tuple (Rep.Tuple.T4 (a2, b2, c2, d2)) -> begin
          match (
            same_witness a1 a2,
            same_witness b1 b2,
            same_witness c1 c2,
            same_witness d1 d2
          )
          with
          | (None, _, _, _)
          | (_, None, _, _)
          | (_, _, None, _)
          | (_, _, _, None)
            -> None
          | (Some Type_equal.T,
             Some Type_equal.T,
             Some Type_equal.T,
             Some Type_equal.T) ->
            Some Type_equal.T
        end
        | Named (_, Some (lazy t2)) -> same_witness t1 t2
        | _ -> None
      end
      | Rep.Tuple.T5 (a1, b1, c1, d1, e1) -> begin
        match t2 with
        | Tuple (Rep.Tuple.T5 (a2, b2, c2, d2, e2)) -> begin
          match (
            same_witness a1 a2,
            same_witness b1 b2,
            same_witness c1 c2,
            same_witness d1 d2,
            same_witness e1 e2
          )
          with
          | (None, _, _, _, _)
          | (_, None, _, _, _)
          | (_, _, None, _, _)
          | (_, _, _, None, _)
          | (_, _, _, _, None)
            -> None
          | (Some Type_equal.T,
             Some Type_equal.T,
             Some Type_equal.T,
             Some Type_equal.T,
             Some Type_equal.T) ->
            Some Type_equal.T
        end
        | Named (_, Some (lazy t2)) -> same_witness t1 t2
        | _ -> None
      end
    end
    | Record r1 -> begin
      match t2 with
      | Record r2 ->
        Type_name.same_witness
          (Rep.Record.typename_of_t r1)
          (Rep.Record.typename_of_t r2)
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end
    | Variant r1 -> begin
      match t2 with
      | Variant r2 ->
        Type_name.same_witness
          (Rep.Variant.typename_of_t r1)
          (Rep.Variant.typename_of_t r2)
      | Named (_, Some (lazy t2)) -> same_witness t1 t2
      | _ -> None
    end

  let same a b = same_witness a b <> None
  let same_witness_exn a b =
    match same_witness a b with
    | Some proof -> proof
    | None -> assert false

  let rec head = function
    | Rep.Named (_, Some (lazy t)) -> head t
    | t -> t
end

module Std = struct

  module Typerep = struct
    include Rep
    module Intf = Intf
  end

  let typerep_of_int    = Typerep.Int
  let typerep_of_int32  = Typerep.Int32
  let typerep_of_int64  = Typerep.Int64
  let typerep_of_char   = Typerep.Char
  let typerep_of_float  = Typerep.Float
  let typerep_of_string = Typerep.String
  let typerep_of_bool   = Typerep.Bool
  let typerep_of_unit   = Typerep.Unit

  let typerep_of_option rep = Typerep.Option rep
  let typerep_of_list   rep = Typerep.List   rep
  let typerep_of_array  rep = Typerep.Array  rep
  let typerep_of_lazy_t rep = Typerep.Lazy   rep
  let typerep_of_ref    rep = Typerep.Ref    rep

  let typerep_of_function dom rng = Typerep.Function (dom, rng)

  type tuple0 = unit
  let value_tuple0 = ()
  let typerep_of_tuple0 = Typerep.Unit
  module M_tuple0 = Type_name.Make0(struct type t = tuple0 let name = "tuple0" end)
  let typename_of_tuple0 = M_tuple0.typename_of_t

  let typerep_of_tuple2 a b = Typerep.Tuple (Typerep.Tuple.T2 (a, b))
  let typerep_of_tuple3 a b c = Typerep.Tuple (Typerep.Tuple.T3 (a, b, c))
  let typerep_of_tuple4 a b c d = Typerep.Tuple (Typerep.Tuple.T4 (a, b, c, d))
  let typerep_of_tuple5 a b c d e = Typerep.Tuple (Typerep.Tuple.T5 (a, b, c, d, e))
end

