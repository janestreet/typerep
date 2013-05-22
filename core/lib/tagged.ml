open Typerep_kernel.Std
open Pre_core.Std

(*
  Runtime boxed value. Also called dynamic.

  Why List of array ?
  Since you have to box everything, to build an untyped list you have to map it
  anyway, so I guess the array representation makes sense, it is more compact.
*)

type t =
| Int of int
| Int32 of int32
| Int64 of int64
| Char of char
| Float of float
| String of string
| Bool of bool
| Unit
| Option of t option
| List of t Farray.t
| Array of t Farray.t
| Lazy of t
| Ref of t
| Record of Type_struct.Record_infos.t * (Type_struct.Field.t * t) Farray.t
| Tuple of t Farray.t
| Variant of Type_struct.Variant_infos.t * Type_struct.Variant.t * t Farray.t
with sexp_of

type untyped = t

exception Type_mismatch of Type_struct.t * t with sexp

let t_of_int    t = Int t
let t_of_int32  t = Int32 t
let t_of_int64  t = Int64 t
let t_of_char   t = Char t
let t_of_float  t = Float t
let t_of_string t = String t
let t_of_bool   t = Bool t
let t_of_unit  () = Unit

let t_of_option' opt = Option opt
let t_of_option of_a t = t_of_option' (Option.map t ~f:of_a)

let t_of_list' lst = List (Farray.of_list lst)
let t_of_list of_a t = List (Farray.of_list_map t ~f:of_a)

let t_of_array' arr = Array (Farray.of_array arr ~f:(fun _ a -> a))
let t_of_array of_a t = Array (Farray.of_array t ~f:(fun _ a -> of_a a))

let t_of_ref' ref = Ref !ref
let t_of_ref of_a t = Ref (of_a !t)

let t_of_lazy_t of_a t = Lazy (of_a (Lazy.force t))
let t_of_lazy_t' = t_of_lazy_t ident

let t_of_tuple' args = Tuple args
let t_of_tuple2' (a,b) = Tuple (Farray.make2 a b)
let t_of_tuple3' (a,b,c) = Tuple (Farray.make3 a b c)
let t_of_tuple4' (a,b,c,d) = Tuple (Farray.make4 a b c d)
let t_of_tuple5' (a,b,c,d,e) = Tuple (Farray.make5 a b c d e)

exception Unexpected of (t * string) with sexp

let variant_of_args args =
  match Farray.length args with
  | 0 -> Unit
  | 1 -> Farray.get args 0
  | _ -> Tuple args

let variant_to_args args ~arity =
  let fail () = raise (Unexpected (args, Printf.sprintf "to_args arity:%d" arity)) in
  match arity with
  | 0 -> begin
    match args with
    | Unit -> Farray.empty ()
    | _ -> fail ()
  end
  | 1 -> Farray.make1 args
  | _ -> begin
    match args with
    | Tuple args ->
      let len = Farray.length args in
      if len = arity then args
      else fail ()
    | _ -> fail ()
  end

module Of_typed = struct

  module Computation_impl = struct
    type 'a t = 'a -> untyped
    include Typerep_kernel.Intf.M(struct type nonrec 'a t = 'a t end)

    let int    = t_of_int
    let int32  = t_of_int32
    let int64  = t_of_int64
    let char   = t_of_char
    let float  = t_of_float
    let string = t_of_string
    let bool   = t_of_bool
    let unit   = t_of_unit

    let option = t_of_option
    let list   = t_of_list
    let array  = t_of_array
    let ref_   = t_of_ref
    let lazy_t = t_of_lazy_t

    let function_ _ = assert false

    let record record typed =
      let infos =
        let has_double_array_tag = Record.has_double_array_tag record in
        { Type_struct.Record_infos.
          has_double_array_tag;
        }
      in
      let arr = Farray.init (Record.length record) ~f:(fun index ->
        match Record.field record index with
        | Record.Field field ->
          let label = Field.label field in
          let index = Field.index field in
          let ufield = { Type_struct.Field.label ; index } in
          ufield, Field.traverse field (Field.get field typed)
      ) in
      Record (infos, arr)

    (* Beware, ca a needs to be evaluated before cb b *)
    let tuple2 ca cb (a,b) =
      let a = ca a in
      let b = cb b in
      Tuple (Farray.make2 a b)

    let tuple3 ca cb cc (a,b,c) =
      let a = ca a in
      let b = cb b in
      let c = cc c in
      Tuple (Farray.make3 a b c)

    let tuple4 ca cb cc cd (a,b,c,d) =
      let a = ca a in
      let b = cb b in
      let c = cc c in
      let d = cd d in
      Tuple (Farray.make4 a b c d)

    let tuple5 ca cb cc cd ce (a,b,c,d,e) =
      let a = ca a in
      let b = cb b in
      let c = cc c in
      let d = cd d in
      let e = ce e in
      Tuple (Farray.make5 a b c d e)

    let variant variant typed =
      let infos =
        let kind =
          Type_struct.Variant.Kind.(
            if Variant.is_polymorphic variant then Polymorphic else Usual
          )
        in
        { Type_struct.Variant_infos.
          kind;
        }
      in
      match Variant.value variant typed with
      | Variant.Value (tag, args) ->
        let label = Tag.label tag in
        let index = Tag.index tag in
        let ocaml_repr = Tag.ocaml_repr tag in
        let arity = Tag.arity tag in
        let args =
          let value = Tag.traverse tag args in
          variant_to_args value ~arity
        in
        let tag = {
          Type_struct.Variant.
          label;
          index;
          ocaml_repr;
        }
        in
        Variant (infos, tag, args)

    module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = 'a
      type 'a output = untyped
      type 'a t = 'a input -> 'a output
    end)
  end

  include Type_generic.Make(struct
    include Computation_impl
    let name = "of_typed"
    let required = [ Type_struct.Generic.ident ]
  end)
end

let int_of_t = function
  | Int i -> i
  | expr -> raise (Unexpected (expr, "int"))

let int32_of_t = function
  | Int32 i -> i
  | expr -> raise (Unexpected (expr, "int32"))

let int64_of_t = function
  | Int64 t -> t
  | expr -> raise (Unexpected (expr, "int64"))

let char_of_t = function
  | Char t -> t
  | expr -> raise (Unexpected (expr, "char"))

let float_of_t = function
  | Float t -> t
  | expr -> raise (Unexpected (expr, "float"))

let bool_of_t = function
  | Bool t -> t
  | expr -> raise (Unexpected (expr, "bool"))

let string_of_t = function
  | String t -> t
  | expr -> raise (Unexpected (expr, "string"))

let unit_of_t = function
  | Unit -> ()
  | expr -> raise (Unexpected (expr, "unit"))

let option_of_t of_a = function
  | Option t -> Option.map t ~f:of_a
  | expr -> raise (Unexpected (expr, "option"))

let list_of_t of_a = function
  | List t -> Farray.to_list_map t ~f:of_a
  | expr -> raise (Unexpected (expr, "list"))

let array_of_t of_a = function
  | Array t -> Farray.to_array t ~f:(fun _ a -> of_a a)
  | expr -> raise (Unexpected (expr, "array"))

let lazy_t_of_t of_a = function
  | Lazy t -> lazy (of_a t)
  | expr -> raise (Unexpected (expr, "lazy"))

let ref_of_t of_a = function
  | Ref t -> ref (of_a t)
  | expr -> raise (Unexpected (expr, "ref"))

let option_of_t' = function
  | Option t -> t
  | expr -> raise (Unexpected (expr, "option"))

let list_of_t' = function
  | List t -> Farray.to_list t
  | expr -> raise (Unexpected (expr, "list"))

let array_of_t' = function
  | Array t -> Farray.to_array t ~f:(fun _ x -> x)
  | expr -> raise (Unexpected (expr, "array"))

let ref_of_t' = function
  | Ref t -> ref t
  | expr -> raise (Unexpected (expr, "ref"))

let lazy_t_of_t' = function
  | Lazy t -> lazy t
  | expr -> raise (Unexpected (expr, "lazy"))

let tuple_of_t' = function
  | Tuple args -> args
  | expr -> raise (Unexpected (expr, "tuple"))

let tuple2_of_t' = function
  | Tuple array when Farray.length array = 2 ->
    Farray.get array 0,
    Farray.get array 1
  | expr -> raise (Unexpected (expr, "tuple2"))

let tuple3_of_t' = function
  | Tuple array when Farray.length array = 3 ->
    Farray.get array 0,
    Farray.get array 1,
    Farray.get array 2
  | expr -> raise (Unexpected (expr, "tuple3"))

let tuple4_of_t' = function
  | Tuple array when Farray.length array = 4 ->
    Farray.get array 0,
    Farray.get array 1,
    Farray.get array 2,
    Farray.get array 3
  | expr -> raise (Unexpected (expr, "tuple4"))

let tuple5_of_t' = function
  | Tuple array when Farray.length array = 5 ->
    Farray.get array 0,
    Farray.get array 1,
    Farray.get array 2,
    Farray.get array 3,
    Farray.get array 4
  | expr -> raise (Unexpected (expr, "tuple5"))

let field i expr =
  let fail () = raise (Unexpected (expr, Printf.sprintf "field %d" i)) in
  if i < 0 then fail ();
  match expr with
  | Option t -> begin
    match t with
    | Some t when i = 0 -> t
    | _ -> fail ()
  end
  | Tuple args
  | List args
  | Array args
  | Variant (_, _, args)
    ->
    if i >= Farray.length args then fail ();
    Farray.get args i
  | Lazy t
  | Ref t
    -> if i = 0 then t else fail ()
  | Record (_, fields) ->
    if i >= Farray.length fields then fail ();
    snd (Farray.get fields i)
  | Int _
  | Int32 _
  | Int64 _
  | Char _
  | Float _
  | String _
  | Bool _
  | Unit
    -> raise (Unexpected (expr, Printf.sprintf "field %d" i))

module Typed_of = struct

  module Computation_impl = struct
    type 'a t = untyped -> 'a
    include Typerep_kernel.Intf.M(struct type nonrec 'a t = 'a t end)

    let int = int_of_t
    let int32 = int32_of_t
    let int64 = int64_of_t
    let char = char_of_t
    let float = float_of_t
    let string = string_of_t
    let bool = bool_of_t
    let unit = unit_of_t
    let option = option_of_t
    let list = list_of_t
    let array = array_of_t
    let lazy_t = lazy_t_of_t
    let ref_ = ref_of_t

    let function_ _ = assert false

    let record record = function
      | Record (_, fields) as expr ->
        let get field =
          let index = Field.index field in
          if index >= Farray.length fields
          then raise (Unexpected (expr, "record"))
          else
            let untyped_val = snd (Farray.get fields index) in
            Field.traverse field untyped_val
        in
        Record.create record { Record.get }

      | expr -> raise (Unexpected (expr, "record"))

    (* Beware, ca a should be evaluated before cb a *)
    let tuple2 ca cb = function
      | Tuple array when Farray.length array = 2 ->
        let a = ca (Farray.get array 0) in
        let b = cb (Farray.get array 1) in
        a, b
      | expr -> raise (Unexpected (expr, "tuple2"))

    let tuple3 ca cb cc = function
      | Tuple array when Farray.length array = 3 ->
        let a = ca (Farray.get array 0) in
        let b = cb (Farray.get array 1) in
        let c = cc (Farray.get array 2) in
        a, b, c
      | expr -> raise (Unexpected (expr, "tuple3"))

    let tuple4 ca cb cc cd = function
      | Tuple array when Farray.length array = 4 ->
        let a = ca (Farray.get array 0) in
        let b = cb (Farray.get array 1) in
        let c = cc (Farray.get array 2) in
        let d = cd (Farray.get array 3) in
        a, b, c, d
      | expr -> raise (Unexpected (expr, "tuple4"))

    let tuple5 ca cb cc cd ce = function
      | Tuple array when Farray.length array = 5 ->
        let a = ca (Farray.get array 0) in
        let b = cb (Farray.get array 1) in
        let c = cc (Farray.get array 2) in
        let d = cd (Farray.get array 3) in
        let e = ce (Farray.get array 4) in
        a, b, c, d, e
      | expr -> raise (Unexpected (expr, "tuple5"))

    let variant variant =
      let memo = Flat_map.Flat_int_map.init (Variant.length variant)
        ~f:(fun index ->
          match Variant.tag variant index with
          | (Variant.Tag tag) as data ->
            let index = Tag.index tag in
            let ocaml_repr = Tag.ocaml_repr tag in
            let key = if Variant.is_polymorphic variant then ocaml_repr else index in
            key, data
        )
      in
      (function
      | Variant ({Type_struct.Variant_infos.kind}, tag, args) as expr -> begin
        let tag_m =
          let repr =
            if Type_struct.Variant.Kind.is_polymorphic kind
            then tag.Type_struct.Variant.ocaml_repr
            else tag.Type_struct.Variant.index
          in
          match Flat_map.Flat_int_map.find memo repr with
          | Some tag_m -> tag_m
          | None -> raise (Unexpected (expr, "variant"))
        in
        match tag_m with
        | Variant.Tag tag -> begin
          match Tag.create tag with
          | Tag.Const const -> const
          | Tag.Args create ->
            let arity = Tag.arity tag in
            let value =
              match arity with
              | 0 -> raise (Unexpected (expr, "variant"))
              | 1 when Farray.length args = 1 -> Tag.traverse tag (Farray.get args 0)
              | 1 -> raise (Unexpected (expr, "variant"))
              | _ -> Tag.traverse tag (Tuple args)
            in
            create value
        end
      end
      | expr -> raise (Unexpected (expr, "variant"))
      )

    module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = untyped
      type 'a output = 'a
      type 'a t = 'a input -> 'a output
    end)
  end

  include Type_generic.Make(struct
    include Computation_impl
    let name = "typed_of"
    let required = [ Type_struct.Generic.ident ]
  end)
end

module Record = struct
  let unpack_with_fields_check fields untyped =
    match untyped with
    | Record (_, untyped_fields)
        when Farray.length untyped_fields = Farray.length fields ->
      let iter (expected, _) (read, _) =
        let { Type_struct.Field.label = elabel ; index = eindex } = expected in
        let { Type_struct.Field.label = rlabel ; index = rindex } = read in
        if
          not (String.equal elabel rlabel)
          || not (Int.equal eindex rindex)
        then
          invalid_arg "dummy" (* catched in untyped_generic *)
      in
      Farray.iter2_exn ~f:iter fields untyped_fields;
      untyped_fields
    | _ -> invalid_arg "dummy" (* catched in untyped_generic *)

  let unpack = function
    | Record (infos, fields) -> infos, fields
    | expr -> raise (Unexpected (expr, "record"))

  let pack infos fields = Record (infos, fields)

  let field_by_name expr name =
    let (_, fields) = unpack expr in
    let field = Farray.findi fields ~f:(fun _ (field, case) ->
      let name' = Type_struct.Field.label field in
      if String.compare name name' = 0 then Some case else None)
    in
    match field with
    | Some value -> value
    | None -> raise (Unexpected (expr, Printf.sprintf "record with field %s" name))
end

module Variant = struct

  let of_args = variant_of_args

  let to_args = variant_to_args

  let match_with_repr expr ~repr =
    match expr with
    | Variant ({Type_struct.Variant_infos.kind},
               { Type_struct.Variant.ocaml_repr; index;
                 label=_ },
               args) ->
      let repr' =
        if Type_struct.Variant.Kind.is_polymorphic kind
        then ocaml_repr
        else index
      in
      if repr = repr' then Some args
      else None
    | Option opt -> begin
      match opt with
      | Some t -> if repr = 1 then Some (Farray.make1 t) else None
      | None -> if repr = 0 then Some (Farray.empty ()) else None
    end
    | _ -> raise (Unexpected (expr, "variant"))

  let unpack expr =
    match expr with
    | Variant ({Type_struct.Variant_infos.kind},
               {Type_struct.Variant.ocaml_repr; index;
                label=_ },
               args) ->
      let repr =
        if Type_struct.Variant.Kind.is_polymorphic kind
        then ocaml_repr
        else index
      in
      `repr repr, args
    | Option opt -> begin
      match opt with
      | None -> `repr 0, Farray.empty ()
      | Some t -> `repr 1, Farray.make1 t
    end
    | _ -> raise (Unexpected (expr, "variant"))

  let unpack_name expr =
    match expr with
    | Variant (_, { Type_struct.Variant.label=name;
                    ocaml_repr=_ ; index=_},
               args) ->
      `name name, args
    | Option opt -> begin
      match opt with
      | None -> `name "None", Farray.empty ()
      | Some t -> `name "Some", Farray.make1 t
    end
    | _ -> raise (Unexpected (expr, "variant"))

  module Make(X: sig
    val infos : Type_struct.Variant_infos.t
    val branches : (Type_struct.Variant.t * Type_struct.t Farray.t) Farray.t
  end) = struct
    include X

    let str = Type_struct.Variant (infos, branches)

    let find_variant =
      let fail variant =
        raise (Type_mismatch (str, Variant (infos, variant, Farray.empty ())))
      in
      match infos.Type_struct.Variant_infos.kind with
      | Type_struct.Variant.Kind.Polymorphic ->
        let map =
          let branches = Farray.to_array branches ~f:(fun index (variant, args) ->
            variant, (index, args))
          in
          let key (variant, _) =
            variant.Type_struct.Variant.label
          in
          Flat_map.Flat_string_map.of_array ~f:key branches
        in
        (* a polymorphic variant may be extended, we want to lookup by name *)
        (fun ({ Type_struct.Variant.label=name;
                ocaml_repr=_; index=_;
              } as variant) ->
          match Flat_map.Flat_string_map.find map name with
          | Some (_,data) -> data
          | None -> fail variant)

      | Type_struct.Variant.Kind.Usual ->
        (* even if a usual variant is extended, the repr should be compatible *)
        (fun ({ Type_struct.Variant.label=name; index;
                ocaml_repr=_;
              } as variant) ->
          if index < Farray.length branches then
            let (variant_case, variant_args) = Farray.get branches index in
            if String.equal variant_case.Type_struct.Variant.label name then
              (index, variant_args)
            else fail variant
          else fail variant)

    let get_tag_of_untyped untyped =
      match untyped with
      | Variant (infos', variant, args) ->
        if Type_struct.Variant_infos.equal infos infos'
        then
          let (index, variant_args) = find_variant variant in
          let arity = Farray.length args in
          if arity <> Farray.length variant_args
          then raise (Type_mismatch (str, untyped))
          else
            let value = of_args args in
            `index index, value
        else
          raise (Type_mismatch (str, untyped))
      | _ -> raise (Type_mismatch (str, untyped))

    let pack variant ~args =
      Variant (infos, variant, args)
  end
end
