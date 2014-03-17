open Typerep_lib.Std
open Pre_core.Std

module type S = sig
  type 'a t
  val of_typestruct : Type_struct.t -> [ `generic of Tagged.t t ]
end

module type Adapter = sig

  type 'a t
  type 'a adapter = 'a t -> Tagged.t t

  val int : int adapter
  val int32 : int32 adapter
  val int64 : int64 adapter
  val nativeint : nativeint adapter
  val char : char adapter
  val bool : bool adapter
  val string : string adapter
  val float : float adapter
  val unit : unit adapter

  val option : Tagged.t option adapter
  val list : Tagged.t list adapter
  val array : Tagged.t array adapter
  val ref_ : Tagged.t ref adapter
  val lazy_t : Tagged.t lazy_t adapter

  val tuple2 : (Tagged.t * Tagged.t) adapter
  val tuple3 : (Tagged.t * Tagged.t * Tagged.t) adapter
  val tuple4 : (Tagged.t * Tagged.t * Tagged.t * Tagged.t) adapter
  val tuple5 : (Tagged.t * Tagged.t * Tagged.t * Tagged.t * Tagged.t) adapter
end

module Make_advanced
  (A:Adapter)
  (X:Type_generic.Computation with type 'a t = 'a A.t)
  : S with type 'a t = 'a X.t = struct

  type 'a t = 'a X.t

  exception Unknown_named of Type_struct.t * Type_struct.Name.t with sexp
  exception Type_mismatch of Type_struct.t * Tagged.t with sexp

  module S = Type_struct

  let of_typestruct str =
    let context = X.Named.Context.create () in
    let table = Int.Table.create () in
    let rec of_typestruct str =
      match str with
      | S.Int -> A.int X.int
      | S.Int32 -> A.int32 X.int32
      | S.Int64 -> A.int64 X.int64
      | S.Nativeint -> A.nativeint X.nativeint
      | S.Char -> A.char X.char
      | S.Float -> A.float X.float
      | S.String -> A.string X.string
      | S.Bool -> A.bool X.bool
      | S.Unit -> A.unit X.unit

      | S.Option str ->
        A.option (X.option (of_typestruct str))

      | S.List str ->
        A.list (X.list (of_typestruct str))

      | S.Array str ->
        A.array (X.array (of_typestruct str))

      | S.Ref str ->
        A.ref_ (X.ref_ (of_typestruct str))

      | S.Lazy str ->
        A.lazy_t (X.lazy_t (of_typestruct str))

      | S.Tuple arr ->
        let comp n = of_typestruct (Farray.get arr n) in
        begin match Farray.length arr with
        | 2 ->
          let c0 = comp 0 in
          let c1 = comp 1 in
          A.tuple2 (X.tuple2 c0 c1)
        | 3 ->
          let c0 = comp 0 in
          let c1 = comp 1 in
          let c2 = comp 2 in
          A.tuple3 (X.tuple3 c0 c1 c2)
        | 4 ->
          let c0 = comp 0 in
          let c1 = comp 1 in
          let c2 = comp 2 in
          let c3 = comp 3 in
          A.tuple4 (X.tuple4 c0 c1 c2 c3)
        | 5 ->
          let c0 = comp 0 in
          let c1 = comp 1 in
          let c2 = comp 2 in
          let c3 = comp 3 in
          let c4 = comp 4 in
          A.tuple5 (X.tuple5 c0 c1 c2 c3 c4)
        | _ -> assert false
        end

      | S.Record (infos, fields_str) ->
        let module Typename_of_t = Make_typename.Make0(struct
          type t = Tagged.t
          let name = "dynamic record"
        end) in
        let typename = Typerep.Named.typename_of_t Typename_of_t.named in
        let fields = Farray.map fields_str ~f:(fun (field, str) ->
          let { Type_struct.Field.index ; label } = field in
          let get untyped =
            let fields =
              try Tagged.Record.unpack_with_fields_check fields_str untyped
              with Invalid_argument _ -> raise (Type_mismatch (str, untyped))
            in
            let value = snd (Farray.get fields index) in
            value
          in
          X.Field.internal_use_only {
            X.Field_internal.
            label;
            rep = of_typestruct str;
            index;
            tyid = Typename.create ();
            get;
          }
        ) in
        let has_double_array_tag = infos.Type_struct.Record_infos.has_double_array_tag in
        let create { X.Record_internal.get } =
          let fields = Farray.mapi fields_str ~f:(fun index (label, _) ->
            label, get (Farray.get fields index)
          ) in
          Tagged.Record.pack infos fields
        in
        let record = X.Record.internal_use_only {
          X.Record_internal.
          typename;
          fields = Farray.to_array ~f:(fun _ f -> X.Record_internal.Field f) fields;
          has_double_array_tag;
          create
        } in
        X.record record

      | S.Variant ({Type_struct.Variant_infos.kind} as infos, tags_str) ->
        let module Variant = Tagged.Variant.Make(struct
          let infos = infos
          let branches = tags_str
        end) in
        let module Typename_of_t = Make_typename.Make0(struct
          type t = Tagged.t
          let name = "dynamic record"
        end) in
        let typename = Typerep.Named.typename_of_t Typename_of_t.named in
        let tags = Farray.mapi tags_str ~f:(fun index (variant, args) ->
          let str = Type_struct.type_struct_of_variant_args args in
          let index =
            (if index <> variant.Type_struct.Variant.index then assert false);
            index
          in
          let ocaml_repr = variant.Type_struct.Variant.ocaml_repr in
          let arity = Farray.length args in
          let label = variant.Type_struct.Variant.label in
          let create =
            if arity = 0
            then X.Tag_internal.Const (Variant.pack variant ~args:(Farray.empty ()))
            else X.Tag_internal.Args (fun value ->
              let args = Tagged.Variant.to_args value ~arity in
              Variant.pack variant ~args
            )
          in
          X.Tag.internal_use_only {
            X.Tag_internal.
            label;
            rep = of_typestruct str;
            arity;
            index;
            ocaml_repr;
            tyid = Typename.create ();
            create;
          }
        ) in
        let polymorphic = Type_struct.Variant.Kind.is_polymorphic kind in
        let value untyped =
          let (`index index), value = Variant.get_tag_of_untyped untyped in
          X.Variant_internal.Value (Farray.get tags index, value)
        in
        let variant = X.Variant.internal_use_only {
          X.Variant_internal.
          typename;
          tags = Farray.to_array ~f:(fun _ t -> X.Variant_internal.Tag t) tags;
          polymorphic;
          value;
        } in
        X.variant variant

      | S.Named (key, content) -> begin
        match Int.Table.find table key with
        | Some shared -> X.Named.get_wip_computation shared
        | None -> begin
          match content with
          | Some str ->
            let shared =
              let module M = Typename.Make0(struct
                type t = Tagged.t
                let name = "untyped-uniq"
              end) in
              X.Named.init context M.typename_of_t
            in
            Int.Table.set table ~key ~data:shared;
            let computation = of_typestruct str in
            X.Named.set_final_computation shared computation
          | None ->
            raise (Unknown_named (str, key))
        end
      end

    in
    let computation = of_typestruct str in
    `generic computation
end

module Writer_adapter(Builder:sig
  type 'a t
  val make : (Tagged.t -> 'a) -> 'a t -> Tagged.t t
end) : Adapter with type 'a t = 'a Builder.t
= struct
  include Builder
  type 'a adapter = 'a t -> Tagged.t t

  let int    clos = make Tagged.int_of_t clos
  let int32  clos = make Tagged.int32_of_t clos
  let int64  clos = make Tagged.int64_of_t clos
  let nativeint clos = make Tagged.nativeint_of_t clos
  let char   clos = make Tagged.char_of_t clos
  let float  clos = make Tagged.float_of_t clos
  let bool   clos = make Tagged.bool_of_t clos
  let string clos = make Tagged.string_of_t clos
  let unit   clos = make Tagged.unit_of_t clos
  let option clos = make Tagged.option_of_t' clos
  let list   clos = make Tagged.list_of_t' clos
  let array  clos = make Tagged.array_of_t' clos
  let ref_   clos = make Tagged.ref_of_t' clos
  let lazy_t clos = make Tagged.lazy_t_of_t' clos

  let tuple2 clos = make Tagged.tuple2_of_t' clos
  let tuple3 clos = make Tagged.tuple3_of_t' clos
  let tuple4 clos = make Tagged.tuple4_of_t' clos
  let tuple5 clos = make Tagged.tuple5_of_t' clos
end

module Reader_adapter(Builder:sig
  type 'a t
  val make : ('a -> Tagged.t) -> 'a t -> Tagged.t t
end) : Adapter with type 'a t = 'a Builder.t
= struct
  include Builder
  type 'a adapter = 'a t -> Tagged.t t


  let int    clos = make Tagged.t_of_int clos
  let int32  clos = make Tagged.t_of_int32 clos
  let int64  clos = make Tagged.t_of_int64 clos
  let nativeint clos = make Tagged.t_of_nativeint clos
  let char   clos = make Tagged.t_of_char clos
  let bool   clos = make Tagged.t_of_bool clos
  let string clos = make Tagged.t_of_string clos
  let float  clos = make Tagged.t_of_float clos
  let unit   clos = make Tagged.t_of_unit clos
  let option clos = make Tagged.t_of_option' clos
  let list   clos = make Tagged.t_of_list' clos
  let array  clos = make Tagged.t_of_array' clos
  let ref_   clos = make Tagged.t_of_ref' clos
  let lazy_t clos = make Tagged.t_of_lazy_t' clos

  let tuple2 clos = make Tagged.t_of_tuple2' clos
  let tuple3 clos = make Tagged.t_of_tuple3' clos
  let tuple4 clos = make Tagged.t_of_tuple4' clos
  let tuple5 clos = make Tagged.t_of_tuple5' clos
end

module Input_adapter(Input:sig
  type t
end) : Adapter with type 'a t = Input.t -> 'a
= struct

  include Reader_adapter(struct
    type 'a t = Input.t -> 'a
    let make to_untyped clos = (fun input -> to_untyped (clos input))
  end)
end

module Output_adapter(Output:sig
  type t
end) : Adapter with type 'a t = 'a -> Output.t
= struct

  include Writer_adapter(struct
    type 'a t = 'a -> Output.t
    let make to_typed clos = (fun untyped -> clos (to_typed untyped))
  end)
end

module Make_input (Input:sig type t end) = Make_advanced(Input_adapter(Input))
module Make_output (Output:sig type t end) = Make_advanced(Output_adapter(Output))

module Make_writer (Builder:sig
  type 'a t
  val make : (Tagged.t -> 'a) -> 'a t -> Tagged.t t
end) = Make_advanced(Writer_adapter(Builder))

module Make_reader(Builder:sig
  type 'a t
  val make : ('a -> Tagged.t) -> 'a t -> Tagged.t t
end) = Make_advanced(Reader_adapter(Builder))

