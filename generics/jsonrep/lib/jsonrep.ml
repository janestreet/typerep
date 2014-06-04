open Core.Std
open Typerep_experimental.Std

module type S = sig
  module Of_json : Type_generic.S with type 'a t = Json.Json_type.t -> 'a
  module Json_of : Type_generic.S with type 'a t = 'a -> Json.Json_type.t
  val t_of_json : 'a Typerep.t -> [`generic of Json.Json_type.t -> 'a]
  val json_of_t : 'a Typerep.t -> [`generic of 'a -> Json.Json_type.t]
  module Tagged : sig
    module Of_json : Tagged_generic.S with type 'a t = Json.Json_type.t -> 'a
    module Json_of : Tagged_generic.S with type 'a t = 'a -> Json.Json_type.t
    val t_of_json : Type_struct.t -> [ `generic of Json.Json_type.t -> Tagged.t ]
    val json_of_t : Type_struct.t -> [ `generic of Tagged.t -> Json.Json_type.t ]
  end
end

module C = Conv

module Jt = struct
  (* we want [Json.Json_type.t] with sexp in order to get better error messages *)
  type json_type = Json.Json_type.json_type =
  | Object of (string * json_type) list
  | Array of json_type list
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
  with sexp

  type t = json_type with sexp

  include (Json.Json_type : (module type of Json.Json_type
               with type json_type := json_type
                and type t := t
  ))
end

exception Type_mismatch of string * Jt.t with sexp

module Of_json_impl = struct
  type 'a t = Jt.t -> 'a
  include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

  let function_ arg_of_json ret_of_json = C.function_of_json arg_of_json ret_of_json

  let int = C.int_of_json
  let int32 = C.int32_of_json
  let int64 = C.int64_of_json
  let nativeint = C.nativeint_of_json
  let char = C.char_of_json
  let float = C.float_of_json
  let string = C.string_of_json
  let bool = C.bool_of_json
  let unit = C.unit_of_json
  let option = C.option_of_json
  let list = C.list_of_json
  let array = C.array_of_json
  let lazy_t = C.lazy_t_of_json
  let ref_ = C.ref_of_json
  let tuple2 = C.tuple2_of_json
  let tuple3 = C.tuple3_of_json
  let tuple4 = C.tuple4_of_json
  let tuple5 = C.tuple5_of_json
  ;;

  (* The following two functions are used by the various implementations of [record] *)
  let json_properties json =
    match json with
    | Jt.Object json_properties ->
      let seen = String.Hash_set.create () in
      let iter (name, _) =
        if Hash_set.mem seen name then
          raise (Type_mismatch("Record: json with duplicate key " ^ name, json));
        Hash_set.add seen name;
      in
      List.iter ~f:iter json_properties;
      json_properties
    | Jt.Array values -> List.mapi values ~f:(fun index elt ->
      (* this automatic generation of fields name offers a way to load json types
         containing tuples with large arity, typically > 5 *)
      let field = Printf.sprintf "f%d" index in
      field, elt
    )
    | _ -> raise (Type_mismatch("Record", json))
  ;;

  let variant variant =
    let tag_by_label =
      let f index =
        match Variant.tag variant index with
        | (Variant.Tag tag) as data -> Tag.label tag, data
      in
      Flat_map.Flat_string_map.init (Variant.length variant) ~f
    in
    let t_of_json json =
      let fail () = raise (Type_mismatch ("Variant",json)) in
      match json with
      | Jt.String label -> begin
          match Flat_map.Flat_string_map.find tag_by_label label with
          | None -> fail ()
          | Some (Variant.Tag tag) -> begin
              match Tag.create tag with
              | Tag.Const const -> const
              | Tag.Args _ -> fail ()
            end
        end
      | Jt.Array ((Jt.String label)::jt_values) -> begin
          match Flat_map.Flat_string_map.find tag_by_label label with
          | None -> fail ()
          | Some (Variant.Tag tag) -> begin
              match Tag.create tag with
              | Tag.Args create ->
                let arity = Tag.arity tag in
                let jt_value =
                  if arity = 1
                  then match jt_values with
                    | [jt_value] -> jt_value
                    | _ -> fail ()
                  else Jt.Array jt_values
                in
                create (Tag.traverse tag jt_value)
              | Tag.Const _ -> fail ()
            end
        end
      | _ -> fail ()
    in
    t_of_json
  ;;

  module Named = Type_generic.Make_named_for_closure(struct
    type 'a input = Jt.t
    type 'a output = 'a
    type 'a t = Jt.t -> 'a
  end)
end

module Json_of_impl = struct
  type 'a t = 'a -> Jt.t
  include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

  let int = C.json_of_int
  let int32 = C.json_of_int32
  let int64 = C.json_of_int64
  let nativeint = C.json_of_nativeint
  let char = C.json_of_char
  let float = C.json_of_float
  let string = C.json_of_string
  let bool = C.json_of_bool
  let unit = C.json_of_unit
  let option = C.json_of_option
  let list = C.json_of_list
  let array = C.json_of_array
  let lazy_t = C.json_of_lazy_t
  let ref_ = C.json_of_ref
  let function_ = C.json_of_function

  let tuple2 = C.json_of_tuple2
  let tuple3 = C.json_of_tuple3
  let tuple4 = C.json_of_tuple4
  let tuple5 = C.json_of_tuple5

  let variant variant =
    (* preallocation of atoms *)
    let atoms = Array.init (Variant.length variant) ~f:(fun index ->
      match Variant.tag variant index with
      | Variant.Tag tag -> Jt.Build.string (Tag.label tag)
    ) in
    fun value ->
      match Variant.value variant value with
      | Variant.Value (tag, args) ->
        let index = Tag.index tag in
        let arity = Tag.arity tag in
        let atom = atoms.(index) in
        match arity with
        | 0 -> atom
        | 1 -> Jt.Array [atom ; Tag.traverse tag args]
        | _ ->
          match Tag.traverse tag args with
          | Jt.Array values -> Jt.Array (atom::values)
          | _ -> assert false

  module Named = Type_generic.Make_named_for_closure(struct
    type 'a input = 'a
    type 'a output = Jt.t
    type 'a t = 'a -> Jt.t
  end)
end

module type X_record = sig
  val version : int
  module Of_json : sig
    val record : 'a Of_json_impl.Record.t -> 'a Of_json_impl.t
  end
  module Json_of : sig
    val record : 'a Json_of_impl.Record.t -> 'a Json_of_impl.t
  end
end

module Make(X:X_record) = struct
  module Of_json = Type_generic.Make(struct
    include Of_json_impl
    include X.Of_json
    let name = sprintf "of_json_v%d" X.version
    let required = []
  end)
  module Json_of = Type_generic.Make(struct
    include Json_of_impl
    include X.Json_of
    let name = sprintf "json_of_v%d" X.version
    let required = []
  end)
  let t_of_json = Of_json.of_typerep
  let json_of_t = Json_of.of_typerep
  module Tagged = struct
    module Of_json = Tagged_generic.Make_input (Jt)(Of_json.Computation)
    module Json_of = Tagged_generic.Make_output(Jt)(Json_of.Computation)
    let t_of_json = Of_json.of_typestruct
    let json_of_t = Json_of.of_typestruct
  end
end

module V2 = Make(struct
  let version = 2
  module Of_json = struct
    open Of_json_impl
    let record record = fun json ->
      let json_properties = json_properties json in
      let properties = lazy (Flat_map.Flat_string_map.of_alist json_properties) in
      let get field =
        let label = Field.label field in
        let index = Field.index field in
        let json_value =
          match List.nth json_properties index with
          | Some (json_name, json_value) when  String.equal json_name label -> json_value
          | Some _ | None ->
            match Flat_map.Flat_string_map.find (Lazy.force properties) label with
            | Some x -> x
            | None -> Jt.Null
        in
        try Field.traverse field json_value
        with
        | exn ->
          failwiths "Exception while deserializing field"
            (label, json, exn)
            <:sexp_of< string * Jt.t * Exn.t>>
      in
      Record.create record { Record.get }
    ;;
  end
  module Json_of = struct
    open Json_of_impl
    let record record value =
      let rec aux acc index =
        if index < 0 then Jt.Build.objekt acc
        else
          let acc =
            match Record.field record index with
            | Record.Field field ->
              match Field.traverse field (Field.get field value) with
              | Jt.Null -> acc
              | json ->
                (Field.label field, json) :: acc
          in
          aux acc (pred index)
      in
      aux [] (pred (Record.length record))
    ;;
  end
end)

module V1 = Make(struct
  let version = 1
  module Of_json = struct
    open Of_json_impl
    let record record = fun json ->
      let json_properties = json_properties json in
      let properties = lazy (Flat_map.Flat_string_map.of_alist json_properties) in
      let get field =
        let label = Field.label field in
        let index = Field.index field in
        let json_value =
          match List.nth json_properties index with
          | Some (json_name, json_value) ->
            if String.equal json_name label then json_value
            else begin
              match Flat_map.Flat_string_map.find (Lazy.force properties) label with
              | Some x -> x
              | None ->
                failwithf "Field %s is present in the destination record but not in the \
                           source JSON." label ()
            end
          | _ ->
            failwithf "Source JSON has %d fields, while destination record has more."
              (index + 1) ()
        in
        Field.traverse field json_value
      in
      Record.create record { Record.get }
  end
  module Json_of = struct
    open Json_of_impl
    let record record value =
      let rec aux acc index =
        if index < 0 then Jt.Build.objekt acc
        else
          let field =
            match Record.field record index with
            | Record.Field field ->
              let label = Field.label field in
              label, Field.traverse field (Field.get field value)
          in
          aux (field::acc) (pred index)
      in
      aux [] (pred (Record.length record))
  end
end)
