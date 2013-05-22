open Typerep_kernel.Std
open Pre_core.Std

module Name = struct
  include Int
  let typerep_of_t = typerep_of_int
  let make_fresh () =
    let index = ref (-1) in
    (fun () -> incr index; !index)
  let incompatible = (-1)
end

module Variant = struct
  module Kind = struct
    type t =
    | Polymorphic
    | Usual
    with sexp, typerep, bin_io

    let is_polymorphic = function
      | Polymorphic -> true
      | Usual -> false
    let equal a b =
      match a with
      | Polymorphic -> b = Polymorphic
      | Usual -> b = Usual
  end
  module V1 = struct
    type t = {
      name : string;
      repr : int;
    } with sexp, bin_io, typerep
  end
  module V2 = struct
    type t = {
      label : string;
      index : int;
      ocaml_repr : int;
    } with sexp, bin_io, typerep
  end
  include V2
  let label t = t.label
  let index t = t.index
  let ocaml_repr t = t.ocaml_repr
  let to_v1 kind t =
    let name = t.label in
    let repr = if Kind.is_polymorphic kind then t.ocaml_repr else t.index in
    { V1.name ; repr }
  let of_v1 kind index this all v1 =
    if Kind.is_polymorphic kind
    then
      { label = v1.V1.name;
        index;
        ocaml_repr = v1.V1.repr; }
    else
      let ocaml_repr =
        let no_arg = Farray.is_empty this in
        let rec count pos acc =
          if pos = index then acc
          else
            let _, args = Farray.get all pos in
            let acc = if no_arg = Farray.is_empty args then succ acc else acc in
            count (succ pos) acc
        in
        count 0 0
      in
      { label = v1.V1.name;
        index = (if index <> v1.V1.repr then assert false; index);
        ocaml_repr;
      }

  module Option = struct
    let none = {
      index = 0;
      ocaml_repr = 0;
      label = "None";
    }
    let some = {
      index = 1;
      ocaml_repr = 0;
      label = "Some";
    }
  end
end

module Variant_infos = struct
  module V1 = struct
    type t = {
      kind : Variant.Kind.t;
    } with sexp, bin_io, typerep
  end
  include V1
  let equal t t' = Variant.Kind.equal t.kind t'.kind
end

module Field = struct
  module V1 = struct
    type t = string with sexp, bin_io, typerep
  end
  module V2 = struct
    type t = {
      label : string;
      index : int;
    } with sexp, bin_io, typerep
  end
  (* module V3 = struct
   *   type t = {
   *     label : string;
   *     index : int;
   *     is_mutable : bool;
   *     etc...
   *   }
   * end *)
  include V2
  let label t = t.label
  let index t = t.index
  let to_v1 t = t.label
  let of_v1 index label = {
    label;
    index;
  }
end

module Record_infos = struct
  module V1 = struct
    type t = {
      has_double_array_tag : bool;
    } with sexp, bin_io, typerep
  end
  include V1
  let equal t t' = t.has_double_array_tag = t'.has_double_array_tag
end

module T = struct
  type t =
  | Int
  | Int32
  | Int64
  | Char
  | Float
  | String
  | Bool
  | Unit
  | Option of t
  | List of t
  | Array of t
  | Lazy of t
  | Ref of t
  | Tuple of t Farray.t
  | Record of Record_infos.t * (Field.t * t) Farray.t
  | Variant of Variant_infos.t * (Variant.t * t Farray.t) Farray.t
  | Named of Name.t * t option
  with sexp, typerep
end
include T

type type_struct = t with sexp

let incompatible () = Named (Name.incompatible, None)

let get_variant_by_repr {Variant_infos.kind} cases repr =
  if Variant.Kind.is_polymorphic kind
  then
    Farray.findi cases ~f:(fun _ ((variant, _) as case) ->
      if Int.equal variant.Variant.ocaml_repr repr then Some case else None)
  else
    let length = Farray.length cases in
    if repr < 0 || repr >= length then None else
      let (variant, _) as case = Farray.get cases repr in
      (if variant.Variant.index <> repr then assert false);
      Some case

let get_variant_by_label _infos cases label =
  Farray.findi cases ~f:(fun _ ((variant, _) as case) ->
    if String.equal variant.Variant.label label then Some case else None)

let rec is_polymorphic_variant = function
  | Named (_, Some content) -> is_polymorphic_variant content
  | Variant ({Variant_infos.kind}, _) -> Variant.Kind.is_polymorphic kind
  | _ -> false

let variant_args_of_type_struct ~arity str =
  match arity with
  | 0 ->
    if str = Unit
    then Farray.empty ()
    else assert false
  | 1 -> Farray.make1 str
  | _ ->
    match str with
    | Tuple args -> args
    | _ -> assert false (* ill formed ast *)

let type_struct_of_variant_args args =
  match Farray.length args with
  | 0 -> Unit
  | 1 -> Farray.get args 0
  | _ -> Tuple args

module Option_as_variant = struct
  open Variant
  let kind = Kind.Usual
  let infos = {
    Variant_infos.
    kind;
  }
  let make some_type =
    infos, Farray.make2
      (Option.none, Farray.empty ())
      (Option.some, Farray.make1 some_type)
end

let option_as_variant ~some = Option_as_variant.make some

class traverse =
object(self)
  method iter t =
    let f t = self#iter t in
    match t with
    | Int
    | Int32
    | Int64
    | Char
    | Float
    | String
    | Bool
    | Unit
      -> ()
    | Option t -> f t
    | List t -> f t
    | Array t -> f t
    | Lazy t -> f t
    | Ref t -> f t
    | Tuple args ->
      Farray.iter ~f args
    | Record ((_:Record_infos.t), fields) ->
      Farray.iter ~f:(fun ((_:Field.t), t) -> f t) fields
    | Variant ((_:Variant_infos.t), cases) ->
      Farray.iter ~f:(fun ((_:Variant.t), args) -> Farray.iter ~f args) cases
    | Named (_, link) -> Option.iter link ~f

  method map t =
    let map_stable_snd ~f ((a, b) as t) =
      let b' = f b in
      if phys_equal b b' then t else (a, b')
    in
    let f t = self#map t in
    match t with
    | Int
    | Int32
    | Int64
    | Char
    | Float
    | String
    | Bool
    | Unit
        -> t
    | Option arg ->
      let arg' = f arg in
      if phys_equal arg arg' then t else Option arg'
    | List arg ->
      let arg' = f arg in
      if phys_equal arg arg' then t else List arg'
    | Array arg ->
      let arg' = f arg in
      if phys_equal arg arg' then t else Array arg'
    | Lazy arg ->
      let arg' = f arg in
      if phys_equal arg arg' then t else Lazy arg'
    | Ref arg ->
      let arg' = f arg in
      if phys_equal arg arg' then t else Ref arg'
    | Tuple args ->
      let args' = Farray.map_stable ~f args in
      if phys_equal args args' then t else Tuple args'
    | Record (infos, fields) ->
      let fields' = Farray.map_stable ~f:(map_stable_snd ~f) fields in
      if phys_equal fields fields' then t else Record (infos, fields')
    | Variant (infos, cases) ->
      let cases' = Farray.map_stable cases
        ~f:(map_stable_snd ~f:(Farray.map_stable ~f))
      in
      if phys_equal cases cases' then t else Variant (infos, cases')
    | Named (name, link) ->
      let link' = Option.map_stable ~f link in
      if phys_equal link link' then t else Named (name, link')
end

module Raw = struct
  module T = struct
    type nonrec t = t with sexp
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
  end
  include Hashable.Make(T)
end

module Named_utils(X:sig
  type t with sexp_of
  class traverse : object
    method iter : t -> unit
    method map : t -> t
  end
  val match_named : t -> [ `Named of Name.t * t option | `Other of t ]
  val cons_named : Name.t -> t option -> t
end) = struct
  let has_named t =
    let module M = struct
      exception Exists
    end in
    let exists = object
      inherit X.traverse as super
      method! iter t =
        match X.match_named t with
        | `Named _ -> raise M.Exists
        | `Other t -> super#iter t
    end in
    try exists#iter t; false with M.Exists -> true

  let remove_dead_links t =
    let used = Name.Hash_set.create () in
    let has_named = ref false in
    let collect = object
      inherit X.traverse as super
      method! iter t =
        super#iter t;
        match X.match_named t with
        | `Named (name, link) ->
          has_named := true;
          if Option.is_none link then Name.Hash_set.add used name
        | `Other _ -> ()
    end in
    collect#iter t;
    if not !has_named then t else begin
      let map = object
        inherit X.traverse as super
        method! map t =
          let t = super#map t in
          match X.match_named t with
          | `Named (name, Some arg) ->
            if Name.Hash_set.mem used name then t else arg
          | `Named (_, None) | `Other _ -> t
      end in
      map#map t
    end

  let alpha_conversion t =
    if not (has_named t) then t else (* allocation optimization *)
    let fresh_name = Name.make_fresh () in
    let table = Name.Table.create () in
    let rename i =
      match Name.Table.find table i with
      | Some i -> i
      | None ->
        let name = fresh_name () in
        Name.Table.set table ~key:i ~data:name;
        name
    in
    let rename = object
      inherit X.traverse as super
      method! map t =
        match X.match_named t with
        | `Named (name, arg) ->
          let name' = rename name in
          let t =
            if Name.equal name name' then t else X.cons_named name' arg
          in
          super#map t
        | `Other t -> super#map t
    end in
    rename#map t

  exception Invalid_recursive_name of Name.t * X.t with sexp

  let standalone_exn ~readonly (t:X.t) =
    if not (has_named t) then t else (* allocation optimization *)
    let local_table = Name.Table.create () in
    let or_lookup name content =
      match content with
      | Some data ->
        Name.Table.set local_table ~key:name ~data;
        content
      | None ->
        match Name.Table.find local_table name with
        | Some _ as content -> content
        | None ->
          match Name.Table.find readonly name with
          | (Some data) as content ->
            Name.Table.set local_table ~key:name ~data;
            content
          | None -> raise (Invalid_recursive_name (name, t))
    in
    let seen = Name.Hash_set.create () in
    let enrich = object
      inherit X.traverse as super
      method! map t =
        let t =
          match X.match_named t with
          | `Named (name, content) ->
            if Name.Hash_set.mem seen name then begin
              if Option.is_none content then t else
                X.cons_named name None
            end else begin
              Name.Hash_set.add seen name;
              let content' = or_lookup name content in
              if phys_equal content content'
              then t
              else X.cons_named name content'
            end
          | `Other t -> t
        in
        super#map t
    end in
    enrich#map t
end

include Named_utils(struct
  type t = type_struct with sexp_of
  class traverse_non_rec = traverse
  class traverse = traverse_non_rec
  let match_named = function
    | Named (name, link) -> `Named (name, link)
    | t -> `Other t
  let cons_named name contents = Named (name, contents)
end)

let reduce t =
  let fresh_name = Name.make_fresh () in
  let alias = Name.Table.create () in
  let consing = Raw.Table.create () in
  let share t =
    match t with
    | Named (name, None) -> begin
      match Name.Table.find alias name with
      | Some shared_name ->
        if Name.equal name shared_name then t else Named (shared_name, None)
      | None -> t
    end
    | Named (name, Some key) -> begin
      match Raw.Table.find consing key with
      | Some (shared_name, shared) ->
        if not (Name.equal name shared_name) then
          Name.Table.set alias ~key:name ~data:shared_name;
        shared
      | None ->
        let shared = Named (name, None) in
        let data = name, shared in
        Raw.Table.set consing ~key ~data;
        t
    end
    | (Record _ | Variant _) as key -> begin
      match Raw.Table.find consing key with
      | Some (_, shared) ->
        shared
      | None -> begin
        let shared_name = fresh_name () in
        let shared = Named (shared_name, None) in
        let data = shared_name, shared in
        Raw.Table.set consing ~key ~data;
        Named (shared_name, Some key)
      end
    end
    | Int
    | Int32
    | Int64
    | Char
    | Float
    | String
    | Bool
    | Unit
    | Option _
    | List _
    | Array _
    | Lazy _
    | Ref _
    | Tuple _
      -> t
  in
  let share = object
    inherit traverse as super
    method! map t =
      let t = super#map t in (* deep first *)
      share t
  end in
  let shared = share#map t in
  let reduced = remove_dead_links shared in
  reduced

exception Invalid_recursive_typestruct of Name.t * t with sexp

let sort_variant_cases cases =
  let cmp (variant, _) (variant', _) =
    String.compare variant.Variant.label variant'.Variant.label
  in
  Farray.sort ~cmp cases

module Pairs = struct
  module T = struct
    type t = Name.t * Name.t with sexp
    let compare (a, b) (a', b') =
      let cmp = Name.compare a a' in
      if cmp <> 0 then cmp else Name.compare b b'
    let hash = Hashtbl.hash
  end
  include Hashable.Make(T)
end

let equivalent_array f a b =
  let len_a = Farray.length a in
  let len_b = Farray.length b in
  if len_a <> len_b then false else
    let rec aux index = if index = len_a then true else
        f (Farray.get a index) (Farray.get b index) && aux (succ index)
    in
    aux 0

let are_equivalent a b =
  let wip = Pairs.Table.create () in
  let start_pair name name' =
    Pairs.Table.set wip ~key:(name, name') ~data:`started
  in
  let finish_pair name name' result =
    Pairs.Table.set wip ~key:(name, name') ~data:(`finished result)
  in
  let status name name' =
    match Pairs.Table.find wip (name, name') with
    | Some ((`started | `finished _) as status) -> status
    | None -> `unknown
  in
  let table_a = Name.Table.create () in
  let table_b = Name.Table.create () in
  let or_lookup table name = function
    | (Some data) as str -> Name.Table.set table ~key:name ~data ; str
    | None -> Name.Table.find table name
  in
  let rec aux a b =
    match a, b with
    | Named (name, struct_a), Named (name', struct_b) -> begin
      match status name name' with
      | `unknown -> begin
        let struct_a = or_lookup table_a name struct_a in
        let struct_b = or_lookup table_b name struct_b in
        match struct_a, struct_b with
        | Some struct_a, Some struct_b ->
          start_pair name name';
          let res = aux struct_a struct_b in
          finish_pair name name' res;
          res
        | Some _, None
        | None, Some _
        | None, None
          -> false
      end
      | `started -> true
      | `finished res -> res
    end

    | Named (name, struct_a), struct_b -> begin
      let struct_a = or_lookup table_a name struct_a in
      match struct_a with
      | None -> false
      | Some struct_a ->
        aux struct_a struct_b
    end

    | struct_a, Named (name, struct_b) -> begin
      let struct_b = or_lookup table_b name struct_b in
      match struct_b with
      | None -> false
      | Some struct_b ->
        aux struct_a struct_b
    end

    | Int, Int
    | Int32, Int32
    | Int64, Int64
    | Char, Char
    | Float, Float
    | String, String
    | Bool, Bool
    | Unit, Unit
      -> true

    | Option t, Option t'
    | List t, List t'
    | Array t, Array t'
    | Lazy t, Lazy t'
    | Ref t, Ref t'
      -> aux t t'

    | Tuple tys, Tuple tys' ->
      equivalent_array aux tys tys'

    | Record (infos, fields), Record (infos', fields') ->
      Record_infos.equal infos infos' &&
        let eq (field, t) (field', t') =
          String.equal field.Field.label field'.Field.label
          && Int.equal field.Field.index field'.Field.index
          && aux t t'
        in
        equivalent_array eq fields fields'

    | Variant (infos, cases), Variant (infos', cases') ->
      Variant_infos.equal infos infos' &&
        let is_polymorphic = Variant.Kind.is_polymorphic infos.Variant_infos.kind in
        let cases = if is_polymorphic then sort_variant_cases cases else cases in
        let cases' = if is_polymorphic then sort_variant_cases cases' else cases' in
        let eq (variant, args) (variant', args') =
          Int.equal variant.Variant.ocaml_repr variant'.Variant.ocaml_repr
          && String.equal variant.Variant.label variant'.Variant.label
          && equivalent_array aux args args'
        in
        equivalent_array eq cases cases'

    | Int, _
    | Int32, _
    | Int64, _
    | Char, _
    | Float, _
    | String, _
    | Bool, _
    | Unit, _
    | Option _, _
    | List _, _
    | Array _, _
    | Lazy _, _
    | Ref _, _
    | Record _, _
    | Variant _, _
    | Tuple _, _
      -> false
  in
  aux a b

let combine_array ~fail combine a b =
  let len = Farray.length a in
  if Farray.length b <> len then fail () else
  Farray.init len ~f:(fun index ->
    combine
      (Farray.get a index)
      (Farray.get b index)
  )

module Incompatible_types = struct
  (* these exception are used to create human readable error messages for
     least_upper_bound. we use exception with sexp rather than an error monad because
     this library does not depend on core, and dealing with exception is the easiest way
     to plunge this library in a world using a error monad, as soon as there exists a
     [try_with] function *)
  exception Invalid_recursive_structure of t * t with sexp
  exception Field_conflict of t * t * (Field.t * Field.t) with sexp
  exception Types_conflict of t * t with sexp
end

let least_upper_bound_exn a b =
  let open Incompatible_types in
  let wip = Pairs.Table.create () in
  let fresh_name = Name.make_fresh () in
  let merge_named_pair name name' =
    let merged_name = fresh_name () in
    Pairs.Table.set wip ~key:(name, name') ~data:(`name merged_name);
    merged_name
  in
  let status name name' =
    match Pairs.Table.find wip (name, name') with
    | Some ((`name _) as status) -> status
    | None -> `unknown
  in
  let table_a = Name.Table.create () in
  let table_b = Name.Table.create () in
  let or_lookup table name = function
    | (Some data) as str -> Name.Table.set table ~key:name ~data ; str
    | None -> Name.Table.find table name
  in
  let rec aux a b =
    let fail () = raise (Types_conflict (a, b)) in
    match a, b with
    | Named (name, struct_a), Named (name', struct_b) -> begin
      match status name name' with
      | `unknown -> begin
        let struct_a = or_lookup table_a name struct_a in
        let struct_b = or_lookup table_b name struct_b in
        match struct_a, struct_b with
        | Some struct_a, Some struct_b -> begin
          let name = merge_named_pair name name' in
          let content = aux struct_a struct_b in
          Named (name, Some content)
        end
        | Some _, None
        | None, Some _
        | None, None
          -> raise (Invalid_recursive_structure (a, b))
      end
      | `name name -> Named (name, None)
    end

    | Named (name, struct_a), struct_b -> begin
      let struct_a = or_lookup table_a name struct_a in
      match struct_a with
      | None -> raise (Invalid_recursive_structure (a, b))
      | Some struct_a ->
        aux struct_a struct_b
    end

    | struct_a, Named (name, struct_b) -> begin
      let struct_b = or_lookup table_b name struct_b in
      match struct_b with
      | None -> raise (Invalid_recursive_structure (a, b))
      | Some struct_b ->
        aux struct_a struct_b
    end

    | Int, Int
    | Int32, Int32
    | Int64, Int64
    | Char, Char
    | Float, Float
    | String, String
    | Bool, Bool
    | Unit, Unit
      -> a

    | Option t, Option t' -> Option (aux t t')
    | List t, List t'     -> List (aux t t')
    | Array t, Array t'   -> Array (aux t t')
    | Lazy t, Lazy t'     -> Lazy (aux t t')
    | Ref t, Ref t'       -> Ref (aux t t')

    | Tuple tys, Tuple tys' ->
      let args = combine_array ~fail aux tys tys' in
      Tuple args

    | Record (infos, fields), Record (infos', fields') ->
      if Record_infos.equal infos infos'
      then
        let combine (field, t) (field', t') =
          if
            String.equal field.Field.label field'.Field.label
            && Int.equal field.Field.index field'.Field.index
          then (field, (aux t t'))
          else raise (Field_conflict (a, b, (field, field')))
        in
        let fields = combine_array ~fail combine fields fields' in
        Record (infos, fields)
      else fail ()

    | Variant (infos_a, cases_a), Variant (infos_b, cases_b) ->
      if Variant_infos.equal infos_a infos_b
      then
        match infos_a.Variant_infos.kind with
        | Variant.Kind.Polymorphic -> begin
          (* polymorphic variant may be merged if there is no conflict in hashing and
             arguments are compatible *)
          let repr_table = Int.Table.create () in
          let iter_variants variants =
            let len = Farray.length variants in
            let rec iter index =
              if index >= len then true
              else begin
                let (({Variant.label=name; ocaml_repr; index=_}, args) as init) =
                  (Farray.get variants index)
                in
                match Int.Table.find repr_table ocaml_repr with
                | None ->
                  Int.Table.set repr_table ~key:ocaml_repr ~data:init;
                  iter (succ index)
                | Some (({Variant.label=name'; ocaml_repr=_ ; index=_ } as variant)
                           , args') ->
                  if name <> name' then false else begin
                    let args = combine_array ~fail aux args args' in
                    let data = variant, args in
                    Int.Table.set repr_table ~key:ocaml_repr ~data;
                    iter (succ index)
                  end
              end
            in iter 0
          in
          if iter_variants cases_a && iter_variants cases_b then begin
            let cases_merged =
              Int.Table.to_init Farray.init repr_table ~f:(fun _ case -> case)
            in
            Variant (infos_a, cases_merged)
          end else fail ()
        end
        | Variant.Kind.Usual -> begin
          (* usual variant may be merged only if one is a prefix of the other and
             arguments are compatible *)
          let len_a = Farray.length cases_a in
          let len_b = Farray.length cases_b in
          let cases_merged = Farray.init (max len_a len_b) ~f:(fun index ->
            let var_a =
              if index < len_a then Some (Farray.get cases_a index) else None
            in
            let var_b =
              if index < len_b then Some (Farray.get cases_b index) else None
            in
            match var_a, var_b with
            | Some (variant_a, args_a), Some (variant_b, args_b) ->
              if
                Int.equal variant_a.Variant.ocaml_repr variant_b.Variant.ocaml_repr
                && String.equal variant_a.Variant.label variant_b.Variant.label
              then
                let args = combine_array ~fail aux args_a args_b in
                variant_a, args
              else
                fail ()
            | Some (variant, args), None ->
              variant, args
            | None, Some (variant, args) ->
              variant, args
            | None, None -> assert false
          ) in
          Variant (infos_a, cases_merged)
        end
      else fail ()

    | Int, _
    | Int32, _
    | Int64, _
    | Char, _
    | Float, _
    | String, _
    | Bool, _
    | Unit, _
    | Option _, _
    | List _, _
    | Array _, _
    | Lazy _, _
    | Ref _, _
    | Record _, _
    | Variant _, _
    | Tuple _, _
      -> fail ()
  in
  aux a b

module type Typestructable = sig
  type t
  val typestruct_of_t : type_struct
end

module Internal_generic = Type_generic.Make(struct
  type 'a t = type_struct
  module Type_struct_variant = Variant
  module Type_struct_field = Field
  include Typerep_kernel.Intf.M(struct type nonrec 'a t = 'a t end)

  let name = "typestruct"
  let required = []

  let int                 = Int
  let int32               = Int32
  let int64               = Int64
  let char                = Char
  let float               = Float
  let string              = String
  let bool                = Bool
  let unit                = Unit
  let option  str         = Option str
  let list    str         = List str
  let array   str         = Array str
  let lazy_t  str         = Lazy str
  let ref_    str         = Ref str
  let tuple2  a b         = Tuple (Farray.make2 a b)
  let tuple3  a b c       = Tuple (Farray.make3 a b c)
  let tuple4  a b c d     = Tuple (Farray.make4 a b c d)
  let tuple5  a b c d e   = Tuple (Farray.make5 a b c d e)

  let function_ _  = assert false

  let record record =
    let infos =
      let has_double_array_tag = Record.has_double_array_tag record in
      { Record_infos.
        has_double_array_tag;
      }
    in
    let fields = Farray.init (Record.length record) ~f:(fun index ->
      match Record.field record index with
      | Record.Field field ->
        let label = Field.label field in
        let type_struct = (Field.traverse field : type_struct) in
        { Type_struct_field.label ; index }, type_struct
    )
    in
    Record (infos, fields)

  let variant variant =
    let infos =
      let polymorphic = Variant.is_polymorphic variant in
      let kind =
        Type_struct_variant.Kind.(if polymorphic then Polymorphic else Usual)
      in
      { Variant_infos.
        kind;
      }
    in
    let tags = Farray.init (Variant.length variant) ~f:(fun index ->
      match Variant.tag variant index with
      | Variant.Tag tag ->
        let label = Tag.label tag in
        let index = Tag.index tag in
        let ocaml_repr = Tag.ocaml_repr tag in
        let arity = Tag.arity tag in
        let variant = {
          Type_struct_variant.
          label;
          ocaml_repr;
          index;
        } in
        let str = Tag.traverse tag in
        let args = variant_args_of_type_struct ~arity str in
        variant, args
    )
    in
    Variant (infos, tags)

  module Named = struct
    module Context = struct
      type t = {
        fresh_name : unit -> Name.t;
      }
      let create () = {
        fresh_name = Name.make_fresh ();
      }
    end

    type 'a t = Name.t

    let init context _name =
      context.Context.fresh_name ()

    let get_wip_computation shared_name = Named (shared_name, None)

    let set_final_computation shared_name str = Named (shared_name, Some str)

    let share : type a. a Typerep.t -> bool = function
      | Typerep.Int        -> false
      | Typerep.Int32      -> false
      | Typerep.Int64      -> false
      | Typerep.Char       -> false
      | Typerep.Float      -> false
      | Typerep.String     -> false
      | Typerep.Bool       -> false
      | Typerep.Unit       -> false
      | Typerep.Option _   -> false
      | Typerep.List _     -> false
      | Typerep.Array _    -> false
      | Typerep.Lazy _     -> false
      | Typerep.Ref _      -> false

      | Typerep.Function _ -> true
      | Typerep.Tuple _    -> true
      | Typerep.Record _   -> true
      | Typerep.Variant _  -> true

      | Typerep.Named _    -> false
  end
end)

module Generic = struct
  include Internal_generic
  let of_typerep_fct rep =
    let `generic str = Internal_generic.of_typerep rep in
    remove_dead_links str
  let of_typerep rep = `generic (of_typerep_fct rep)
end
let of_typerep = Generic.of_typerep_fct

let sexp_of_typerep rep = sexp_of_t (of_typerep rep)

module Diff = struct

  module Path = struct
    (* the path leading to the variant. succession of field names, variant names,
       or string representation of base types. for tuple with use 'f0', 'f1', etc. *)
    type t = string list with sexp
  end

  module Compatibility = struct
    type t = [
    | `Backward_compatible
    | `Break
    ] with sexp
  end

  module Atom = struct
    (* [str * str] means [old * new] *)
    type t =
    | Update of type_struct * type_struct
    | Add_field of (Field.t * type_struct)
    | Remove_field of (Field.t * type_struct)
    | Update_field of (Field.t * type_struct) * (Field.t * type_struct)
    | Add_variant of (Compatibility.t * Variant.t * type_struct Farray.t)
    | Remove_variant of (Variant.t * type_struct Farray.t)
    | Update_variant of
        (Variant.t * type_struct Farray.t)
      * (Variant.t * type_struct Farray.t)
    with sexp
  end

  type t = (Path.t * Atom.t) list with sexp

  (*
    The diff is done such as the length of the path associated with atoms is maximal
  *)
  let compute a b =
    let wip = Pairs.Table.create () in
    let start_pair name name' =
      Pairs.Table.set wip ~key:(name, name') ~data:`started
    in
    let status name name' =
      match Pairs.Table.find wip (name, name') with
      | Some (`started as status) -> status
      | None -> `unknown
    in
    let table_a = Name.Table.create () in
    let table_b = Name.Table.create () in
    let or_lookup table name = function
      | (Some data) as str -> Name.Table.set table ~key:name ~data ; str
      | None -> Name.Table.find table name
    in
    let diffs = Queue.create () in
    let enqueue path diff = Queue.push (path, diff) diffs in
    let rec aux_list :
        'a. add:('a -> unit)
        -> remove:('a -> unit)
        -> compute:(('a  * 'a list) -> ('a * 'a list) -> 'a list * 'a list)
        -> 'a list -> 'a list -> unit
      = fun ~add ~remove ~compute a b ->
      match a, b with
      | [], [] -> ()
      | [], _::_ -> List.iter ~f:add b
      | _::_, [] -> List.iter ~f:remove a
      | hd::tl, hd'::tl' ->
        let tl, tl' = compute (hd, tl) (hd', tl') in
        aux_list ~add ~remove ~compute tl tl'
    in
    let aux_farray :
        'a. add:('a -> unit)
        -> remove:('a -> unit)
        -> compute:(('a  * 'a list) -> ('a * 'a list) -> 'a list * 'a list)
        -> 'a Farray.t -> 'a Farray.t -> unit
      = fun ~add ~remove ~compute a b ->
      aux_list ~add ~remove ~compute (Farray.to_list a) (Farray.to_list b)
    in
    let rec aux path a b =
      match a, b with
      | Named (name_a, struct_a), Named (name_b, struct_b) -> begin
        match status name_a name_b with
        | `unknown -> begin
          let struct_a = or_lookup table_a name_a struct_a in
          let struct_b = or_lookup table_b name_b struct_b in
          match struct_a, struct_b with
          | Some struct_a, Some struct_b ->
            start_pair name_a name_b;
            aux path struct_a struct_b
          | Some _, None
          | None, Some _
          | None, None -> enqueue path (Atom.Update (a, b))
        end
        | `started -> ()
      end

      | Named (name, struct_a), struct_b -> begin
        let struct_a = or_lookup table_a name struct_a in
        match struct_a with
        | None -> enqueue path (Atom.Update (a, b))
        | Some struct_a ->
          aux path struct_a struct_b
      end

      | struct_a, Named (name, struct_b) -> begin
        let struct_b = or_lookup table_b name struct_b in
        match struct_b with
        | None -> enqueue path (Atom.Update (a, b))
        | Some struct_b ->
          aux path struct_a struct_b
      end

      | Int, Int
      | Int32, Int32
      | Int64, Int64
      | Char, Char
      | Float, Float
      | String, String
      | Bool, Bool
      | Unit, Unit
        -> ()

      | Option a, Option b -> aux ("option"::path) a b
      | List a, List b -> aux ("list"::path) a b
      | Array a, Array b -> aux ("array"::path) a b
      | Lazy a, Lazy b -> aux ("lazy"::path) a b
      | Ref a, Ref b -> aux ("ref"::path) a b

      | Tuple tys, Tuple tys' ->
        let arity = Farray.length tys in
        let arity' = Farray.length tys' in
        if arity <> arity'
        then enqueue path (Atom.Update (a, b))
        else
          (* since arity = arity', add and remove are never called *)
          let add _ = assert false in
          let remove _ =  assert false in
          let index = ref 0 in
          let compute (ty, tl) (ty', tl') =
            let path = ("f"^(string_of_int !index)) :: path in
            aux path ty ty';
            incr index;
            tl, tl'
          in
          aux_farray ~add ~remove ~compute tys tys'

      | Record (infos, fields), Record (infos', fields') ->
        if not (Record_infos.equal infos infos')
        then enqueue path (Atom.Update (a, b)) else begin
          let add hd = enqueue path (Atom.Add_field hd) in
          let remove hd = enqueue path (Atom.Remove_field hd) in
          let update a b = enqueue path (Atom.Update_field (a, b)) in
          let labels fields =
            let set = String.Hash_set.create () in
            let iter (field, _) = String.Hash_set.add set field.Field.label in
            Farray.iter ~f:iter fields;
            set
          in
          let labels_a = labels fields in
          let labels_b = labels fields' in
          let compute ((field, str) as hd, tl) ((field', str') as hd', tl') =
            let field = field.Field.label in
            let field' = field'.Field.label in
            if String.equal field field'
            then begin
              aux (field::path) str str';
              tl, tl'
            end else begin
              match
                String.Hash_set.mem labels_b field,
                String.Hash_set.mem labels_a field' with
                | false, false
                | true, true -> update hd hd'; tl, tl'
                | true, false -> add hd'; (hd::tl), tl'
                | false, true -> remove hd; tl, (hd'::tl')
            end
          in
          aux_farray ~add ~remove ~compute fields fields'
        end

      | Variant ({Variant_infos.kind} as infos, cases), Variant (infos', cases') ->
        if not (Variant_infos.equal infos infos')
        then enqueue path (Atom.Update (a, b)) else begin
          let is_polymorphic = Variant.Kind.is_polymorphic kind in
          let cases = if is_polymorphic then sort_variant_cases cases else cases in
          let cases' = if is_polymorphic then sort_variant_cases cases' else cases' in
          let add compatible (a,b) = enqueue path (Atom.Add_variant (compatible, a, b)) in
          let remove hd = enqueue path (Atom.Remove_variant hd) in
          let update a b = enqueue path (Atom.Update_variant (a, b)) in
          let labels cases =
            let set = String.Hash_set.create () in
            let iter (variant, _) = String.Hash_set.add set variant.Variant.label in
            Farray.iter ~f:iter cases;
            set
          in
          let labels_a = labels cases in
          let labels_b = labels cases' in
          let compute ((variant, args) as hd, tl) ((variant', args') as hd', tl') =
            let label = variant.Variant.label in
            let label' = variant'.Variant.label in
            let arity = Farray.length args in
            let arity' = Farray.length args' in
            match String.equal label label', arity = arity' with
            | true, true -> begin
              Farray.iter2_exn args args'
                ~f:(let path = label::path in fun str str' -> aux path str str');
              tl, tl'
            end
            | true, false -> begin
              update hd hd';
              tl, tl'
            end
            | false, (true | false) -> begin
              match
                String.Hash_set.mem labels_b label,
                String.Hash_set.mem labels_a label' with
              | false, false
              | true, true -> update hd hd'; tl, tl'
              | true, false ->
                let compatible =
                  if is_polymorphic || List.is_empty tl'
                  then `Backward_compatible
                  else `Break
                in
                add compatible hd'; (hd::tl), tl'
              | false, true -> remove hd; tl, (hd'::tl')
            end
          in
          aux_farray ~add:(add `Backward_compatible) ~remove ~compute cases cases'
        end

      | Int, _
      | Int32, _
      | Int64, _
      | Char, _
      | Float, _
      | String, _
      | Bool, _
      | Unit, _
      | Option _, _
      | List _, _
      | Array _, _
      | Lazy _, _
      | Ref _, _
      | Tuple _, _
      | Record _, _
      | Variant _, _
        -> enqueue path (Atom.Update (a, b))
    in
    aux [] a b;
    let diffs = Queue.fold (fun acc x -> x::acc) [] diffs in
    let f (path, diff) = List.rev path, diff in
    List.rev_map ~f diffs

  let is_bin_prot_subtype ~subtype:a ~supertype:b =
    let diffs = compute a b in
    let for_all = function
      | _, Atom.Add_variant (compatible, _, _) -> compatible = `Backward_compatible
      | _ -> false
    in
    List.for_all ~f:for_all diffs

  let incompatible_changes t =
    let filter (_, atom) =
      let open Atom in
      match atom with
      | Add_variant (compatible, _, _) -> compatible = `Break
      | Update _
      | Add_field _
      | Remove_field _
      | Update_field _
      | Remove_variant _
      | Update_variant _
        -> true
    in
    List.rev_filter ~f:filter t
end

module To_typerep = struct
  exception Unbound_name of t * Name.t with sexp
  exception Unsupported_tuple of t with sexp

  let to_typerep : t -> Typerepable.t = fun type_struct ->
    let table = Name.Table.create () in
    let open Typerepable in
    let rec aux = function
      | Int -> Rep typerep_of_int
      | Int32 -> Rep typerep_of_int32
      | Int64 -> Rep typerep_of_int64
      | Char -> Rep typerep_of_char
      | Float -> Rep typerep_of_float
      | String -> Rep typerep_of_string
      | Bool -> Rep typerep_of_bool
      | Unit -> Rep typerep_of_unit
      | Option t ->
        let Rep rep = aux t in
        Rep (typerep_of_option rep)
      | List t ->
        let Rep rep = aux t in
        Rep (typerep_of_list rep)
      | Array t ->
        let Rep rep = aux t in
        Rep (typerep_of_array rep)
      | Lazy t ->
        let Rep rep = aux t in
        Rep (typerep_of_lazy_t rep)
      | Ref t ->
        let Rep rep = aux t in
        Rep (typerep_of_ref rep)
      | (Tuple args) as type_struct -> begin
        match Farray.to_array ~f:(fun _ x -> x) args with
        | [| a ; b |] ->
          let Rep a = aux a in
          let Rep b = aux b in
          Rep (typerep_of_tuple2 a b)
        | [| a ; b ; c |] ->
          let Rep a = aux a in
          let Rep b = aux b in
          let Rep c = aux c in
          Rep (typerep_of_tuple3 a b c)
        | [| a ; b ; c ; d |] ->
          let Rep a = aux a in
          let Rep b = aux b in
          let Rep c = aux c in
          let Rep d = aux d in
          Rep (typerep_of_tuple4 a b c d)
        | [| a ; b ; c ; d ; e |] ->
          let Rep a = aux a in
          let Rep b = aux b in
          let Rep c = aux c in
          let Rep d = aux d in
          let Rep e = aux e in
          Rep (typerep_of_tuple5 a b c d e)
        | _ -> raise (Unsupported_tuple type_struct)
      end
      | Record (infos, fields) ->
        let len = Farray.length fields in
        let typed_fields = Farray.to_array fields ~f:(fun index (field, str) ->
          if index <> field.Field.index then assert false;
          let label = field.Field.label in
          let Rep typerep_of_field = aux str in
          let get obj =
            let cond = Obj.is_block obj && Obj.size obj = len in
            if not cond then assert false;
            (* [Obj.field] works on float array *)
            Obj.obj (Obj.field obj index)
          in
          let field = {
            Typerep.Field_internal.
            label;
            rep = typerep_of_field;
            index;
            tyid = Type_name.create ();
            get;
          } in
          Typerep.Record_internal.Field (Typerep.Field.internal_use_only field)
        )
        in
        let module Typename_of_t = Type_named.Make0(struct
          type t = Obj.t
          let name = "dynamic record"
        end)
        in
        let typename = Typerep.Named.typename_of_t Typename_of_t.named in
        let has_double_array_tag = infos.Record_infos.has_double_array_tag in
        let create { Typerep.Record_internal.get } =
          let t =
            let tag =
              if has_double_array_tag
              then Obj.double_array_tag
              else 0
            in
            Obj.new_block tag len
          in
          let iter = function
            | Typerep.Record_internal.Field field ->
              let index = Typerep.Field.index field in
              let value = get field in
              Obj.set_field t index (Obj.repr value)
          in
          Array.iter ~f:iter typed_fields;
          t
        in
        let record = Typerep.Record.internal_use_only {
          Typerep.Record_internal.
          typename;
          fields = typed_fields;
          has_double_array_tag;
          create;
        } in
        let typerep_of_t =
          Typerep.Named ((Typename_of_t.named,
                           (Some (lazy (Typerep.Record record)))))
        in
        Rep typerep_of_t
      | Variant ({Variant_infos.kind}, tags_str) ->
        let polymorphic = Variant.Kind.is_polymorphic kind in
        let typed_tags = Farray.to_array tags_str ~f:(fun index (variant, args) ->
          let type_struct = type_struct_of_variant_args args in
          let Rep typerep_of_tag = aux type_struct in
          let index = (if index <> variant.Variant.index then assert false); index in
          let ocaml_repr = variant.Variant.ocaml_repr in
          let arity = Farray.length args in
          let label = variant.Variant.label in
          let create_polymorphic =
            if arity = 0
            then Typerep.Tag_internal.Const (Obj.repr ocaml_repr)
            else Typerep.Tag_internal.Args (fun value ->
              let block = Obj.new_block 0 2 in
              Obj.set_field block 0 (Obj.repr ocaml_repr);
              Obj.set_field block 1 (Obj.repr value);
              block
            )
          in
          let create_usual =
            match arity with
            | 0 -> Typerep.Tag_internal.Const (Obj.repr ocaml_repr)
            | 1 -> Typerep.Tag_internal.Args (fun value ->
              let block = Obj.new_block ocaml_repr 1 in
              Obj.set_field block 0 (Obj.repr value);
              block)
            | n -> Typerep.Tag_internal.Args (fun value ->
              let args = Obj.repr value in
              if not (Obj.size args = n) then assert false;
              let block = Obj.dup args in
              Obj.set_tag block ocaml_repr;
              block)
          in
          let create = if polymorphic then create_polymorphic else create_usual in
          let tyid =
            (fun (type exist) (typerep_of_tag:exist Typerep.t) ->
              if arity = 0
              then
                let Type_equal.T =
                  Typerep.same_witness_exn typerep_of_tuple0 typerep_of_tag
                in
                (typename_of_tuple0 : exist Type_name.t)
              else
                (Type_name.create () : exist Type_name.t)
            ) typerep_of_tag
          in
          let tag = {
            Typerep.Tag_internal.
            label;
            rep = typerep_of_tag;
            arity;
            index;
            ocaml_repr;
            tyid;
            create;
          } in
          Typerep.Variant_internal.Tag (Typerep.Tag.internal_use_only tag)
        ) in
        let module Typename_of_t = Type_named.Make0(struct
          type t = Obj.t
          let name = "dynamic variant"
        end) in
        let typename = Typerep.Named.typename_of_t Typename_of_t.named in
        let value_polymorphic =
          let map =
            let map exists =
              match exists with
              | Typerep.Variant_internal.Tag tag ->
                let ocaml_repr = Typerep.Tag.ocaml_repr tag in
                ocaml_repr, exists
            in
            Flat_map.Flat_int_map.of_array_map ~f:map typed_tags
          in
          (fun obj ->
            let repr = Typerep_obj.repr_of_poly_variant (Obj.obj obj) in
            let no_arg = Obj.is_int obj in
            match Flat_map.Flat_int_map.find map repr with
            | None -> assert false
            | Some (Typerep.Variant_internal.Tag tag) ->
              let arity = Typerep.Tag.arity tag in
              let value =
                match no_arg with
                | true ->
                  if arity = 0
                  then Obj.repr value_tuple0
                  else assert false
                | false ->
                  let size = Obj.size obj in
                  if size <> 2 then assert false;
                  let value = Obj.field obj 1 in
                  if arity = 1
                  then value
                  else begin
                    if Obj.is_int value then assert false;
                    if Obj.size value <> arity then assert false;
                    value
                  end
              in
              Typerep.Variant_internal.Value (tag, Obj.obj value)
          )
        in
        let value_usual =
          let bound = Array.length typed_tags in
          let collect pred_args =
            let rec aux acc index =
              if index >= bound then Farray.of_list (List.rev acc)
              else
                match typed_tags.(index) with
                | (Typerep.Variant_internal.Tag tag) as exists ->
                  let arity = Typerep.Tag.arity tag in
                  let acc =
                    if pred_args arity then exists::acc else acc
                  in
                  aux acc (succ index)
            in
            aux [] 0
          in
          let without_args = collect (fun i -> i = 0) in
          let with_args = collect (fun i -> i > 0) in
          let find_tag ~no_arg idx =
            let table = if no_arg then without_args else with_args in
            if idx < 0 || idx >= Farray.length table then assert false;
            Farray.get table idx
          in
          (fun obj ->
            match Obj.is_int obj with
            | true -> begin
              match find_tag ~no_arg:true (Obj.obj obj) with
              | Typerep.Variant_internal.Tag tag ->
                let Type_equal.T =
                  Type_name.same_witness_exn
                    (Typerep.Tag.tyid tag)
                    typename_of_tuple0
                in
                let arity = Typerep.Tag.arity tag in
                if arity <> 0 then assert false;
                Typerep.Variant_internal.Value (tag, value_tuple0)
            end
            | false ->
              let idx = Obj.tag obj in
              match find_tag ~no_arg:false idx with
              | Typerep.Variant_internal.Tag tag ->
                let arity = Typerep.Tag.arity tag in
                if arity <> Obj.size obj then assert false;
                let value =
                  if arity = 1
                  then Obj.field obj 0
                  else
                    let block = Obj.dup obj in
                    Obj.set_tag block 0; (* tuple *)
                    block
                in
                Typerep.Variant_internal.Value (tag, Obj.obj value)
          )
        in
        let value = if polymorphic then value_polymorphic else value_usual in
        let variant = Typerep.Variant.internal_use_only {
          Typerep.Variant_internal.
          typename;
          tags = typed_tags;
          polymorphic;
          value;
        } in
        let typerep_of_t =
          Typerep.Named ((Typename_of_t.named,
                           (Some (lazy (Typerep.Variant variant)))))
        in
        Rep typerep_of_t
      | Named (name, content) ->
        match Name.Table.find table name with
        | Some content -> content
        | None ->
          let module T = struct
            (*
              let [t] be the type represented by this named type_struct
              this type will be equal to the existential type returned by the call to
              aux performed on the content.
            *)
            type t
            let name = string_of_int name
          end in
          let module Named = Type_named.Make0(T) in
          let content =
            match content with
            | Some content -> content
            | None ->
              raise (Unbound_name (type_struct, name))
          in
          let release_content_ref = ref (`aux_content content) in
          let typerep_of_t = Typerep.Named (Named.named, Some (lazy (
            match !release_content_ref with
            | `aux_content content ->
              let Rep typerep_of_content = aux content in
              let rep = (fun (type content) (rep:content Typerep.t) ->
                (Obj.magic (rep : content Typerep.t) : T.t Typerep.t)
              ) typerep_of_content
              in
              release_content_ref := `typerep rep;
              rep
            | `typerep rep -> rep
          ))) in
          let data = Rep typerep_of_t in
          Name.Table.set table ~key:name ~data;
          data
    in
    aux type_struct
end
let to_typerep = To_typerep.to_typerep

module Versioned = struct
  module Version = struct
    type t = [
    | `v0
    | `v1
    | `v2
    | `v3
    | `v4
    ] with bin_io, sexp, typerep
    let v1 = `v1
    let v2 = `v2
    let v3 = `v3
    let v4 = `v4
  end
  exception Not_downgradable of Sexp.t with sexp
  module type V_sig = sig
    type t
    with sexp, bin_io, typerep
    val serialize : type_struct -> t
    val unserialize : t -> type_struct
  end
  module V0 : V_sig = struct
    type t =
    | Unit
    with sexp, bin_io, typerep

    let serialize = function
      | T.Unit -> Unit
      | str -> raise (Not_downgradable (T.sexp_of_t str))

    let unserialize = function
      | Unit -> T.Unit
  end
  module V1 : V_sig = struct
    type t =
    | Int
    | Int32
    | Int64
    | Char
    | Float
    | String
    | Bool
    | Unit
    | Option of t
    | List of t
    | Array of t
    | Lazy of t
    | Ref of t
    | Record of (Field.V1.t * t) Farray.t
    | Tuple of t Farray.t
    | Variant of Variant.Kind.t * (Variant.V1.t * t Farray.t) Farray.t
    with sexp, bin_io, typerep

    let rec serialize = function
      | T.Int      -> Int
      | T.Int32    -> Int32
      | T.Int64    -> Int64
      | T.Char     -> Char
      | T.Float    -> Float
      | T.String   -> String
      | T.Bool     -> Bool
      | T.Unit     -> Unit
      | T.Option t -> Option (serialize t)
      | T.List t   -> List (serialize t)
      | T.Array t  -> Array (serialize t)
      | T.Lazy t   -> Lazy (serialize t)
      | T.Ref t    -> Ref (serialize t)
      | T.Record (infos, fields) as str ->
        if infos.Record_infos.has_double_array_tag
        then raise (Not_downgradable (T.sexp_of_t str));
        let map (field, t) =
          let field = Field.to_v1 field in
          field, serialize t in
        let fields = Farray.map ~f:map fields in
        Record fields
      | T.Tuple args -> Tuple (Farray.map ~f:serialize args)
      | T.Variant (info, tags) ->
        let kind = info.Variant_infos.kind in
        let map (variant, t) =
          let variant_v1 = Variant.to_v1 kind variant in
          variant_v1, Farray.map ~f:serialize t
        in
        Variant (kind, Farray.map ~f:map tags)
      | (T.Named _) as str -> raise (Not_downgradable (T.sexp_of_t str))

    let rec unserialize = function
      | Int      -> T.Int
      | Int32    -> T.Int32
      | Int64    -> T.Int64
      | Char     -> T.Char
      | Float    -> T.Float
      | String   -> T.String
      | Bool     -> T.Bool
      | Unit     -> T.Unit
      | Option t -> T.Option (unserialize t)
      | List t   -> T.List (unserialize t)
      | Array t  -> T.Array (unserialize t)
      | Lazy t   -> T.Lazy (unserialize t)
      | Ref t    -> T.Ref (unserialize t)
      | Record fields ->
        let infos = { Record_infos.
          (* this is wrong is some cases, if so the exec should upgrade to >= v3 *)
          has_double_array_tag = false;
        } in
        let mapi index (field, t) =
          let field = Field.of_v1 index field in
          field, unserialize t
        in
        let fields = Farray.mapi ~f:mapi fields in
        T.Record (infos, fields)
      | Tuple args -> T.Tuple (Farray.map ~f:unserialize args)
      | Variant (kind, tags) ->
        let infos = { Variant_infos.kind } in
        let mapi index (variant_v1, t) =
          let variant = Variant.of_v1 kind index t tags variant_v1 in
          variant, Farray.map ~f:unserialize t
        in
        T.Variant (infos, Farray.mapi ~f:mapi tags)
  end
  (* Adding Named *)
  module V2 : V_sig = struct
    type t =
    | Int
    | Int32
    | Int64
    | Char
    | Float
    | String
    | Bool
    | Unit
    | Option of t
    | List of t
    | Array of t
    | Lazy of t
    | Ref of t
    | Record of (Field.V1.t * t) Farray.t
    | Tuple of t Farray.t
    | Variant of Variant.Kind.t * (Variant.V1.t * t Farray.t) Farray.t
    | Named of Name.t * t option
    with sexp, bin_io, typerep

    let rec serialize = function
      | T.Int      -> Int
      | T.Int32    -> Int32
      | T.Int64    -> Int64
      | T.Char     -> Char
      | T.Float    -> Float
      | T.String   -> String
      | T.Bool     -> Bool
      | T.Unit     -> Unit
      | T.Option t -> Option (serialize t)
      | T.List t   -> List (serialize t)
      | T.Array t  -> Array (serialize t)
      | T.Lazy t   -> Lazy (serialize t)
      | T.Ref t    -> Ref (serialize t)
      | T.Record (infos, fields) as str ->
        if infos.Record_infos.has_double_array_tag
        then raise (Not_downgradable (T.sexp_of_t str));
        let map (field, t) =
          let field = Field.to_v1 field in
          field, serialize t
        in
        let fields = Farray.map ~f:map fields in
        Record fields
      | T.Tuple args -> Tuple (Farray.map ~f:serialize args)
      | T.Variant (info, tags) ->
        let kind = info.Variant_infos.kind in
        let map (variant, t) =
          let variant_v1 = Variant.to_v1 kind variant in
          variant_v1, Farray.map ~f:serialize t
        in
        Variant (kind, Farray.map ~f:map tags)
      | T.Named (name, content) ->
        let content = Option.map ~f:serialize content in
        Named (name, content)

    let rec unserialize = function
      | Int      -> T.Int
      | Int32    -> T.Int32
      | Int64    -> T.Int64
      | Char     -> T.Char
      | Float    -> T.Float
      | String   -> T.String
      | Bool     -> T.Bool
      | Unit     -> T.Unit
      | Option t -> T.Option (unserialize t)
      | List t   -> T.List (unserialize t)
      | Array t  -> T.Array (unserialize t)
      | Lazy t   -> T.Lazy (unserialize t)
      | Ref t    -> T.Ref (unserialize t)
      | Record fields ->
        let infos = { Record_infos.
          (* this is wrong is some cases, if so the exec should upgrade to >= v3 *)
          has_double_array_tag = false;
        } in
        let mapi index (field, t) =
          let field = Field.of_v1 index field in
          field, unserialize t
        in
        let fields = Farray.mapi ~f:mapi fields in
        T.Record (infos, fields)
      | Tuple args -> T.Tuple (Farray.map ~f:unserialize args)
      | Variant (kind, tags) ->
        let infos = { Variant_infos.kind } in
        let mapi index (variant_v1, t) =
          let variant = Variant.of_v1 kind index t tags variant_v1 in
          variant, Farray.map ~f:unserialize t
        in
        T.Variant (infos, Farray.mapi ~f:mapi tags)
      | Named (name, content) ->
        let content = Option.map ~f:unserialize content in
        T.Named (name, content)
  end
  (* Adding meta-info to the records and variants *)
  module V3 : V_sig = struct
    type t =
    | Int
    | Int32
    | Int64
    | Char
    | Float
    | String
    | Bool
    | Unit
    | Option of t
    | List of t
    | Array of t
    | Lazy of t
    | Ref of t
    | Record of Record_infos.V1.t * (Field.V1.t * t) Farray.t
    | Tuple of t Farray.t
    | Variant of Variant_infos.V1.t * (Variant.V1.t * t Farray.t) Farray.t
    | Named of Name.t * t option
    with sexp, bin_io, typerep

    let rec serialize = function
      | T.Int      -> Int
      | T.Int32    -> Int32
      | T.Int64    -> Int64
      | T.Char     -> Char
      | T.Float    -> Float
      | T.String   -> String
      | T.Bool     -> Bool
      | T.Unit     -> Unit
      | T.Option t -> Option (serialize t)
      | T.List t   -> List (serialize t)
      | T.Array t  -> Array (serialize t)
      | T.Lazy t   -> Lazy (serialize t)
      | T.Ref t    -> Ref (serialize t)
      | T.Record (infos, fields) ->
        let map (field, t) =
          let field = Field.to_v1 field in
          field, serialize t
        in
        let fields = Farray.map ~f:map fields in
        Record (infos, fields)
      | T.Tuple args -> Tuple (Farray.map ~f:serialize args)
      | T.Variant (infos, tags) ->
        let kind = infos.Variant_infos.kind in
        let map (variant, t) =
          let variant_v1 = Variant.to_v1 kind variant in
          variant_v1, Farray.map ~f:serialize t
        in
        Variant (infos, Farray.map ~f:map tags)
      | T.Named (name, content) ->
        let content = Option.map ~f:serialize content in
        Named (name, content)

    let rec unserialize = function
      | Int      -> T.Int
      | Int32    -> T.Int32
      | Int64    -> T.Int64
      | Char     -> T.Char
      | Float    -> T.Float
      | String   -> T.String
      | Bool     -> T.Bool
      | Unit     -> T.Unit
      | Option t -> T.Option (unserialize t)
      | List t   -> T.List (unserialize t)
      | Array t  -> T.Array (unserialize t)
      | Lazy t   -> T.Lazy (unserialize t)
      | Ref t    -> T.Ref (unserialize t)
      | Record (infos, fields) ->
        let mapi index (field, t) =
          let field = Field.of_v1 index field in
          field, unserialize t
        in
        let fields = Farray.mapi ~f:mapi fields in
        T.Record (infos, fields)
      | Tuple args -> T.Tuple (Farray.map ~f:unserialize args)
      | Variant (infos, tags) ->
        let kind = infos.Variant_infos.kind in
        let mapi index (variant_v1, t) =
          let variant = Variant.of_v1 kind index t tags variant_v1 in
          variant, Farray.map ~f:unserialize t
        in
        T.Variant (infos, Farray.mapi ~f:mapi tags)
      | Named (name, content) ->
        let content = Option.map ~f:unserialize content in
        T.Named (name, content)
  end

  (* Switching to Variant.V2 and Field.V2.t *)
  module V4 : V_sig with type t = T.t = struct
    type t = T.t =
    | Int
    | Int32
    | Int64
    | Char
    | Float
    | String
    | Bool
    | Unit
    | Option of t
    | List of t
    | Array of t
    | Lazy of t
    | Ref of t
    | Tuple of t Farray.t
    | Record of Record_infos.V1.t * (Field.V2.t * t) Farray.t
    | Variant of Variant_infos.V1.t * (Variant.V2.t * t Farray.t) Farray.t
    | Named of Name.t * t option
    with sexp, bin_io
    let typerep_of_t = T.typerep_of_t
    let typename_of_t = T.typename_of_t
    (* *)

    let serialize t = t
    let unserialize t = t
  end

  type t = [
  | `V0 of V0.t
  | `V1 of V1.t
  | `V2 of V2.t
  | `V3 of V3.t
  | `V4 of V4.t
  ] with bin_io, sexp, typerep

  let aux_unserialize = function
    | `V0 v0 -> V0.unserialize v0
    | `V1 v1 -> V1.unserialize v1
    | `V2 v2 -> V2.unserialize v2
    | `V3 v3 -> V3.unserialize v3
    | `V4 v4 -> V4.unserialize v4

  let serialize ~version v4 =
    match version with
    | `v0 -> `V0 (V0.serialize v4)
    | `v1 -> `V1 (V1.serialize v4)
    | `v2 -> `V2 (V2.serialize v4)
    | `v3 -> `V3 (V3.serialize v4)
    | `v4 -> `V4 (V4.serialize v4)

  let version = function
    | `V0 _ -> `v0
    | `V1 _ -> `v1
    | `V2 _ -> `v2
    | `V3 _ -> `v3
    | `V4 _ -> `v4

  let change_version ~version:requested t =
    if version t = requested
    then t
    else serialize ~version:requested (aux_unserialize t)

  module Diff = struct
    let compute a b = Diff.compute (aux_unserialize a) (aux_unserialize b)
    let is_bin_prot_subtype ~subtype ~supertype =
      let subtype = aux_unserialize subtype in
      let supertype = aux_unserialize supertype in
      Diff.is_bin_prot_subtype ~subtype ~supertype
  end

  let is_polymorphic_variant t = is_polymorphic_variant (aux_unserialize t)

  let least_upper_bound_exn t1 t2 =
    let supremum = least_upper_bound_exn (aux_unserialize t1) (aux_unserialize t2) in
    serialize ~version:(version t1) supremum

  let unserialize = aux_unserialize

  let to_typerep t =
    To_typerep.to_typerep (aux_unserialize t)
end

type 'a typed_t = t

let recreate_dynamically_typerep_for_test (type a) (rep:a Typerep.t) =
  let Typerepable.Rep typerep = to_typerep (of_typerep rep) in
  (fun (type b) (typerep:b Typerep.t) ->
    (* this Obj.magic is used to be able to add more testing, basically we use the ocaml
       runtime to deal with value create with the Obj module during the execution of
       [Type_struct.to_typerep] and allow ocaml to treat them as value of the right type
       as they had been generated by some functions whose code had been known at compile
       time *)
    (Obj.magic (typerep:b Typerep.t) : a Typerep.t)
  ) typerep

