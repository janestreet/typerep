open Core.Std let _ = _squelch_unused_module_warning_
open Typerep_experimental.Std

module Transaction_type = struct
  module V1 = struct
    type t =
    | Trade
    | Order

    module Typename_of_t = Make_typename.Make0(struct
      type non_rec = t
      type t = non_rec
      let name = "Combination.Transaction_type.V1.t"
    end)

    let typerep_of_t =
      let name_of_t = Typename_of_t.named in
      Typerep.Named (name_of_t, Some (lazy (
        let tag0 = Typerep.Tag.internal_use_only {
          Typerep.Tag_internal.
          label = "Trade";
          rep = typerep_of_tuple0;
          arity = 0;
          index = 0;
          ocaml_repr = 0;
          tyid = Typename.create ();
          create = Typerep.Tag_internal.Const Trade;
        } in
        let tag1 = Typerep.Tag.internal_use_only {
          Typerep.Tag_internal.
          label = "Order";
          rep = typerep_of_tuple0;
          arity = 0;
          index = 1;
          ocaml_repr = 1;
          tyid = Typename.create ();
          create = Typerep.Tag_internal.Const Order;
        } in
        let typename = Typerep.Named.typename_of_t name_of_t in
        let polymorphic = false in
        let tags = [|
          Typerep.Variant_internal.Tag tag0;
          Typerep.Variant_internal.Tag tag1;
        |] in
        let value = function
          | Trade -> Typerep.Variant_internal.Value (tag0, value_tuple0)
          | Order -> Typerep.Variant_internal.Value (tag1, value_tuple0)
        in
        Typerep.Variant (Typerep.Variant.internal_use_only {
          Typerep.Variant_internal.
          typename;
          tags;
          polymorphic;
          value;
        })
      )))
  end

  module V2 = struct
    type t =
    | Trade
    | Order
    | Journal of string * string

    module Typename_of_t = Make_typename.Make0(struct
      type non_rec = t
      type t = non_rec
      let name = "Combination.Transaction_type.V2.t"
    end)

    let typerep_of_t =
      let name_of_t = Typename_of_t.named in
      Typerep.Named (name_of_t, Some (lazy (
        let tag0 = Typerep.Tag.internal_use_only {
          Typerep.Tag_internal.
          label = "Trade";
          rep = typerep_of_tuple0;
          arity = 0;
          index = 0;
          ocaml_repr = 0;
          tyid = Typename.create ();
          create = Typerep.Tag_internal.Const Trade;
        } in
        let tag1 = Typerep.Tag.internal_use_only {
          Typerep.Tag_internal.
          label = "Order";
          rep = typerep_of_tuple0;
          arity = 0;
          index = 1;
          ocaml_repr = 1;
          tyid = Typename.create ();
          create = Typerep.Tag_internal.Const Order;
        } in
        let tag2 = Typerep.Tag.internal_use_only {
          Typerep.Tag_internal.
          label = "Journal";
          rep = typerep_of_tuple2 typerep_of_string typerep_of_string;
          arity = 2;
          index = 1;
          ocaml_repr = 0;
          tyid = Typename.create ();
          create = Typerep.Tag_internal.Args (fun (v1, v2) -> Journal (v1, v2));
        } in
        let typename = Typerep.Named.typename_of_t name_of_t in
        let polymorphic = false in
        let tags = [|
          Typerep.Variant_internal.Tag tag0;
          Typerep.Variant_internal.Tag tag1;
          Typerep.Variant_internal.Tag tag2;
        |] in
        let value = function
          | Trade -> Typerep.Variant_internal.Value (tag0, value_tuple0)
          | Order -> Typerep.Variant_internal.Value (tag1, value_tuple0)
          | Journal (v1, v2) -> Typerep.Variant_internal.Value (tag2, (v1, v2))
        in
        Typerep.Variant (Typerep.Variant.internal_use_only {
          Typerep.Variant_internal.
          typename;
          tags;
          polymorphic;
          value;
        })
      )))
  end
end

module V1 = struct
  type t = {
    transaction_type : Transaction_type.V1.t;
    username : string;
  }

  module Typename_of_t = Make_typename.Make0(struct
    type non_rec = t
    type t = non_rec
    let name = "Combination.V1.t"
  end)

  let typerep_of_t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some(lazy(
      let field0 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "transaction_type";
        index = 0;
        rep = Transaction_type.V1.typerep_of_t;
        tyid = Typename.create ();
        get = (fun t -> t.transaction_type);
      } in
      let field1 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "username";
        index = 1;
        rep = typerep_of_string;
        tyid = Typename.create ();
        get = (fun t -> t.username);
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let has_double_array_tag =
        Typerep_obj.has_double_array_tag {
          transaction_type = Typerep_obj.double_array_value;
          username = Typerep_obj.double_array_value;
        }
      in
      let fields = [|
        Typerep.Record_internal.Field field0;
        Typerep.Record_internal.Field field1;
      |] in
      let create { Typerep.Record_internal.get } =
        let transaction_type = get field0 in
        let username = get field1 in
        { transaction_type ; username }
      in
      Typerep.Record (Typerep.Record.internal_use_only {
        Typerep.Record_internal.
        typename;
        has_double_array_tag;
        fields;
        create;
      })
    )))
end

module V2 = struct
  type t = {
    transaction_type : Transaction_type.V2.t;
    username : string;
    tags : (string * string) list;
  }

  module Typename_of_t = Make_typename.Make0(struct
    type non_rec = t
    type t = non_rec
    let name = "Combination.V2.t"
  end)

  let typerep_of_t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some(lazy(
      let field0 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "transaction_type";
        index = 0;
        rep = Transaction_type.V2.typerep_of_t;
        tyid = Typename.create ();
        get = (fun t -> t.transaction_type);
      } in
      let field1 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "username";
        index = 1;
        rep = typerep_of_string;
        tyid = Typename.create ();
        get = (fun t -> t.username);
      } in
      let field2 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "tags";
        index = 2;
        rep = typerep_of_list (typerep_of_tuple2 typerep_of_string typerep_of_string);
        tyid = Typename.create ();
        get = (fun t -> t.tags);
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let has_double_array_tag =
        Typerep_obj.has_double_array_tag {
          transaction_type = Typerep_obj.double_array_value;
          username = Typerep_obj.double_array_value;
          tags = Typerep_obj.double_array_value;
        }
      in
      let fields = [|
        Typerep.Record_internal.Field field0;
        Typerep.Record_internal.Field field1;
        Typerep.Record_internal.Field field2;
      |] in
      let create { Typerep.Record_internal.get } =
        let transaction_type = get field0 in
        let username = get field1 in
        let tags = get field2 in
        { transaction_type ; username ; tags }
      in
      Typerep.Record (Typerep.Record.internal_use_only {
        Typerep.Record_internal.
        typename;
        has_double_array_tag;
        fields;
        create;
      })
    )))
end

type t =
| V1 of V1.t
| V2 of V2.t

module Typename_of_t = Make_typename.Make0(struct
  type non_rec = t
  type t = non_rec
  let name = "Combination.t"
end)

let typerep_of_t : t Typerep.t =
  let name_of_t = Typename_of_t.named in
  Typerep.Named (name_of_t, Some (lazy (
    let tag0 = Typerep.Tag.internal_use_only {
      Typerep.Tag_internal.
      label = "V1";
      rep = V1.typerep_of_t;
      arity = 1;
      index = 0;
      ocaml_repr = 0;
      tyid = Typename.create ();
      create = Typerep.Tag_internal.Args (fun x -> V1 x);
    } in
    let tag1 = Typerep.Tag.internal_use_only {
      Typerep.Tag_internal.
      label = "V2";
      rep = V2.typerep_of_t;
      arity = 1;
      index = 1;
      ocaml_repr = 1;
      tyid = Typename.create ();
      create = Typerep.Tag_internal.Args (fun x -> V2 x)
    } in
    let typename = Typerep.Named.typename_of_t name_of_t in
    let polymorphic = false in
    let tags = [|
      Typerep.Variant_internal.Tag tag0;
      Typerep.Variant_internal.Tag tag1;
    |] in
    let value = function
      | V1 x -> Typerep.Variant_internal.Value (tag0, x)
      | V2 x -> Typerep.Variant_internal.Value (tag1, x)
    in
    Typerep.Variant (Typerep.Variant.internal_use_only {
      Typerep.Variant_internal.
      typename;
      tags;
      polymorphic;
      value;
    })
  )))
