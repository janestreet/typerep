open Core.Std let _ = _squelch_unused_module_warning_
open Typerep_experimental.Std

module M1 = struct

  type t =
  | A of int
  | B of float

  module Typename_of_t = Make_typename.Make0(struct
    type non_rec = t
    type t = non_rec
    let name = "Variants.M1.t"
  end)

  let typerep_of_t : t Typerep.t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some (lazy (
      let tag0 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "A";
        rep = typerep_of_int;
        arity = 1;
        index = 0;
        ocaml_repr = 0;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> A x);
      } in
      let tag1 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "B";
        rep = typerep_of_float;
        arity = 1;
        index = 1;
        ocaml_repr = 1;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> B x)
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let polymorphic = false in
      let tags = [|
        Typerep.Variant_internal.Tag tag0;
        Typerep.Variant_internal.Tag tag1;
      |] in
      let value = function
        | A x -> Typerep.Variant_internal.Value (tag0, x)
        | B x -> Typerep.Variant_internal.Value (tag1, x)
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

module M2 = struct

  type t = [
  | `A of int
  | `B of float
  ]

  module Typename_of_t = Make_typename.Make0(struct
    type non_rec = t
    type t = non_rec
    let name = "Variants.M2.t"
  end)

  let typerep_of_t : t Typerep.t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some (lazy (
      let tag0 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "A";
        rep = typerep_of_int;
        arity = 1;
        index = 0;
        ocaml_repr = Typerep_obj.repr_of_poly_variant `A;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> `A x);
      } in
      let tag1 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "B";
        rep = typerep_of_float;
        arity = 1;
        index = 1;
        ocaml_repr = Typerep_obj.repr_of_poly_variant `B;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> `B x)
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let polymorphic = true in
      let tags = [|
        Typerep.Variant_internal.Tag tag0;
        Typerep.Variant_internal.Tag tag1;
      |] in
      let value = function
        | `A x -> Typerep.Variant_internal.Value (tag0, x)
        | `B x -> Typerep.Variant_internal.Value (tag1, x)
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

module M3 = struct

  type t =
  | M1 of M1.t
  | M2 of M2.t

  module Typename_of_t = Make_typename.Make0(struct
    type non_rec = t
    type t = non_rec
    let name = "Variants.M3.t"
  end)

  let typerep_of_t : t Typerep.t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some (lazy (
      let tag0 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "M1";
        rep = M1.typerep_of_t;
        arity = 1;
        index = 0;
        ocaml_repr = 0;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> M1 x);
      } in
      let tag1 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "M2";
        rep = M2.typerep_of_t;
        arity = 1;
        index = 1;
        ocaml_repr = 1;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> M2 x)
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let polymorphic = false in
      let tags = [|
        Typerep.Variant_internal.Tag tag0;
        Typerep.Variant_internal.Tag tag1;
      |] in
      let value = function
        | M1 x -> Typerep.Variant_internal.Value (tag0, x)
        | M2 x -> Typerep.Variant_internal.Value (tag1, x)
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

module P1 = struct

  type 'a t = [
  | `A of 'a
  | `B of float
  ]

  module Typename_of_t = Make_typename.Make1(struct
    type nonrec 'a t = 'a t
    let name = "Variants.P1.t"
  end)

  let typerep_of_t of_p1 =
    let name_of_t = Typename_of_t.named of_p1 in
    Typerep.Named (name_of_t, Some (lazy (
      let tag0 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "A";
        rep = of_p1;
        arity = 1;
        index = 0;
        ocaml_repr = Typerep_obj.repr_of_poly_variant `A;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> `A x);
      } in
      let tag1 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "B";
        rep = typerep_of_float;
        arity = 1;
        index = 1;
        ocaml_repr = Typerep_obj.repr_of_poly_variant `B;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> `B x)
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let polymorphic = true in
      let tags = [|
        Typerep.Variant_internal.Tag tag0;
        Typerep.Variant_internal.Tag tag1;
      |] in
      let value = function
        | `A x -> Typerep.Variant_internal.Value (tag0, x)
        | `B x -> Typerep.Variant_internal.Value (tag1, x)
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

module I1 = struct

  type t = A of [ `A ]

  module Typename_of_t = Make_typename.Make0 (struct
    type nonrec t = t
    let name = "test.ml.t"
  end)

  let typerep_of_t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some (lazy (
      let tag0 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "A";
        rep =
          (let tag0 =
             Typerep.Tag.internal_use_only {
               Typerep.Tag_internal.
               label = "A";
               rep = typerep_of_tuple0;
               arity = 0;
               index = 0;
               ocaml_repr = Typerep_obj.repr_of_poly_variant `A;
               tyid = typename_of_tuple0;
               create = Typerep.Tag_internal.Const `A;
             } in
           let typename = Typename.create () in
           let tags = [|
             Typerep.Variant_internal.Tag tag0
           |] in
           let polymorphic = true in
           let value = function
             | `A -> Typerep.Variant_internal.Value (tag0, value_tuple0)
           in
           Typerep.Variant (Typerep.Variant.internal_use_only {
             Typerep.Variant_internal.
             typename;
             tags;
             polymorphic;
             value;
           }));
        arity = 1;
        index = 0;
        ocaml_repr = 0;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun v0 -> A v0);
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let tags = [|
        Typerep.Variant_internal.Tag tag0
      |] in
      let polymorphic = false in
      let value = function
        | A v0 -> Typerep.Variant_internal.Value (tag0, v0)
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
