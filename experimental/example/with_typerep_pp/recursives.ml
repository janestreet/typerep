open Core.Std let _ = _squelch_unused_module_warning_
open Typerep_experimental.Std

module M1 = struct

  type 'a tree = | Leaf of 'a | Tree of 'a * 'a tree * 'a tree

  module Typename_of_tree = Make_typename.Make1(struct
    type 'a t = 'a tree
    let name = "Recursives.M1.t"
  end)

  let rec typerep_of_tree : 'a. 'a Typerep.t -> 'a tree Typerep.t =
    fun (type a) (of_a:a Typerep.t) ->
    let name_of_tree = Typename_of_tree.named of_a in
    Typerep.Named (name_of_tree, Some (lazy (
      let tag0 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "Leaf";
        rep = of_a;
        arity = 1;
        index = 0;
        ocaml_repr = 0;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun x -> Leaf x);
      } in
      let tag1 = Typerep.Tag.internal_use_only {
        Typerep.Tag_internal.
        label = "Tree";
        rep = (
          let v1 = of_a
          and v2 = typerep_of_tree of_a
          and v3 = typerep_of_tree of_a
          in typerep_of_tuple3 v1 v2 v3
        );
        arity = 3;
        index = 1;
        ocaml_repr = 1;
        tyid = Typename.create ();
        create = Typerep.Tag_internal.Args (fun (v1, v2, v3) -> Tree (v1, v2, v3));
      } in
      let typename = Typerep.Named.typename_of_t name_of_tree in
      let polymorphic = false in
      let tags = [|
        Typerep.Variant_internal.Tag tag0;
        Typerep.Variant_internal.Tag tag1;
      |] in
      let value = function
        | Leaf x -> Typerep.Variant_internal.Value (tag0, x)
        | Tree (v1, v2, v3) -> Typerep.Variant_internal.Value (tag1, (v1, v2, v3))
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
