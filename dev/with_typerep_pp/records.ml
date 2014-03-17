open Typerep_experimental.Std

module M1 = struct

  type t = {
    a : int;
    b : float;
  }

  module Typename_of_t = Make_typename.Make0(struct
    type nonrec t = t
    let name = "Records.M1.t"
  end)

  let typerep_of_t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some (lazy (
      let field0 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "a";
        index = 0;
        rep = typerep_of_int;
        tyid = Typename.create ();
        get = (fun t -> t.a);
      } in
      let field1 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "b";
        index = 1;
        rep = typerep_of_float;
        tyid = Typename.create ();
        get = (fun t -> t.b);
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let has_double_array_tag =
        Typerep_obj.has_double_array_tag {
          a = Typerep_obj.double_array_value;
          b = Typerep_obj.double_array_value;
        }
      in
      let fields = [|
        Typerep.Record_internal.Field field0;
        Typerep.Record_internal.Field field1;
      |] in
      let create { Typerep.Record_internal.get } =
        let a = get field0 in
        let b = get field1 in
        { a ; b }
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

module M2 = struct

  type t = {
    a : int * string;
    b : float;
  }

  module Typename_of_t = Make_typename.Make0(struct
    type non_rec = t
    type t = non_rec
    let name = "Records.M2.t"
  end)

  let typerep_of_t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some (lazy (
      let field0 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "a";
        index = 0;
        rep = typerep_of_tuple2 typerep_of_int typerep_of_string;
        tyid = Typename.create ();
        get = (fun t -> t.a);
      } in
      let field1 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "b";
        index = 1;
        rep = typerep_of_float;
        tyid = Typename.create ();
        get = (fun t -> t.b);
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let has_double_array_tag =
        Typerep_obj.has_double_array_tag {
          a = Typerep_obj.double_array_value;
          b = Typerep_obj.double_array_value;
        }
      in
      let fields = [|
        Typerep.Record_internal.Field field0;
        Typerep.Record_internal.Field field1;
      |] in
      let create { Typerep.Record_internal.get } =
        let a = get field0 in
        let b = get field1 in
        { a ; b }
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

module M3 = struct
  type t = {
    m1 : M1.t;
    m2 : M2.t;
  }

  module Typename_of_t = Make_typename.Make0(struct
    type non_rec = t
    type t = non_rec
    let name = "Records.M3.t"
  end)

  let typerep_of_t =
    let name_of_t = Typename_of_t.named in
    Typerep.Named (name_of_t, Some (lazy (
      let field0 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "m1";
        index = 0;
        rep = M1.typerep_of_t;
        tyid = Typename.create ();
        get = (fun t -> t.m1);
      } in
      let field1 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "m2";
        index = 1;
        rep = M2.typerep_of_t;
        tyid = Typename.create ();
        get = (fun t -> t.m2);
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let has_double_array_tag =
        Typerep_obj.has_double_array_tag {
          m1 = Typerep_obj.double_array_value;
          m2 = Typerep_obj.double_array_value;
        }
      in
      let fields = [|
        Typerep.Record_internal.Field field0;
        Typerep.Record_internal.Field field1;
      |] in
      let create { Typerep.Record_internal.get } =
        let m1 = get field0 in
        let m2 = get field1 in
        { m1 ; m2 }
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

(* parametric *)

module P1 = struct

  type 'a t = {
    a : 'a;
    b : float;
  }

  module Typename_of_t = Make_typename.Make1(struct
    type 'a non_rec = 'a t
    type 'a t = 'a non_rec
    let name = "Records.P1.t"
  end)

  let typerep_of_t (type p1) (of_p1:p1 Typerep.t) =
    let name_of_t = Typename_of_t.named of_p1 in
    Typerep.Named (name_of_t, Some (lazy (
      let field0 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "a";
        index = 0;
        rep = of_p1;
        tyid = Typename.create ();
        get = (fun t -> t.a);
      } in
      let field1 = Typerep.Field.internal_use_only {
        Typerep.Field_internal.
        label = "b";
        index = 1;
        rep = typerep_of_float;
        tyid = Typename.create ();
        get = (fun t -> t.b);
      } in
      let typename = Typerep.Named.typename_of_t name_of_t in
      let has_double_array_tag =
        Typerep_obj.has_double_array_tag {
          a = Typerep_obj.double_array_value;
          b = Typerep_obj.double_array_value;
        }
      in
      let fields = [|
        Typerep.Record_internal.Field field0;
        Typerep.Record_internal.Field field1;
      |] in
      let create { Typerep.Record_internal.get } =
        let a = get field0 in
        let b = get field1 in
        { a ; b }
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
