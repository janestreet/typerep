open Core.Std
open Typerep_experimental.Std

let hash_variant = Typerep_obj.hash_variant

let print_rep name typerep =
  let sexp = Type_struct.sexp_of_typerep typerep in
  print_endline ("struct representation of "^name);
  print_endline (Sexp.to_string_hum sexp)

(* manual examples *)

let f () =
  print_rep "Records.M1.t" Records.M1.typerep_of_t;
  print_rep "Records.M2.t" Records.M2.typerep_of_t;
  print_rep "Records.M3.t" Records.M3.typerep_of_t;

  print_rep "Variants.M1.t" Variants.M1.typerep_of_t;
  print_rep "Variants.M2.t" Variants.M2.typerep_of_t;
  print_rep "Variants.M3.t" Variants.M3.typerep_of_t;

  print_rep "Tuples.M1.t" Tuples.M1.typerep_of_t;
  print_rep "Tuples.M2.t" Tuples.M2.typerep_of_t;
  print_rep "Tuples.M3.t" Tuples.M3.typerep_of_t;

  print_rep "Combination.t" Combination.typerep_of_t;

  print_rep "int Variants.P1.t" (Variants.P1.typerep_of_t typerep_of_int);
  print_rep "int Records.P1.t" (Records.P1.typerep_of_t typerep_of_int);

  print_rep "int Recursives.M1.t" (Recursives.M1.typerep_of_tree typerep_of_int);
;;

let () = if Array.length Sys.argv > 1 && Sys.argv.(1) = "--print-rep" then f ()

let print_typestruct str =
  let sexp = Type_struct.sexp_of_t str in
  print_endline (Sexp.to_string_hum sexp)

module S = Type_struct
module V = S.Variant.Kind

let vr index name array =
  let array = Farray.of_array array ~f:(fun _ x -> x) in
  { S.Variant. label=name ; ocaml_repr = hash_variant name ; index }, array
let vu index ocaml_repr name array =
  let array = Farray.of_array array ~f:(fun _ x -> x) in
  { S.Variant. label=name ; index ; ocaml_repr }, array

let fields t = Farray.of_array t ~f:(fun index (label, value) ->
  let field = { S.Field.index ; label } in
  field, value)

let tags t = Farray.of_array t ~f:(fun _ x -> x)

let stuple array = S.Tuple (Farray.of_array array ~f:(fun _ x -> x))

let simple_array = { S.Record_infos.has_double_array_tag = false }
let double_array = { S.Record_infos.has_double_array_tag = true }

let polymorphic = { S.Variant_infos.kind = V.Polymorphic }
let usual = { S.Variant_infos.kind = V.Usual }

(* General tests about code generation + structure building *)
TEST_MODULE = struct

  let base_check name expected typestruct =
    (* polymorphic equality is ok for Type_struct.t *)
    if Type_struct.are_equivalent typestruct expected then true else begin
      print_endline ("testing "^name);
      print_endline "expected:";
      print_typestruct expected;
      print_endline "built:";
      print_typestruct typestruct;
      false
    end

  let check expected typerep =
    let result = ref true in
    let typestruct = Type_struct.of_typerep typerep in
    result := base_check "typestruct" expected typestruct && !result;
    let Typerep.T typerep_of_t = Type_struct.to_typerep typestruct in
    let typestruct2 : Type_struct.t = Type_struct.of_typerep typerep_of_t in
    result := base_check "typerep" expected typestruct2 && !result;
    let check_version vn =
      let versioned =
        try
          Some (Type_struct.Versioned.serialize ~version:vn typestruct)
        with
        | Type_struct.Versioned.Not_downgradable _ -> None
      in
      match versioned with
      | Some versioned ->
        let typestruct_of_t = Type_struct.Versioned.unserialize versioned in
        let name = Sexp.to_string (Type_struct.Versioned.Version.sexp_of_t vn) in
        result := base_check name expected typestruct_of_t && !result;
      | None -> ()
    in
    List.iter ~f:check_version Type_struct.Versioned.Version.([
      v1;
      v2;
      v3;
      v4;
    ]);
    !result

  (* simple cases *)

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = int
      with typerep
    end in
    let exp = S.Int in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< int >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = int32
      with typerep
    end in
    let exp = S.Int32 in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< int32 >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = int64
      with typerep
    end in
    let exp = S.Int64 in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< int64 >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = char
      with typerep
    end in
    let exp = S.Char in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< char >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float
      with typerep
    end in
    let exp = S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = string
      with typerep
    end in
    let exp = S.String in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< string >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = bool
      with typerep
    end in
    let exp = S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< bool >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = unit
      with typerep
    end in
    let exp = S.Unit in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< unit >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = bool option
      with typerep
    end in
    let exp = S.Option S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< bool option >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float option
      with typerep
    end in
    let exp = S.Option S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float option >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = bool list
      with typerep
    end in
    let exp = S.List S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< bool list >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float list
      with typerep
    end in
    let exp = S.List S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float list >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = bool array
      with typerep
    end in
    let exp = S.Array S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< bool array >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float array
      with typerep
    end in
    let exp = S.Array S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float array >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = bool lazy_t
      with typerep
    end in
    let exp = S.Lazy S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< bool lazy_t >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float lazy_t
      with typerep
    end in
    let exp = S.Lazy S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float lazy_t >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = bool ref
      with typerep
    end in
    let exp = S.Ref S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< bool ref >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float ref
      with typerep
    end in
    let exp = S.Ref S.Float in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float ref >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float * string
      with typerep
    end in
    let exp = S.Tuple (Farray.of_list [ S.Float ; S.String ]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float * string >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float * string * bool
      with typerep
    end in
    let exp = S.Tuple (Farray.of_list [ S.Float ; S.String ; S.Bool ]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float * string * bool >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float * string * bool * unit
      with typerep
    end in
    let exp = S.Tuple (Farray.of_list [ S.Float ; S.String ; S.Bool ; S.Unit ]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float * string * bool * unit >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = float * string * bool * unit * int
      with typerep
    end in
    let exp = S.Tuple (Farray.of_list [ S.Float ; S.String ; S.Bool ; S.Unit ; S.Int ]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< float * string * bool * unit * int >>);
  ;;


  (* nested with previous types *)

  TEST_UNIT =
    let module A : sig
      type t with typerep
    end = struct
      type t = bool
      with typerep
    end in
    let module M : sig
      type t with typerep
    end = struct
      type t = A.t option
      with typerep
    end in
    let exp = S.Option S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< A.t option >>);
  ;;

  TEST_UNIT =
    let module A : sig
      type t with typerep
    end = struct
      type t = bool
      with typerep
    end in
    let module M : sig
      type t with typerep
    end = struct
      type t = A.t list
      with typerep
    end in
    let exp = S.List S.Bool in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< M.t >>);
  ;;

  (* records *)

  TEST_UNIT =
    let module M : sig
      type t with typerep
      module A : sig
        type nonrec t = t with typerep
      end
    end = struct
      module T = struct
        type t = {
          float : float;
          string : string;
          bool : bool;
          unit : unit;
          int : int;
        } with typerep
      end
      module A = struct
        type t = T.t = {
          float : float;
          string : string;
          bool : bool;
          unit : unit;
          int : int;
        } with typerep
      end
      include T
    end in
    let exp = S.Record (simple_array, fields [|
      "float", S.Float;
      "string", S.String;
      "bool", S.Bool;
      "unit", S.Unit;
      "int", S.Int;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< M.t >>);
    assert (check exp M.A.typerep_of_t);
    assert (check exp <:typerep_of< M.A.t >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = {
        f1 : float;
        f2 : float;
      } with typerep
    end in
    let exp = S.Record (double_array, fields [|
      "f1", S.Float;
      "f2", S.Float;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< M.t >>);
  ;;

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type 'a poly = {
        f1 : float;
        f2 : 'a;
      } with typerep
      type t = float poly with typerep
    end in
    let exp = S.Record (simple_array, fields [|
      "f1", S.Float;
      "f2", S.Float;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< M.t >>);
  ;;

  (* variants arity 1 *)

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = [
      | `float of float
      | `string of string
      | `bool of bool
      | `unit of unit
      | `int of int
      ] with typerep
    end in
    let exp = S.Variant (polymorphic, tags [|
      vr 0 "float" [| S.Float |];
      vr 1 "string" [| S.String |];
      vr 2 "bool" [| S.Bool |];
      vr 3 "unit" [| S.Unit |];
      vr 4 "int" [| S.Int |];
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< M.t >>);
  ;;

  (* variants arity n *)

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t = [
      | `zero
      | `one of unit
      | `two of bool * bool
      | `three of unit * unit * unit
      | `five of unit * unit * unit * unit * unit
      ] with typerep
    end in
    let exp = S.Variant (polymorphic, tags [|
      vr 0 "zero" [||];
      vr 1 "one" [| S.Unit |];
      vr 2 "two" [| S.Tuple (Farray.of_list [ S.Bool ; S.Bool ]) |];
      vr 3 "three" [| S.Tuple (Farray.of_list [ S.Unit ; S.Unit ; S.Unit ]) |];
      vr 4 "five" [| S.Tuple (Farray.init 5 ~f:(fun _ -> S.Unit)) |];
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< M.t >>);
  ;;

  (* sum arity 1 *)

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t =
      | Float of float
      | String of string
      | Bool of bool
      | Unit of unit
      | Int of int
      with typerep
    end in
    let exp = S.Variant (usual, tags [|
      vu 0 0 "Float" [| S.Float |];
      vu 1 1 "String" [| S.String |];
      vu 2 2 "Bool" [| S.Bool |];
      vu 3 3 "Unit" [| S.Unit |];
      vu 4 4 "Int" [| S.Int |];
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< M.t >>);
  ;;

  (* sum arity n *)

  TEST_UNIT =
    let module M : sig
      type t with typerep
    end = struct
      type t =
      | Zero
      | One of unit
      | Two of bool * bool
      | Two_tuple of (bool * bool)
      | Three of unit * unit * unit
      | Three_tuple of (unit * unit * unit)
      | Five of unit * unit * unit * unit * unit
      | Five_tuple of (unit * unit * unit * unit * unit)
      with typerep
    end in
    let exp = S.Variant (usual, tags [|
      vu 0 0 "Zero" [||];
      vu 1 0 "One" [| S.Unit |];
      vu 2 1 "Two" [| S.Bool ; S.Bool |];
      vu 3 2 "Two_tuple" [| S.Tuple (Farray.of_list [ S.Bool ; S.Bool ]) |];
      vu 4 3 "Three" [| S.Unit ; S.Unit ; S.Unit |];
      vu 5 4 "Three_tuple" [| S.Tuple (Farray.of_list [ S.Unit ; S.Unit ; S.Unit ]) |];
      vu 6 5 "Five" (Array.init 5 ~f:(fun _ -> S.Unit));
      vu 7 6 "Five_tuple" [| S.Tuple (Farray.init 5 ~f:(fun _ -> S.Unit)) |];
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< M.t >>);
  ;;

  (* polymorphism *)

  (* records *)

  TEST_UNIT =
    let module A : sig
      type 'a t with typerep
    end = struct
      type 'a t = 'a * int with typerep
    end in
    let module M : sig
      type ('a, 'b, 'c) t with typerep
    end = struct
      type ('a, 'b, 'c) t = {
        a : 'a * 'a * 'a ;
        b : 'b;
        c : 'c;
        int : int;
        t_A : 'c A.t;
      } with typerep
    end in
    let module M1 : sig
      type t = (float, int A.t, int A.t A.t) M.t with typerep
    end = struct
      type t = (float, int A.t, int A.t A.t) M.t with typerep
    end in
    let exp = S.Record (simple_array, fields [|
      "a", S.Tuple (Farray.of_list [ S.Float; S.Float ; S.Float ]);
      "b", S.Tuple (Farray.of_list [ S.Int ; S.Int ]);
      "c", S.Tuple (Farray.of_list [ S.Tuple (Farray.of_list [ S.Int ; S.Int ]) ; S.Int ]);
      "int", S.Int;
      "t_A", S.Tuple (Farray.of_list
        [ S.Tuple (Farray.of_list [ S.Tuple (Farray.of_list [ S.Int ; S.Int ]) ; S.Int ]);
          S.Int ]);
    |]) in
    assert (check exp M1.typerep_of_t);
    assert (check exp <:typerep_of< (float, int A.t, int A.t A.t) M.t >>);
  ;;

  (* variants *)

  TEST_UNIT =
    let module A : sig
      type 'a t with typerep
    end = struct
      type 'a t = [
      | `a of 'a
      | `int of int * string
      ] with typerep
    end in
    let module M : sig
      type ('a, 'b, 'c) t with typerep
    end = struct
      type ('a, 'b, 'c) t = [
      | `a of 'a * 'a * 'a
      | `b of 'b
      | `c of 'c
      | `int of int
      | `t_A of 'a A.t
      | `no_arg
      ] with typerep
    end in
    let module M1 : sig
      type t = (float, int A.t, int A.t A.t) M.t with typerep
    end = struct
      type t = (float, int A.t, int A.t A.t) M.t with typerep
    end in
    let exp = S.Variant (polymorphic, tags [|
      vr 0 "a" [| stuple [| S.Float; S.Float ; S.Float |] |];
      vr 1 "b" [| S.Variant (polymorphic, tags [|
        vr 0 "a" [| S.Int |] ;
        vr 1 "int" [| stuple [| S.Int ; S.String |] |] |]) |];
      vr 2 "c" [| S.Variant (polymorphic, tags [|
        vr 0 "a" [| S.Variant (polymorphic, tags
                                [| vr 0 "a" [| S.Int |] ;
                                   vr 1 "int" [| stuple [| S.Int ; S.String |] |] |]) |];
                           vr 1 "int" [| stuple [| S.Int ; S.String |] |] |]) |];
      vr 3 "int" [| S.Int |];
      vr 4 "t_A" [| S.Variant (polymorphic, tags [| vr 0 "a" [| S.Float |] ;
                            vr 1 "int" [| stuple [| S.Int ; S.String |] |] |]) |];
      vr 5 "no_arg" [||];
    |]) in
    assert (check exp M1.typerep_of_t);
    assert (check exp <:typerep_of< (float, int A.t, int A.t A.t) M.t >>);
  ;;

  (* sums *)

  TEST_UNIT =
    let module A : sig
      type 'a t with typerep
    end = struct
      type 'a t =
      | A of 'a
      | Int of int * string
      with typerep
    end in
    let module M : sig
      type ('a, 'b, 'c) t with typerep
    end = struct
      type ('a, 'b, 'c) t =
      | A of ('a * 'a * 'a)
      | B of 'b
      | C of 'c
      | Int of int
      | T_A of 'a A.t
      | No_arg
      with typerep
    end in
    let module M1 : sig
      type t = (float, int A.t, int A.t A.t) M.t with typerep
    end = struct
      type t = (float, int A.t, int A.t A.t) M.t with typerep
    end in
    let exp = S.Variant (usual, tags [|
      vu 0 0 "A" [| stuple [| S.Float; S.Float ; S.Float |] |];
      vu 1 1 "B" [| S.Variant (usual, tags [|
        vu 0 0 "A" [| S.Int |] ;
        vu 1 1 "Int" [| S.Int ; S.String |] |]) |];
      vu 2 2 "C" [| S.Variant (usual, tags [| vu 0 0 "A"
                           [| S.Variant (usual, tags
                               [| vu 0 0 "A" [| S.Int |] ;
                                  vu 1 1 "Int" [| S.Int ; S.String |] |]) |];
                           vu 1 1 "Int" [| S.Int ; S.String |] |]) |];
      vu 3 3 "Int" [| S.Int |];
      vu 4 4 "T_A" [| S.Variant (usual, tags [| vu 0 0 "A" [| S.Float |] ;
                            vu 1 1 "Int" [| S.Int ; S.String |] |]) |];
      vu 5 0 "No_arg" [||];
    |]) in
    assert (check exp M1.typerep_of_t);
    assert (check exp <:typerep_of< (float, int A.t, int A.t A.t) M.t >>);
  ;;

  (* phantom and mutability *)
  TEST_UNIT =
    let module A = struct
      type ('a,'b) t = { mutable foo: 'a } with typerep
    end in
    let module M = struct
      type t = (unit, int) A.t with typerep
    end in
    let exp = S.Record (simple_array, fields [|
      "foo", S.Unit;
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check exp <:typerep_of< (unit, int) A.t >>);
  ;;

  (* sort of a real case *)

  TEST_UNIT =
    let module Transaction_type = struct
      module V1 = struct
        type t =
        | Trade
        | Order
        with typerep
      end
      module V2 = struct
        module Account : sig
          type t = private string with typerep
        end = struct
          type t = string with typerep
        end
        type t =
        | Trade
        | Order
        | Journal of Account.t * Account.t
        with typerep
      end
    end in
    let tt_v1 = S.Variant (usual, tags [|
      vu 0 0 "Trade" [||];
      vu 1 1 "Order" [||];
    |]) in
    assert (check tt_v1 Transaction_type.V1.typerep_of_t);
    let tt_v2 = S.Variant (usual, tags [|
      vu 0 0 "Trade" [||];
      vu 1 1 "Order" [||];
      vu 2 0 "Journal" [| S.String ; S.String |];
    |]) in
    assert (check tt_v2 Transaction_type.V2.typerep_of_t);
    let module V1 = struct
      type t = {
        transaction_type : Transaction_type.V1.t;
        username : string;
      } with typerep
    end in
    let module V2 = struct
      type t = {
        transaction_type : Transaction_type.V2.t;
        username : string;
        tags : (string * string) list;
      } with typerep
    end in
    let module M_v1 = struct
      type t =
      | V1 of V1.t
      with typerep
    end in
    let module M_v2 = struct
      type t =
      | V1 of V1.t
      | V2 of V2.t
      with typerep
    end in
    let v1 = S.Record (simple_array, fields [|
      "transaction_type", tt_v1;
      "username", S.String ;
    |]) in
    let v2 = S.Record (simple_array, fields [|
      "transaction_type", tt_v2;
      "username", S.String ;
      "tags", S.List (stuple [| S.String ; S.String |]);
    |]) in
    let exp_v1 = S.Variant (usual, tags [| vu 0 0 "V1" [| v1 |] |]) in
    let exp_v2 = S.Variant (usual, tags [|
      vu 0 0 "V1" [| v1 |] ;
      vu 1 1 "V2" [| v2 |] ;
    |]) in
    assert (check exp_v1 M_v1.typerep_of_t);
    assert (check exp_v2 M_v2.typerep_of_t);

  (* recursive types *)

  (* sum *)

  TEST_UNIT =
    let module M = struct
      type t =
      | Zero
      | Succ of t
      with typerep
    end in
    let exp = S.Named (0, Some (S.Variant (usual, tags [|
      vu 0 0 "Zero" [||];
      vu 1 0 "Succ" [| S.Named (0, None) |];
    |]))) in
    let cyclic = S.Named (42, Some (S.Variant (usual, tags [|
      vu 0 0 "Zero" [||];
      vu 1 0 "Succ" [| S.Named (42, None) |];
    |]))) in
    assert (check cyclic M.typerep_of_t);
    assert (S.are_equivalent exp cyclic);
    assert (check exp M.typerep_of_t);
  ;;

  TEST_UNIT =
    let module M = struct
      type t =
      | Leaf
      | Node of t * t
      with typerep
    end in
    let exp = S.Named (0, Some (S.Variant (usual, tags [|
      vu 0 0 "Leaf" [||];
      vu 1 0 "Node" [| S.Named (0, None) ; S.Named (0, None) |];
    |]))) in
    assert (check exp M.typerep_of_t);
  ;;

  (* polymorphic *)

  TEST_UNIT =
    let module M = struct
      type t = [
      | `Zero
      | `Succ of t
      ] with typerep
    end in
    let exp = S.Named (0, Some (S.Variant (polymorphic, tags [|
      vr 0 "Zero" [||];
      vr 1 "Succ" [| S.Named (0, None) |];
    |]))) in
    let cyclic = S.Named (42, Some (S.Variant (polymorphic, tags [|
      vr 0 "Zero" [||];
      vr 1 "Succ" [| S.Named (42, None) |];
    |]))) in
    assert (check cyclic M.typerep_of_t);
    assert (S.are_equivalent exp cyclic);
    assert (check exp M.typerep_of_t);
  ;;

  (* record *)

  TEST_UNIT =
    let module M = struct
      type t = {
        int : int;
        self : t;
      } with typerep
    end in
    let exp = S.Named (0, Some (S.Record (simple_array, fields [|
      "int", S.Int;
      "self", S.Named (0, None);
    |]))) in
    let cyclic = S.Named (0, Some (S.Record (simple_array, Farray.of_list [
      { S.Field.label="int";index=0}, S.Int;
      { S.Field.label="self";index=1}, S.Named (0, None);
    ]))) in
    let exp2 = S.Record (simple_array, fields [|
      "int", S.Int;
      "self", S.Named (0, Some (
        S.Record (simple_array, fields [|
          "int", S.Int;
          "self", S.Named (0, Some (
            S.Record (simple_array, fields [|
              "int", S.Int;
              "self", S.Named (0, None);
            |])
          ))|])));
    |]) in
    assert (check exp M.typerep_of_t);
    assert (check cyclic M.typerep_of_t);
    assert (check exp2 M.typerep_of_t);
  ;;

  (* with parameters *)

  (* sum *)

  TEST_UNIT =
    let module M = struct
      type 'a tree =
      | Leaf of 'a
      | Tree of 'a * 'a tree * 'a tree
      with typerep
    end in
    let exp arg = S.Named (0, Some (S.Variant (usual, tags [|
      vu 0 0 "Leaf" [| arg |];
      vu 1 1 "Tree" [| arg ; S.Named (0, None) ; S.Named (0, None) |];
    |]))) in
    assert (check (exp S.Int) (M.typerep_of_tree typerep_of_int));
    assert (check (exp S.Float) (M.typerep_of_tree typerep_of_float));
  ;;

  TEST_UNIT =
    let module T = struct
      type ('a, 'b) t =
      | Empty
      | Node of ('b, 'a) t
      | A of 'a
      | B of 'b
      with typerep
    end in
    let module M = struct
      type t = (int, string) T.t
      with typerep
    end in
    let exp = S.Named (0, Some (S.Variant (usual, tags [|
      vu 0 0 "Empty" [||];
      vu 1 0 "Node" [| S.Variant (usual, tags [|
        vu 0 0 "Empty" [||];
        vu 1 0 "Node" [| S.Named (0, None) |];
        vu 2 1 "A" [| S.String |];
        vu 3 2 "B" [| S.Int |];
      |])|];
      vu 2 1 "A" [| S.Int |];
      vu 3 2 "B" [| S.String |];
    |]))) in
    assert (check exp M.typerep_of_t);
  ;;

  (* inlining not named polymorphic variant types *)

  TEST_UNIT =
    let module A = struct
      type t = A of [ `A ] with typerep
      let exp = S.Variant (usual, tags [|
        vu 0 0 "A" [|
          S.Variant (polymorphic, tags [|
            vr 0 "A" [||];
          |]);
        |];
      |])
    end in
    assert (check A.exp A.typerep_of_t);
  ;;

  TEST_UNIT =
    let module A = struct
      type 'a t = [ `A of 'a | `B ] with typerep
      let exp arg = S.Variant (polymorphic, tags [|
        vr 0 "A" [| arg |];
        vr 1 "B" [||];
      |])
      let param = S.Variant (polymorphic, tags [|
        vr 0 "A" [||];
      |])
    end in
    let module B = struct
      type ('a, 't, 'b) t = [ `A of 'a * 't | `B of 'b ] with typerep
      let exp a t b = S.Variant (polymorphic, tags [|
        vr 0 "A" [| S.Tuple (Farray.of_list [ a ; t ]) |];
        vr 1 "B" [| b |];
      |])
    end in
    let module M = struct
      type 'a t =
      | Leaf of [ `A of 'a * 'a t | `B of [ `A of 'a | `B ] ]
      | Tree of 'a * 'a t * 'a t
      with typerep
      let exp arg = S.Named (0, Some (S.Variant (usual, tags [|
        vu 0 0 "Leaf" [| B.exp arg (S.Named (0, None)) (A.exp arg) |];
        vu 1 1 "Tree" [| arg ; S.Named (0, None) ; S.Named (0, None) |];
      |])))
    end in
    assert (check (A.exp A.param) <:typerep_of< [ `A ] A.t >>);
    assert (check (A.exp S.Int) (A.typerep_of_t typerep_of_int));
    assert (check (A.exp S.Int) <:typerep_of< [ `A of int | `B ] >>);
    assert (check (A.exp S.String) (A.typerep_of_t typerep_of_string));
    assert (check (A.exp S.String) <:typerep_of< [ `A of string | `B ] >>);
    assert (check (B.exp S.Int S.String S.Float)
              (B.typerep_of_t typerep_of_int typerep_of_string typerep_of_float));
    assert (check (B.exp S.String S.Float S.Int)
              (B.typerep_of_t typerep_of_string typerep_of_float typerep_of_int));
    assert (check (M.exp S.Int) (M.typerep_of_t typerep_of_int));
    assert (check (M.exp S.String) (M.typerep_of_t typerep_of_string));
  ;;

  TEST_UNIT =
    let module A = struct
      type t = True of int | False with typerep
    end in
    (* the preprocessor for polymorphic variants should do the right thing, but camlp4
       doesn't so the generated code doesn't even type (the type definition is that caml
       receives contains the constructor ` True (ie " True"))
       let module B = struct
         type t = [ `True | `False of int ]
         let _ = (`True : t)
       end in *)
    assert (check (S.Variant (usual, tags [|
        vu 1 0 "True" [| S.Int |];
        vu 0 0 "False" [| |];
      |])) A.typerep_of_t)

end

(* breaking the abstraction ? *)
TEST_MODULE = struct

  module A : sig
    type 'a t
    include Typerepable.S1 with type 'a t := 'a t
    val create : 'a -> 'a t
  end = struct
    type 'a t = 'a option with typerep
    let create a = Some a
  end

  module B : sig
    type 'a t
    include Typerepable.S1 with type 'a t := 'a t
    val read : 'a t -> 'a
  end = struct
    type 'a t = 'a option with typerep
    let read = function
      | Some a -> a
      | None -> assert false
  end

  TEST_UNIT =
    let module M = struct
      type a = int A.t with typerep
      type b = int B.t with typerep
    end in
    let break (a : M.a) : int option =
      match
        Typename.same_witness
          (Typerep.typename_of_t M.typerep_of_a)
          (Typerep.typename_of_t M.typerep_of_b)
      with
      | Some proof ->
        let b = Type_equal.conv proof a in
        Some (B.read b)
      | None -> None
    in
    let a = A.create 42 in
    assert (break a = None)

  TEST_UNIT =
    let module M = struct
      type t =
      | Foo1 of int
      | Foo2 of int
      | Foo3 of int
      with typerep

      let get_int =
        match Typerep.head typerep_of_t with
        | Typerep.Variant variant -> (fun t ->
          let Typerep.Variant.Value (tag, arg) = Typerep.Variant.value variant t in
          let witness =
            Typerep.same_witness_exn (Typerep.Tag.traverse tag) typerep_of_int
          in
          Type_equal.conv witness arg
        )
        | _ -> assert false
    end
    in
    assert (M.get_int (M.Foo1 1) = 1);
    assert (M.get_int (M.Foo2 2) = 2);
end
