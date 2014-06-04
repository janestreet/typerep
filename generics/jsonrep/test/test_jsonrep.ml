open Core.Std
open Typerep_experimental.Std
open Json_typerep.Jsonrep

module Jt = Json.Json_type

TEST_MODULE = struct

  type x =
    | Foo
    | Bar of int
    | Baz of float * float
    with typerep

  type mrec = {
    foo: int;
    bar: float
  } with typerep

  type tree = Leaf | Node of tree * tree with typerep

  let test_json x typerep_of_x =
    let test t_of_json json_of_t =
      let `generic x_of_json = t_of_json typerep_of_x in
      let `generic json_of_x = json_of_t typerep_of_x in
      Polymorphic_compare.equal x (x_of_json (json_of_x x))
    in
       test V1.t_of_json V1.json_of_t
    && test V2.t_of_json V2.json_of_t
    && test V2.t_of_json V1.json_of_t
  ;;

  TEST_UNIT = assert(test_json 5 typerep_of_int)
  TEST_UNIT = assert(test_json 'm' typerep_of_char)
  TEST_UNIT = assert(test_json 5.0 typerep_of_float)
  TEST_UNIT = assert(test_json "hello, world" typerep_of_string)
  TEST_UNIT = assert(test_json true typerep_of_bool)
  TEST_UNIT = assert(test_json false typerep_of_bool)
  TEST_UNIT = assert(test_json () typerep_of_unit)
  TEST_UNIT = assert(test_json None (typerep_of_option typerep_of_int))
  TEST_UNIT = assert(test_json (Some 42) (typerep_of_option typerep_of_int))
  TEST_UNIT = assert(test_json [1;2;3;4;5] (typerep_of_list typerep_of_int))
  TEST_UNIT = assert(test_json [|6;7;8;9;19|] (typerep_of_array typerep_of_int))
  TEST_UNIT =
    assert(test_json (52,78)
      (typerep_of_tuple2 typerep_of_int typerep_of_int))
  TEST_UNIT =
    assert(test_json (52,78,89)
      (typerep_of_tuple3 typerep_of_int typerep_of_int typerep_of_int))
  TEST_UNIT =
    assert(test_json (52,78,89, "hi")
      (typerep_of_tuple4
        typerep_of_int typerep_of_int
        typerep_of_int typerep_of_string))
  TEST_UNIT =
    assert(test_json (52,78,89, "hi",false)
      (typerep_of_tuple5
        typerep_of_int typerep_of_int
        typerep_of_int typerep_of_string typerep_of_bool))
  TEST_UNIT = assert(test_json Foo typerep_of_x)
  TEST_UNIT = assert(test_json (Bar 9) typerep_of_x)
  TEST_UNIT = assert(test_json (Baz (6.2, 7.566)) typerep_of_x)
  TEST_UNIT = assert(test_json {foo=5;bar=76.2} typerep_of_mrec)
  TEST_UNIT =
    assert(test_json
      (Node ((Node ((Node (Leaf, Leaf)), Leaf)), Leaf)) typerep_of_tree)
end

TEST_MODULE = struct
  module Jt = Json.Json_type

  type t = {
    a : int option;
  } with typerep

  let `generic t_of_json_v1    = V1.t_of_json typerep_of_t
  let `generic json_of_t_v1    = V1.json_of_t typerep_of_t

  let `generic t_of_json_v2    = V2.t_of_json typerep_of_t
  let `generic json_of_t_v2    = V2.json_of_t typerep_of_t

  module Ocaml = struct
    let some = { a = Some 42 }
    let none = { a = None }
  end

  module Json = struct
    let some      = Jt.Object [ "a",  Jt.Int 42 ]
    let none_with = Jt.Object [ "a",  Jt.Null ]
    let none_sans = Jt.Object []
  end

  TEST = json_of_t_v1   Ocaml.none      = Json.none_with
  TEST = json_of_t_v2   Ocaml.none      = Json.none_sans

  TEST = json_of_t_v1   Ocaml.some      = Json.some
  TEST = json_of_t_v2   Ocaml.some      = Json.some

  TEST = t_of_json_v2   Json.none_sans  = Ocaml.none

  TEST = t_of_json_v1   Json.none_with  = Ocaml.none
  TEST = t_of_json_v2   Json.none_with  = Ocaml.none

  TEST = t_of_json_v1   Json.some       = Ocaml.some
  TEST = t_of_json_v2   Json.some       = Ocaml.some

end
