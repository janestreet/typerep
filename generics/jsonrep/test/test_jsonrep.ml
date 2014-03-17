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
    let `generic x_of_json = t_of_json typerep_of_x in
    let `generic json_of_x = json_of_t typerep_of_x in
    Polymorphic_compare.equal x (x_of_json (json_of_x x))

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
