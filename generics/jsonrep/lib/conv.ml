open Core.Std
module Jt = Json.Json_type

exception Type_mismatch of string * Jt.t

let int_of_json = function
  | Jt.Int n -> n
  | json -> raise (Type_mismatch ("Int", json))

let int32_of_json = fun json ->
  let fail () = raise (Type_mismatch ("Int32", json)) in
  match json with
  | Jt.Int n ->begin
    match Int32.of_int n with
    | Some x -> x
    | None -> fail ()
    end
  | _ -> fail ()

let int64_of_json = function
  | Jt.Int n -> Int64.of_int n
  | json -> raise (Type_mismatch ("Int64", json))

let nativeint_of_json = function
  | Jt.Int n -> Nativeint.of_int n
  | json -> raise (Type_mismatch ("Nativeint", json))

let char_of_json = function
  | Jt.String c when Int.equal (String.length c) 1 -> String.get c 0
  | json -> raise (Type_mismatch ("Char", json))

let float_of_json = function
  | Jt.Float n -> n
  | json -> raise (Type_mismatch ("Float", json))

let string_of_json = function
  | Jt.String s -> s
  | json -> raise (Type_mismatch ("String", json))

let bool_of_json = function
  | Jt.Bool b -> b
  | json -> raise (Type_mismatch ("Bool", json))

let unit_of_json = function
  | Jt.Array [] -> ()
  | json -> raise (Type_mismatch ("Unit", json))

let option_of_json t_of_json = function
  | Jt.Null -> None
  | json -> Some (t_of_json json)

let list_of_json t_of_json = function
  | Jt.Array jts -> List.map jts ~f:t_of_json
  | json -> raise (Type_mismatch ("List", json))

let array_of_json t_of_json = function
  | Jt.Array jts -> Array.of_list (List.map jts ~f:t_of_json)
  | json -> raise (Type_mismatch ("Array", json))

let lazy_t_of_json t_of_json = fun json -> Lazy.from_val (t_of_json json)
let ref_of_json t_of_json = fun json -> ref (t_of_json json)

let function_of_json _arg_of_json _ret_of_json =
  fun json -> raise (Type_mismatch ("Function",json))

let tuple2_of_json a_of_json b_of_json = function
  | Jt.Array [a;b] -> a_of_json a, b_of_json b
  | json -> raise (Type_mismatch ("Tuple2", json))

let tuple3_of_json a_of_json b_of_json c_of_json = function
  | Jt.Array [a;b;c] -> a_of_json a, b_of_json b, c_of_json c
  | json -> raise (Type_mismatch ("Tuple3", json))

let tuple4_of_json a_of_json b_of_json c_of_json d_of_json = function
  | Jt.Array [a;b;c;d] -> a_of_json a, b_of_json b, c_of_json c, d_of_json d
  | json -> raise (Type_mismatch ("Tuple4", json))

let tuple5_of_json a_of_json b_of_json c_of_json d_of_json e_of_json = function
  | Jt.Array [a;b;c;d;e] ->
      a_of_json a
    , b_of_json b
    , c_of_json c
    , d_of_json d
    , e_of_json e
  | json -> raise (Type_mismatch ("Tuple5", json))

let json_of_int = fun i -> Jt.Build.int i
let json_of_int32 = fun i -> Jt.Build.int (Int32.to_int_exn i)
let json_of_int64 = fun i -> Jt.Build.int (Int64.to_int_exn i)
let json_of_nativeint = fun i -> Jt.Build.int (Nativeint.to_int_exn i)
let json_of_char = fun c -> Jt.Build.string (Char.to_string c)
let json_of_float = fun f -> Jt.Build.float f
let json_of_string = fun s -> Jt.Build.string s
let json_of_bool = fun b -> Jt.Build.bool b
let json_of_unit = fun () -> Jt.Build.list ident []
let json_of_option json_of_t = function
  | None -> Jt.Build.null
  | Some t -> json_of_t t
let json_of_list json_of_t = fun l -> Jt.Build.list (fun t -> json_of_t t) l
let json_of_array json_of_t = fun arr ->
  Jt.Build.list (fun t -> json_of_t t) (Array.to_list arr)
let json_of_lazy_t json_of_t = fun l -> json_of_t (Lazy.force l)
let json_of_ref json_of_t = fun r -> json_of_t !r
let json_of_function _json_of_arg _json_of_ref = fun _ -> Jt.Build.string "<fun>"

let json_of_tuple2 json_of_a json_of_b = fun (a,b) ->
  Jt.Array [json_of_a a; json_of_b b]
let json_of_tuple3 json_of_a json_of_b json_of_c = fun (a,b,c) ->
  Jt.Array [json_of_a a; json_of_b b; json_of_c c]
let json_of_tuple4 json_of_a json_of_b json_of_c json_of_d = fun (a,b,c,d) ->
  Jt.Array [json_of_a a; json_of_b b; json_of_c c; json_of_d d]
let json_of_tuple5 json_of_a json_of_b json_of_c json_of_d json_of_e = fun (a,b,c,d,e) ->
  Jt.Array [json_of_a a; json_of_b b; json_of_c c; json_of_d d; json_of_e e]
