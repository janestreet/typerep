open Core.Std

exception Type_mismatch of string * Json.Json_type.t

val int_of_json : Json.Json_type.t -> int
val int32_of_json : Json.Json_type.t -> int32
val int64_of_json : Json.Json_type.t -> int64
val nativeint_of_json : Json.Json_type.t -> nativeint
val char_of_json : Json.Json_type.t -> char
val float_of_json : Json.Json_type.t -> float
val string_of_json : Json.Json_type.t -> string
val bool_of_json : Json.Json_type.t -> bool
val unit_of_json : Json.Json_type.t -> unit
val option_of_json : (Json.Json_type.t -> 'a) -> Json.Json_type.t -> 'a option
val list_of_json : (Json.Json_type.t -> 'a) -> Json.Json_type.t -> 'a list
val array_of_json : (Json.Json_type.t -> 'a) -> Json.Json_type.t -> 'a array
val lazy_t_of_json : (Json.Json_type.t -> 'a) -> Json.Json_type.t -> 'a lazy_t
val function_of_json :
  (Json.Json_type.t -> 'a)
  -> (Json.Json_type.t -> 'b)
  -> Json.Json_type.t -> ('a -> 'b)
val ref_of_json : (Json.Json_type.t -> 'a) -> Json.Json_type.t -> 'a ref
val tuple2_of_json :
  (Json.Json_type.t -> 'a)
  -> (Json.Json_type.t -> 'b)
  -> Json.Json_type.t -> ('a * 'b)
val tuple3_of_json :
  (Json.Json_type.t -> 'a)
  -> (Json.Json_type.t -> 'b)
  -> (Json.Json_type.t -> 'c)
  -> Json.Json_type.t -> ('a * 'b * 'c)
val tuple4_of_json  :
  (Json.Json_type.t -> 'a)
  -> (Json.Json_type.t -> 'b)
  -> (Json.Json_type.t -> 'c)
  -> (Json.Json_type.t -> 'd)
  -> Json.Json_type.t -> ('a * 'b * 'c * 'd)
val tuple5_of_json  :
  (Json.Json_type.t -> 'a)
  -> (Json.Json_type.t -> 'b)
  -> (Json.Json_type.t -> 'c)
  -> (Json.Json_type.t -> 'd)
  -> (Json.Json_type.t -> 'e)
  -> Json.Json_type.t -> ('a * 'b * 'c * 'd * 'e)

val json_of_int : int -> Json.Json_type.t
val json_of_int32 : int32 -> Json.Json_type.t
val json_of_int64 : int64 -> Json.Json_type.t
val json_of_nativeint : nativeint -> Json.Json_type.t
val json_of_char : char -> Json.Json_type.t
val json_of_float : float -> Json.Json_type.t
val json_of_string : string -> Json.Json_type.t
val json_of_bool : bool -> Json.Json_type.t
val json_of_unit : unit -> Json.Json_type.t

val json_of_option : ('a  -> Json.Json_type.t) -> 'a option -> Json.Json_type.t
val json_of_list : ('a  -> Json.Json_type.t) -> 'a list -> Json.Json_type.t
val json_of_array : ('a  -> Json.Json_type.t) -> 'a array -> Json.Json_type.t
val json_of_lazy_t : ('a  -> Json.Json_type.t) -> 'a lazy_t -> Json.Json_type.t
val json_of_ref : ('a  -> Json.Json_type.t) -> 'a ref -> Json.Json_type.t

val json_of_function :
  ('a  -> Json.Json_type.t) ->
  ('b  -> Json.Json_type.t) ->
  ('a -> 'b) -> Json.Json_type.t

val json_of_tuple2 :
  ('a -> Json.Json_type.t) -> ('b -> Json.Json_type.t)
  -> ('a * 'b) -> Json.Json_type.t
val json_of_tuple3 :
  ('a -> Json.Json_type.t) -> ('b -> Json.Json_type.t)
  -> ('c -> Json.Json_type.t)
  -> ('a * 'b * 'c) -> Json.Json_type.t
val json_of_tuple4 :
  ('a -> Json.Json_type.t) -> ('b -> Json.Json_type.t)
  -> ('c -> Json.Json_type.t) -> ('d -> Json.Json_type.t)
  -> ('a * 'b * 'c * 'd) -> Json.Json_type.t
val json_of_tuple5 : ('a -> Json.Json_type.t) -> ('b -> Json.Json_type.t)
  -> ('c -> Json.Json_type.t) -> ('d -> Json.Json_type.t) -> ('e -> Json.Json_type.t)
  -> ('a * 'b * 'c * 'd * 'e) -> Json.Json_type.t
