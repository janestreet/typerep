open! Base
open Std_internal

(* using the hash_variant of pa_type_conv at compile time *)
let repr_of_poly_variant : [> ] -> int =
  fun variant ->
  let obj = Stdlib.Obj.repr variant in
  if Stdlib.Obj.is_int obj
  then Stdlib.Obj.obj obj
  else (
    let size = Stdlib.Obj.size obj in
    assert (size = 2);
    let repr = Stdlib.Obj.field obj 0 in
    assert (Stdlib.Obj.is_int repr);
    Stdlib.Obj.obj repr)
;;

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := (223 * !accu) + Char.to_int s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land ((1 lsl 31) - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu
;;

(* a few unit tests of cases that have triggered diffs in the past of this
   lib *)
let () = assert (repr_of_poly_variant `Latency_stats = hash_variant "Latency_stats")
let () = assert (repr_of_poly_variant `zero = hash_variant "zero")

external opaque : ('a : any). 'a -> 'a @@ portable = "%opaque" [@@layout_poly]

external obj_magic : ('a : any) ('b : any). 'a -> 'b @@ portable = "%obj_magic"
[@@layout_poly]

let double_array_value (type a : any) (typerep : a Typerep.t) : unit -> a =
  match Typerep.kind typerep with
  | Value -> fun [@inline never] () -> opaque (obj_magic 0.)
  | Value_or_null -> fun [@inline never] () -> opaque (obj_magic Null)
  | Float64 -> fun [@inline never] () -> opaque (obj_magic #1.0)
  | Bits32 -> fun [@inline never] () -> opaque (obj_magic #2l)
  | Bits64 -> fun [@inline never] () -> opaque (obj_magic #3L)
  | Word -> fun [@inline never] () -> opaque (obj_magic #4n)
  | Tuple2_u _ | Tuple3_u _ | Tuple4_u _ | Tuple5_u _ ->
    failwith "records containing unboxed tuples are not supported"
;;

let has_double_array_tag a =
  Stdlib.Obj.double_array_tag = Stdlib.Obj.tag (Stdlib.Obj.repr a)
;;

let () =
  let module M = struct
    type double =
      { a : float
      ; b : float
      }

    type simple =
      { c : float
      ; d : int
      }

    let double =
      { a = double_array_value typerep_of_float ()
      ; b = double_array_value typerep_of_float ()
      }
    ;;

    let simple =
      { c = double_array_value typerep_of_float ()
      ; d = double_array_value typerep_of_int ()
      }
    ;;
  end
  in
  assert (has_double_array_tag M.double);
  assert (not (has_double_array_tag M.simple))
;;
