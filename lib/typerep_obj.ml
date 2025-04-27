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
let[@inline never] double_array_value () = Sys.opaque_identity (Stdlib.Obj.magic 0.)

let double_array_non_value (type any : any) (typerep : any Typerep.t_non_value)
  : unit -> any
  =
  match Typerep.head typerep with
  | Float_u -> fun () -> #4.0
  | Int32_u -> fun () -> #4l
  | Int64_u -> fun () -> #4L
  | Nativeint_u -> fun () -> #4n
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

    let double = { a = double_array_value (); b = double_array_value () }
    let simple = { c = double_array_value (); d = double_array_value () }
  end
  in
  assert (has_double_array_tag M.double);
  assert (not (has_double_array_tag M.simple))
;;
