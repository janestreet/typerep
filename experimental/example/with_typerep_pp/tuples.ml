open Core.Std let _ = _squelch_unused_module_warning_
open Typerep_experimental.Std

module M1 = struct
  type t = int * string

  let typerep_of_t =
    typerep_of_tuple2 typerep_of_int typerep_of_string
end

module M2 = struct
  type t = int * string * float

  let typerep_of_t =
    typerep_of_tuple3 typerep_of_int typerep_of_string typerep_of_float
end

module M3 = struct
  type t = M1.t * M2.t

  let typerep_of_t =
    typerep_of_tuple2 M1.typerep_of_t M2.typerep_of_t
end
