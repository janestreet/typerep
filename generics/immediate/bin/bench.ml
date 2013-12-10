open Core.Std
open Core_bench.Std
open Typerep_kernel.Std

module Immediate = Typerep_immediate.Std.Immediate
module Always = Immediate.Always

module M = struct
  type t = A | B | C with typerep

  let always = Option.value_exn (Always.of_typerep typerep_of_t)
end

let tests =
  [
    Bench.Test.create ~name:"Always.value_as_int"
      (fun () -> ignore (Always.value_as_int M.always M.A))
  ]
;;

let () = Command.run (Bench.make_command tests)
