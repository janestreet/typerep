open Core.Std
open Typerep_experimental.Std

TEST_MODULE = struct

  let inverts_composition_is_ident typerep value : bool =
    let `generic to_typed = Tagged.Typed_of.of_typerep typerep in
    let `generic of_typed = Tagged.Of_typed.of_typerep typerep in
    Polymorphic_compare.equal value (to_typed (of_typed value))

  let check rep v = assert (inverts_composition_is_ident rep v)

  TEST_UNIT =
    let module M = struct
      type t = int with typerep
    end in
    check (M.typerep_of_t) 42

  TEST_UNIT =
    let module M = struct
      type t = int32 with typerep
    end in
    check (M.typerep_of_t) (Int32.of_int_exn 1337)

  TEST_UNIT =
    let module M = struct
      type t = int64 with typerep
    end in
    check (M.typerep_of_t) (Int64.of_int_exn 1980)

  TEST_UNIT =
    let module M = struct
      type t = char with typerep
    end in
    check (M.typerep_of_t) 'a'

  TEST_UNIT =
    let module M = struct
      type t = float with typerep
    end in
    check (M.typerep_of_t) 3.1415

  TEST_UNIT =
    let module M = struct
      type t = bool with typerep
    end in
    check (M.typerep_of_t) true;
    check (M.typerep_of_t) false

  TEST_UNIT =
    let module M = struct
      type t = string with typerep
    end in
    check (M.typerep_of_t) "foo"

  TEST_UNIT =
    let module M = struct
      type t = unit with typerep
    end in
    check (M.typerep_of_t) ()

  TEST_UNIT =
    let module M = struct
      type t = int option with typerep
    end in
    check (M.typerep_of_t) (None) ;
    check (M.typerep_of_t) (Some 2012)

  TEST_UNIT =
    let module M = struct
      type t = string list with typerep
    end in
    check (M.typerep_of_t) [] ;
    check (M.typerep_of_t) ["foo"; "bar"; "baz"]

  TEST_UNIT =
    let module M = struct
      type t = bool array with typerep
    end in
    check (M.typerep_of_t) [||] ;
    check (M.typerep_of_t) [| true ; false ; true |]

  TEST_UNIT =
    let module M = struct
      type 'a t = 'a ref with typerep
    end in
    check (M.typerep_of_t typerep_of_int) (ref 0)

  TEST_UNIT =
    let module M = struct
      type ('a, 'b) t = { foo : 'a ; bar : 'b ; baz : unit } with typerep
    end in
    check (M.typerep_of_t typerep_of_int typerep_of_char) { M. foo=0 ; bar='a' ; baz=() }

  TEST_UNIT =
    let module M = struct
      type t = int * int with typerep
    end in
    check (M.typerep_of_t) (1,2)

  TEST_UNIT =
    let module M = struct
      type t = int * int * int with typerep
    end in
    check (M.typerep_of_t) (1,2,3)

  TEST_UNIT =
    let module M = struct
      type t = int * int * int * int with typerep
    end in
    check (M.typerep_of_t) (1,2,3,4)

  TEST_UNIT =
    let module M = struct
      type t = int * int * int * int * int with typerep
    end in
    check (M.typerep_of_t) (1,2,3,4,5)

  TEST_UNIT =
    let module M = struct
      type 'a t = Nil | Cons of 'a * 'a t with typerep
    end in
    M.(check (typerep_of_t typerep_of_int) (Cons (1, Cons (2, Cons (3, Nil)))))

  TEST_UNIT =
    let module M = struct
      type china = unit with typerep
      type t = [ `The | `Republic of china ] with typerep
    end in
    M.(check typerep_of_t `The) ;
    M.(check typerep_of_t (`Republic ()))
end
