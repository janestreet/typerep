open Core.Std
open Typereplib.Std

module S = Sexprep

(* Tests to make sure we can deserialize values we serialized *)
(* Tests to make sure we can deserialize values serialized by with sexp *)
(* Tests to make sure with sexp can deserialize values serialized by us *)
TEST_MODULE = struct

  let t_of_sexp_of_sexp_of_t typerep value =
    let `generic t_of_sexp = S.t_of_sexp typerep in
    let `generic sexp_of_t = S.sexp_of_t typerep in
    t_of_sexp (sexp_of_t value)

  let check_untyped value typerep =
    let `generic t_of_sexp = S.t_of_sexp typerep in
    let `generic sexp_of_t = S.sexp_of_t typerep in
    let `generic sexp_of_un =
      Sexprep.Tagged.Sexp_of.of_typestruct (Type_struct.of_typerep typerep)
    in
    let `generic un_of_sexp =
      Sexprep.Tagged.Of_sexp.of_typestruct (Type_struct.of_typerep typerep)
    in
    let new_value = t_of_sexp (sexp_of_un (un_of_sexp (sexp_of_t value))) in
    Polymorphic_compare.equal new_value value

  let check_typerep value typerep =
    let str = t_of_sexp_of_sexp_of_t typerep value in
    Polymorphic_compare.equal value str

  let check_obj_typerep (type a) (value:a) (typerep:a Typerep.t) =
    let `generic sexp_of_t = S.sexp_of_t typerep in
    let sexp = sexp_of_t value in
    let typestruct = Type_struct.of_typerep typerep in
    let typerep_of_t = Type_struct.recreate_dynamically_typerep_for_test typerep in
    let objstruct = Type_struct.of_typerep typerep_of_t in
    let fail = ref false in
    let () =
      let `generic t_of_sexp = S.t_of_sexp typerep_of_t in
      let obj_value = t_of_sexp sexp in
      if not (Polymorphic_compare.equal obj_value value) then begin
        fail := true;
        Printf.printf "typestruct: %s\n"
          (Sexp.to_string_hum (Type_struct.sexp_of_t typestruct));
        Printf.printf "objstruct: %s\n"
          (Sexp.to_string_hum (Type_struct.sexp_of_t objstruct));
        Printf.printf "polymorphic equality failed obj_value: %S\n%!"
          (Sexp.to_string_hum sexp)
      end
    in
    let () =
      let `generic sexp_of_t = S.sexp_of_t typerep_of_t in
      let obj_sexp = sexp_of_t value in
      let obj_str = Sexp.to_string_hum obj_sexp in
      let value_str = Sexp.to_string_hum sexp in
      if not (String.equal obj_str value_str) then begin
        fail := true;
        Printf.printf "typestruct: %s\n"
          (Sexp.to_string_hum (Type_struct.sexp_of_t typestruct));
        Printf.printf "objstruct: %s\n"
          (Sexp.to_string_hum (Type_struct.sexp_of_t objstruct));
        Printf.printf "sexp equality failed obj_value:\nvalue sexp:\n%s\nobj sexp\n%s\n%!"
          value_str
          obj_str
      end
    in
    not !fail

  let check value typerep =
    if (check_typerep value typerep)
      && (check_untyped value typerep)
      && (check_obj_typerep value typerep)
    then true else
    begin
      let `generic sexp_of_t = S.sexp_of_t typerep in
      let `generic t_of_sexp = S.t_of_sexp typerep in
      let sexp = sexp_of_t value in
      let second_sexp = sexp_of_t (t_of_sexp sexp) in
      print_endline "sexp:";
      print_endline (Sexp.to_string_hum sexp);
      print_endline "sexp of (t of (sexp of t)):";
      print_endline (Sexp.to_string_hum second_sexp);
      false
    end

  let check_of_sexp value typerep sexp_of_t =
    let `generic t_of_sexp = S.t_of_sexp typerep in
    Polymorphic_compare.equal value (t_of_sexp (sexp_of_t value))

  let check_of_t value typerep t_of_sexp =
    let `generic sexp_of_t = S.sexp_of_t typerep in
    Polymorphic_compare.equal value (t_of_sexp (sexp_of_t value))

  TEST_UNIT =
    let module M = struct
      type t = int with typerep, sexp
    end in
    let value = 5 in
    assert(check value M.typerep_of_t);
    assert(check_of_sexp value M.typerep_of_t M.sexp_of_t);
    assert(check_of_t value M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type t = int32 with typerep, sexp
    end in
    let value = Int32.of_int_exn 5 in
    assert(check value M.typerep_of_t);
    assert(check_of_sexp value M.typerep_of_t M.sexp_of_t);
    assert(check_of_t value M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type t = int64 with typerep, sexp
    end in
    let value = Int64.of_int_exn 5 in
    assert(check value M.typerep_of_t);
    assert(check_of_sexp value M.typerep_of_t M.sexp_of_t);
    assert(check_of_t value M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type t = char with typerep, sexp
    end in
    let value = 'c' in
    assert(check value M.typerep_of_t);
    assert(check_of_sexp value M.typerep_of_t M.sexp_of_t);
    assert(check_of_t value M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type t = float with typerep, sexp
    end in
    let value = 543.02 in
    assert(check value M.typerep_of_t);
    assert(check_of_sexp value M.typerep_of_t M.sexp_of_t);
    assert(check_of_t value M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type t = string with typerep, sexp
    end in
    let value = "Hello, world!" in
    assert(check value M.typerep_of_t);
    assert(check_of_sexp value M.typerep_of_t M.sexp_of_t);
    assert(check_of_t value M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type t = bool with typerep, sexp
    end in
    assert(check true M.typerep_of_t);
    assert(check false M.typerep_of_t);
    assert(check_of_sexp true M.typerep_of_t M.sexp_of_t);
    assert(check_of_sexp false M.typerep_of_t M.sexp_of_t);
    assert(check_of_t true M.typerep_of_t M.t_of_sexp);
    assert(check_of_t false M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type t = unit with typerep, sexp
    end in
    assert(check () M.typerep_of_t);
    assert(check_of_sexp () M.typerep_of_t M.sexp_of_t);
    assert(check_of_t () M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type 'a t = 'a option with typerep, sexp
    end in
    assert(check None (M.typerep_of_t typerep_of_int));
    assert(check (Some 5) (M.typerep_of_t typerep_of_int));
    assert(check_of_sexp None
      (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_sexp (Some 5)
      (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_t None
      (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp));
    assert(check_of_t (Some 5)
      (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp))

  TEST_UNIT =
    let module M = struct
      type 'a t = 'a list with typerep, sexp
    end in
    assert(check [] (M.typerep_of_t typerep_of_int));
    assert(check [1;2;6;5;4;3] (M.typerep_of_t typerep_of_int));
    assert(check_of_sexp [] (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_sexp [1;2;6;5;4;3]
      (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_t [] (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp));
    assert(check_of_t [1;2;6;5;4;3]
      (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp))

  TEST_UNIT =
    let module M = struct
      type 'a t = 'a array with typerep, sexp
    end in
    assert(check [||] (M.typerep_of_t typerep_of_int));
    assert(check [|1;2;6;5;4;3|] (M.typerep_of_t typerep_of_int));
    assert(check_of_sexp [||]
      (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_sexp [|1;2;6;5;4;3|]
      (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_t [||] (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp));
    assert(check_of_t [|1;2;6;5;4;3|]
      (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp))

  TEST_UNIT =
    let module M = struct
      type 'a t = 'a ref with typerep, sexp
    end in
    assert(check (ref 6) (M.typerep_of_t typerep_of_int));
    assert(check_of_sexp (ref 6)
      (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_t (ref 6) (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp))

  TEST_UNIT =
    let module M = struct
      type 'a t = 'a lazy_t with typerep, sexp
    end in
    let value = lazy 42 in
    let typerep = M.typerep_of_t typerep_of_int in
    let sexp_of_t = M.sexp_of_t sexp_of_int in
    let t_of_sexp = M.t_of_sexp int_of_sexp in
    let `generic sexp_of_x = S.sexp_of_t typerep in
    let `generic x_of_sexp = S.t_of_sexp typerep in
    assert (Int.equal (Lazy.force value) (Lazy.force (x_of_sexp (sexp_of_x value))));
    assert (Int.equal (Lazy.force value) (Lazy.force (x_of_sexp (sexp_of_t value))));
    assert (Int.equal (Lazy.force value) (Lazy.force (t_of_sexp (sexp_of_x value))));
  ;;


  TEST_UNIT =
    let module M = struct
      type 'a t = {foo:'a; bar:float} with typerep, sexp
    end in
    assert(check {M.foo=5;bar=43.25} (M.typerep_of_t typerep_of_int));
    assert(check_of_sexp {M.foo=5;bar=43.25}
      (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert( check_of_t {M.foo=5;bar=43.25}
      (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp))


  TEST_UNIT =
    let module M = struct
      type ('a, 'b) t = ('a * 'b) with typerep, sexp
    end in
    assert(check (5,45.67)
      (M.typerep_of_t typerep_of_int typerep_of_float));
    assert(check_of_sexp (5,45.67)
      (M.typerep_of_t typerep_of_int typerep_of_float)
      (M.sexp_of_t sexp_of_int sexp_of_float));
    assert(check_of_t (5,45.67)
      (M.typerep_of_t typerep_of_int typerep_of_float)
      (M.t_of_sexp int_of_sexp float_of_sexp))

  TEST_UNIT =
    let module M = struct
      type ('a, 'b, 'c) t = ('a * 'b * 'c) with typerep, sexp
    end in
    assert(check (5,45,3.14159)
      (M.typerep_of_t typerep_of_int typerep_of_int typerep_of_float));
    assert(check_of_sexp (5,45,3.14159)
      (M.typerep_of_t typerep_of_int typerep_of_int typerep_of_float)
      (M.sexp_of_t sexp_of_int sexp_of_int sexp_of_float));
    assert(check_of_t (5,45,3.14159)
      (M.typerep_of_t typerep_of_int typerep_of_int typerep_of_float)
      (M.t_of_sexp int_of_sexp int_of_sexp float_of_sexp))


  TEST_UNIT =
    let module M = struct
      type ('a, 'b, 'c, 'd) t = ('a * 'b * 'c * 'd) with typerep, sexp
    end in
    assert(check (5,45,3.14159,1.14159)
      (M.typerep_of_t typerep_of_int typerep_of_int typerep_of_float typerep_of_float));
    assert(check_of_sexp (5,45,3.14159,1.14159)
      (M.typerep_of_t typerep_of_int typerep_of_int typerep_of_float typerep_of_float)
      (M.sexp_of_t sexp_of_int sexp_of_int sexp_of_float sexp_of_float));
    assert(check_of_t (5,45,3.14159,1.14159)
      (M.typerep_of_t typerep_of_int typerep_of_int typerep_of_float typerep_of_float)
      (M.t_of_sexp int_of_sexp int_of_sexp float_of_sexp float_of_sexp))


  TEST_UNIT =
    let module M = struct
      type ('a, 'b, 'c, 'd, 'e) t = ('a * 'b * 'c * 'd * 'e) with typerep, sexp
    end in
    assert(check (5,45,3.14159,1.14159,"hi")
      (M.typerep_of_t typerep_of_int typerep_of_int
                      typerep_of_float typerep_of_float
                      typerep_of_string));
    assert(check_of_sexp (5,45,3.14159,1.14159,"hi")
      (M.typerep_of_t typerep_of_int typerep_of_int
                      typerep_of_float typerep_of_float
                      typerep_of_string)
      (M.sexp_of_t sexp_of_int sexp_of_int
                   sexp_of_float sexp_of_float
                   sexp_of_string));
    assert(check_of_t (5,45,3.14159,1.14159,"hi")
      (M.typerep_of_t typerep_of_int typerep_of_int
                      typerep_of_float typerep_of_float
                      typerep_of_string)
      (M.t_of_sexp int_of_sexp int_of_sexp
                  float_of_sexp float_of_sexp
                  string_of_sexp))


  TEST_UNIT =
    let module M = struct
      type 'a t =
        | Foo
        | Bar of 'a
        | Baz of int * int
        | Bee
        | Bax of (int * int)
        | Baa of 'a * 'a
        | Bab of ('a * 'a) with typerep, sexp
    end in
    (* sexprep serialize and deserialize *)
    assert(check M.Foo (M.typerep_of_t typerep_of_int));
    assert(check (M.Bar 651) (M.typerep_of_t typerep_of_int));
    assert(check (M.Bar "651") (M.typerep_of_t typerep_of_string));
    assert(check M.Bee (M.typerep_of_t typerep_of_bool));
    assert(check (M.Baz (651,54)) (M.typerep_of_t typerep_of_int));
    assert(check (M.Bax (651,54)) (M.typerep_of_t typerep_of_int));
    assert(check (M.Baa (651,54)) (M.typerep_of_t typerep_of_int));
    assert(check (M.Bab (651,54)) (M.typerep_of_t typerep_of_int));
    (* sexplib serialize; sexprep deserialize *)
    assert(check_of_sexp M.Foo (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_sexp (M.Bar 651) (M.typerep_of_t typerep_of_int) (M.sexp_of_t sexp_of_int));
    assert(check_of_sexp (M.Baz (651,54))
      (M.typerep_of_t typerep_of_int)
      (M.sexp_of_t sexp_of_int));
    assert(check_of_sexp (M.Bax (651,54))
      (M.typerep_of_t typerep_of_int)
      (M.sexp_of_t sexp_of_int));
    assert(check_of_sexp (M.Baa (651,54))
      (M.typerep_of_t typerep_of_int)
      (M.sexp_of_t sexp_of_int));
    assert(check_of_sexp (M.Bab (651,54))
      (M.typerep_of_t typerep_of_int)
      (M.sexp_of_t sexp_of_int));
    (* sexprep serialize; sexplib deserialize *)
    assert(check_of_t M.Foo (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp));
    assert(check_of_t (M.Bar 651) (M.typerep_of_t typerep_of_int) (M.t_of_sexp int_of_sexp));
    assert(check_of_t (M.Baz (651,54))
      (M.typerep_of_t typerep_of_int)
      (M.t_of_sexp int_of_sexp));
    assert(check_of_t (M.Bax (651,54))
      (M.typerep_of_t typerep_of_int)
      (M.t_of_sexp int_of_sexp));
    assert(check_of_t (M.Baa (651,54))
      (M.typerep_of_t typerep_of_int)
      (M.t_of_sexp int_of_sexp));
    assert(check_of_t (M.Bab (651,54))
      (M.typerep_of_t typerep_of_int)
      (M.t_of_sexp int_of_sexp))

  TEST_UNIT =
    let module M = struct
      type t =
        | Foo
        | Bar of int
        | Baz of int * int
        | Bax of (int * int)
        with typerep, sexp
    end in
    (* sexprep serialize and deserialize *)
    assert(check M.Foo M.typerep_of_t);
    assert(check (M.Bar 651) M.typerep_of_t);
    assert(check (M.Baz (651,54)) M.typerep_of_t);
    assert(check (M.Bax (651,54)) M.typerep_of_t);
    (* sexplib serialize; sexprep deserialize *)
    assert(check_of_sexp M.Foo M.typerep_of_t M.sexp_of_t);
    assert(check_of_sexp (M.Bar 651) M.typerep_of_t M.sexp_of_t);
    assert(check_of_sexp (M.Baz (651,54)) M.typerep_of_t M.sexp_of_t);
    assert(check_of_sexp (M.Bax (651,54)) M.typerep_of_t M.sexp_of_t);
    (* sexprep serialize; sexplib deserialize *)
    assert(check_of_t M.Foo M.typerep_of_t M.t_of_sexp);
    assert(check_of_t (M.Bar 651) M.typerep_of_t M.t_of_sexp);
    assert(check_of_t (M.Baz (651,54)) M.typerep_of_t M.t_of_sexp);
    assert(check_of_t (M.Bax (651,54)) M.typerep_of_t M.t_of_sexp)

  TEST_UNIT =
    let module M = struct
      type t = Leaf | Node of t * t with typerep,sexp
    end in
    let rec producer n =
      if n > 0
      then M.Node (producer (n-1), producer (n-1))
      else M.Leaf
    in
    let value = producer 15 in
    assert(check value M.typerep_of_t);
    assert(check_of_sexp value M.typerep_of_t M.sexp_of_t);
    assert(check_of_t value M.typerep_of_t M.t_of_sexp);

  TEST_UNIT =
    let module M = struct
      type 'a t = [ `Foo | `Bar of 'a ] with typerep
    end in
    let typerep = M.typerep_of_t typerep_of_unit in
    assert(check `Foo typerep) ;
    assert(check (`Bar ()) typerep)

  module Rev_option : sig
    type 'a t with typerep
    val of_option : 'a option -> 'a t
    val register : unit -> unit
  end = struct
    module T = struct
      type 'a t = 'a option with typerep(abstract)
    end
    include T

    let of_option t = t

    let t_of_sexp a_of_sexp sexp =
      match sexp with
      | Sexp.Atom ("enon" | "enoN") -> None
      | Sexp.List [el]
      | Sexp.List [el ; Sexp.Atom ("emos" | "emoS")] -> Some (a_of_sexp el)
      | _ -> assert false

    let sexp_of_t sexp_of_a a =
      match a with
      | None -> Sexp.Atom "enoN"
      | Some a -> Sexp.List [sexp_of_a a ; Sexp.Atom "emoS"]

    let register () =
    Type_struct.Generic.register1 (module struct
      include T
      let compute = fun t -> Type_struct.Option t
    end : Type_struct.Generic.S1);
    S.Of_sexp.register1 (module struct
      include T
      let compute = t_of_sexp
    end : S.Of_sexp.S1);
    S.Sexp_of.register1 (module struct
      include T
      let compute = sexp_of_t
    end : S.Sexp_of.S1)
  end

  TEST_UNIT =
    let module A = struct
      type t = int Rev_option.t with typerep
    end in
    assert (
      try
        ignore (check (Rev_option.of_option None) A.typerep_of_t);
        false
      with S.Of_sexp.Not_implemented _ -> true);
    Rev_option.register ();
    assert (check_typerep (Rev_option.of_option None) A.typerep_of_t);
    assert (check_typerep (Rev_option.of_option (Some 0)) A.typerep_of_t);
    assert (check_typerep (Rev_option.of_option (Some 42)) A.typerep_of_t);
  ;;

  TEST_UNIT =
    let module Rev_int : sig
      type t = int with typerep
      val register : unit -> unit
    end = struct
      module T = struct
        type t = int with typerep
      end
      include T

      exception Parse_error of Sexp.t with sexp

      let t_of_sexp = function
      | Sexp.Atom str ->
        let str' = String.copy str in
        let len = String.length str in
        let rec aux index index' =
          if index >= len then () else begin
            str'.[index] <- str.[index'];
            aux (succ index) (pred index')
          end
        in
        aux 0 (pred len);
        int_of_string str'

      | Sexp.List _ as sexp -> raise (Parse_error sexp)

      let register () =
        Type_struct.Generic.register0 (module struct
          include T
          let compute = Type_struct.Int
        end : Type_struct.Generic.S0);
        S.Of_sexp.register typerep_of_t t_of_sexp
    end in
    assert (check 421 Rev_int.typerep_of_t);
    Rev_int.register();
    let `generic t_of_sexp = S.t_of_sexp Rev_int.typerep_of_t in
    let `generic sexp_of_t = S.sexp_of_t Rev_int.typerep_of_t in
    assert (Int.equal 421 (t_of_sexp (sexp_of_t 124)));
  ;;

end
