open Core.Std
open Typerep_experimental.Std

let print_str str =
  let sexp = Type_struct.sexp_of_t str in
  print_endline (Sexp.to_string_hum sexp)

module S = Type_struct

(* Tests about Type_struct diffing *)
TEST_MODULE = struct

  let check exp rep1 rep2 =
    let s1 = S.of_typerep rep1 in
    let s2 = S.of_typerep rep2 in
    let diff = S.Diff.compute s1 s2 in
    let diff = S.Diff.sexp_of_t diff in
    let exp = try Sexp.of_string exp with _ -> Sexp.Atom exp in
    if diff = exp then true else begin
      print_endline "s1:";
      print_str s1;
      print_endline "s2:";
      print_str s2;
      print_endline "expected diff";
      print_endline (Sexp.to_string_hum exp);
      print_endline "computed diff";
      print_endline (Sexp.to_string_hum diff);
      false
    end

  TEST_UNIT =
    let module A = struct
      type t = int with typerep
    end in
    let module B = struct
      type t = float with typerep
    end in
    let exp = "((() (Update Int Float)))" in
    assert (check exp A.typerep_of_t B.typerep_of_t)

  TEST_UNIT =
    let module A = struct
      type t = {
        a : int;
      } with typerep
    end in
    let module B = struct
      type t = {
        a : int;
        b : float;
      } with typerep
    end in
    let exp = "((() (Add_field (((label b) (index 1)) Float))))" in
    assert (check exp A.typerep_of_t B.typerep_of_t);
    assert (not (S.Diff.is_bin_prot_subtype
                   ~subtype:(S.of_typerep A.typerep_of_t)
                   ~supertype:(S.of_typerep B.typerep_of_t)));
    let exp = "((() (Remove_field (((label b) (index 1)) Float))))" in
    assert (check exp B.typerep_of_t A.typerep_of_t);

  TEST_UNIT =
    let module A = struct
      type t =
      | V1 of int
      | V2 of int
      | V3 of int
      with typerep
    end in
    let module B = struct
      type t =
      | V1 of int
      | V2bis of float
      | V3 of int
      with typerep
    end in
    let exp =
"((()
  (Update_variant
   (((label V2)    (index 1) (ocaml_repr 1)) (Int))
   (((label V2bis) (index 1) (ocaml_repr 1)) (Float))
 )))"
    in
    assert (check exp A.typerep_of_t B.typerep_of_t);

  TEST_UNIT =
    let module A = struct
      type t =
      | V1 of int
      | V3 of int
      with typerep
    end in
    let module B = struct
      type t =
      | V1 of int
      | V2 of int * float
      | V3 of int
      with typerep
    end in
    let exp = "((() (Add_variant (Break ((label V2) (index 1) (ocaml_repr 1)) (Int Float)))))" in
    assert (check exp A.typerep_of_t B.typerep_of_t);
    let exp = "((() (Remove_variant (((label V2) (index 1) (ocaml_repr 1)) (Int Float)))))" in
    assert (check exp B.typerep_of_t A.typerep_of_t);
    assert (not ( (* order matters for non polymorphic variants *)
      S.Diff.is_bin_prot_subtype
        ~subtype:(S.of_typerep A.typerep_of_t)
        ~supertype:(S.of_typerep B.typerep_of_t)));

  TEST_UNIT =
    let module A = struct
      type t = [
      | `V3 of int
      | `V1 of int
      ] with typerep
    end in
    let module B = struct
      type t = [
      | `V1 of int
      | `V2 of int * float
      | `V3 of int
      ] with typerep
    end in
    let exp =
"((()(Add_variant (Backward_compatible ((label V2) (index 1) (ocaml_repr 19228)) ((Tuple (Int Float)))))))"
    in
    assert (check exp A.typerep_of_t B.typerep_of_t);
    let exp = "((() (Remove_variant (((label V2)  (index 1) (ocaml_repr 19228)) ((Tuple (Int Float)))))))"
    in
    assert (check exp B.typerep_of_t A.typerep_of_t);
    assert (S.Diff.is_bin_prot_subtype
              ~subtype:(S.of_typerep A.typerep_of_t)
              ~supertype:(S.of_typerep B.typerep_of_t));
end
