open Typerep_core.Std
open Pre_core.Std

module SC = struct
  include Sexplib.Conv
  let of_sexp_error what sexp = raise (Of_sexp_error (Failure what, sexp))
  let quadruple_of_sexp a_of_sexp b_of_sexp c_of_sexp d_of_sexp =
    function
    | Sexp.List [a;b;c;d] ->
          a_of_sexp a
        , b_of_sexp b
        , c_of_sexp c
        , d_of_sexp d
    | (Sexp.List _) as sexp -> of_sexp_error
      "quadruple_of_sexp: list must contain exactly four elements only" sexp
    | (Sexp.Atom _) as sexp -> of_sexp_error "quadruple_of_sexp: list needed" sexp

  let quintuple_of_sexp a_of_sexp b_of_sexp c_of_sexp d_of_sexp e_of_sexp =
    function
    | Sexp.List [a;b;c;d;e] ->
          a_of_sexp a
        , b_of_sexp b
        , c_of_sexp c
        , d_of_sexp d
        , e_of_sexp e
    | (Sexp.List _) as sexp -> of_sexp_error
      "quintuple_of_sexp: list must contain exactly five elements only" sexp
    | (Sexp.Atom _) as sexp -> of_sexp_error "quintuple_of_sexp: list needed" sexp

  let sexp_of_quadruple sexp_of_a sexp_of_b sexp_of_c sexp_of_d (a,b,c,d) =
    let sexp_a = sexp_of_a a in
    let sexp_b = sexp_of_b b in
    let sexp_c = sexp_of_c c in
    let sexp_d = sexp_of_d d in
    Sexp.List [sexp_a; sexp_b; sexp_c; sexp_d]

  let sexp_of_quintuple
    sexp_of_a sexp_of_b sexp_of_c sexp_of_d sexp_of_e (a,b,c,d,e) =
    let sexp_a = sexp_of_a a in
    let sexp_b = sexp_of_b b in
    let sexp_c = sexp_of_c c in
    let sexp_d = sexp_of_d d in
    let sexp_e = sexp_of_e e in
    Sexp.List [sexp_a; sexp_b; sexp_c; sexp_d; sexp_e]
end

module Of_sexp = struct
  exception Type_mismatch of string * Sexp.t

  module Computation_impl = struct
    type 'a t = Sexp.t -> 'a
    include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

    let int = SC.int_of_sexp
    let int32 = SC.int32_of_sexp
    let int64 = SC.int64_of_sexp
    let nativeint = SC.nativeint_of_sexp
    let char = SC.char_of_sexp
    let float = SC.float_of_sexp
    let string = SC.string_of_sexp
    let bool = SC.bool_of_sexp
    let unit = SC.unit_of_sexp
    let option contents_of_sexp = SC.option_of_sexp contents_of_sexp
    let list contents_of_sexp = SC.list_of_sexp contents_of_sexp
    let array contents_of_sexp = SC.array_of_sexp contents_of_sexp
    let lazy_t contents_of_sexp = SC.lazy_t_of_sexp contents_of_sexp
    let ref_ contents_of_sexp = SC.ref_of_sexp contents_of_sexp
    let function_ _arg_of_sexp _return_of_sexp = SC.fun_of_sexp
    let tuple2 = SC.pair_of_sexp
    let tuple3 = SC.triple_of_sexp
    let tuple4 = SC.quadruple_of_sexp
    let tuple5 = SC.quintuple_of_sexp

    let record record = fun sexp ->
      let fail () = raise (Type_mismatch ("Record", sexp)) in
      match sexp with
      | Sexp.List sexp_properties -> begin
        let properties = lazy (
          let seen = String.Hash_set.create () in
          Flat_map.Flat_string_map.of_alist (
            List.rev_map sexp_properties ~f:(function
            | Sexp.List [Sexp.Atom name; sexp_value] ->
              if String.Hash_set.mem seen name then fail ();
              String.Hash_set.add seen name;
              (name, sexp_value)
            | _ -> fail ()
            )
          ))
        in
        let get field =
          let label = Field.label field in
          let index = Field.index field in
          let sexp_value =
            match List.nth sexp_properties index with
            | Some (Sexp.List [Sexp.Atom sexp_name; sexp_value]) ->
              if String.equal sexp_name label then sexp_value
              else begin
                match Flat_map.Flat_string_map.find (Lazy.force properties) label with
                | Some x -> x
                | None -> fail ()
              end
            | _ -> fail ()
          in
          Field.traverse field sexp_value
        in
        Record.create record { Record.get }
      end
      | _ -> fail ()

    let variant variant =
      let tag_by_label =
        let f index =
          match Variant.tag variant index with
          | (Variant.Tag tag) as data -> Tag.label tag, data
        in
        Flat_map.Flat_string_map.init (Variant.length variant) ~f
      in
      let t_of_sexp sexp =
        let fail () = raise (Type_mismatch ("Variant", sexp)) in
        match sexp with
        | Sexp.Atom label -> begin
          match Flat_map.Flat_string_map.find tag_by_label label with
          | Some (Variant.Tag tag) -> begin
            match Tag.create tag with
            | Tag.Const const -> const
            | Tag.Args _ -> fail ()
          end
          | _ -> fail ()
        end
        | Sexp.List ((Sexp.Atom label)::sexps) -> begin
          match Flat_map.Flat_string_map.find tag_by_label label with
          | Some (Variant.Tag tag) -> begin
            match Tag.create tag with
            | Tag.Args create ->
              let arity = Tag.arity tag in
              let sexp_value =
                if arity = 1
                then match sexps with
                | [sexp] -> sexp
                | _ -> fail ()
                else Sexp.List sexps
              in
              create (Tag.traverse tag sexp_value)
            | Tag.Const _ -> fail ()
          end
          | _ -> fail ()
        end
        | _ -> fail ()
      in
      t_of_sexp

    module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = Sexp.t
      type 'a output = 'a
      type 'a t = Sexp.t -> 'a
    end)
  end

  include Type_generic.Make(struct
    include Computation_impl
    let name = "of_sexp"
    let required = [ Type_struct.Generic.ident ]
  end)

end

module Sexp_of = struct

  module Computation_impl = struct
    type 'a t = 'a -> Sexp.t
    include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

    let int = SC.sexp_of_int
    let int32 = SC.sexp_of_int32
    let int64 = SC.sexp_of_int64
    let nativeint = SC.sexp_of_nativeint
    let char = SC.sexp_of_char
    let float = SC.sexp_of_float
    let string = SC.sexp_of_string
    let bool = SC.sexp_of_bool
    let function_ _sexp_of_arg _sexp_of_ret = SC.sexp_of_fun
    let unit = SC.sexp_of_unit
    let option = SC.sexp_of_option
    let ref_ = SC.sexp_of_ref
    let lazy_t = SC.sexp_of_lazy_t
    let list = SC.sexp_of_list
    let array = SC.sexp_of_array
    let tuple2 = SC.sexp_of_pair
    let tuple3 = SC.sexp_of_triple
    let tuple4 = SC.sexp_of_quadruple
    let tuple5 = SC.sexp_of_quintuple

    let record record =
      (* preallocation of atoms *)
      let atoms = Array.init (Record.length record) ~f:(fun index ->
        match Record.field record index with
        | Record.Field field -> Sexp.Atom (Field.label field)
      ) in
      fun value ->
        let rec aux acc index =
          if index < 0 then Sexp.List acc
          else
            let field =
              match Record.field record index with
              | Record.Field field ->
                let field_value = Field.traverse field (Field.get field value) in
                let index = Field.index field in
                Sexp.List [ atoms.(index) ; field_value ]
            in
            aux (field::acc) (pred index)
        in
        aux [] (pred (Record.length record))

    let variant variant =
      (* preallocation of atoms *)
      let atoms = Array.init (Variant.length variant) ~f:(fun index ->
        match Variant.tag variant index with
        | Variant.Tag tag -> Sexp.Atom (Tag.label tag)
      ) in
      fun value ->
        match Variant.value variant value with
        | Variant.Value (tag, args) ->
          let index = Tag.index tag in
          let arity = Tag.arity tag in
          let atom = atoms.(index) in
          match arity with
          | 0 -> atom
          | 1 -> Sexp.List [ atom ; Tag.traverse tag args ]
          | _ ->
            (* this might be a cause of this being slower because of this [Sexp.List
               sexps] temporary cons/decons I'm not sure how to get rid of it though *)
            match Tag.traverse tag args with
            | Sexp.List sexps -> Sexp.List (atom::sexps)
            | _ -> assert false

    module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = 'a
      type 'a output = Sexp.t
      type 'a t = 'a -> Sexp.t
    end)

  end

  include Type_generic.Make(struct
    include Computation_impl
    let name = "sexp_of"
    let required = [ Type_struct.Generic.ident ]
  end)

end

let t_of_sexp = Of_sexp.of_typerep
let sexp_of_t = Sexp_of.of_typerep

module Make_sexpable(X:Typerepable.S0) = struct
  type t = X.t
  let `generic sexp_of_t = sexp_of_t X.typerep_of_t
  let `generic t_of_sexp = t_of_sexp X.typerep_of_t
end

let make_sexpable (type a) (typerep_of_t : a Typerep.t) =
  let module M = Make_sexpable(struct
    type t = a
    let typerep_of_t = typerep_of_t
    let typename_of_t = Typerep.typename_of_t typerep_of_t
  end) in
  (module M : Sexpable.S with type t = a)

module Tagged = struct

  module Of_sexp = Tagged_generic.Make_input(Sexp)(Of_sexp.Computation)
  module Sexp_of = Tagged_generic.Make_output(Sexp)(Sexp_of.Computation)

  let t_of_sexp = Of_sexp.of_typestruct
  let sexp_of_t = Sexp_of.of_typestruct

  module Make_sexpable(X:sig val typestruct_of_t : Type_struct.t end) = struct
    type t = Tagged.t
    let `generic sexp_of_t = sexp_of_t X.typestruct_of_t
    let `generic t_of_sexp = t_of_sexp X.typestruct_of_t
  end

  let make_sexpable typestruct_of_t =
    let module M = Make_sexpable(struct
      let typestruct_of_t = typestruct_of_t
    end) in
    (module M : Sexpable.S with type t = Tagged.t)
end
