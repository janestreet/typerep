open Bin_prot
open Bin_prot.Common
open Typerep_extended.Std

module Computation_impl = struct

  type 'a t = buf -> pos_ref : pos_ref -> unit
  type 'a size_reader = 'a t
  include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

  let check_next = Common.check_next

  let static buf ~pos_ref i =
    let next = !pos_ref + i in
    check_next buf next;
    pos_ref := next;
  ;;

  let int buf ~pos_ref =
    let _ = Read.bin_read_int buf ~pos_ref in
    ()

  let int32 buf ~pos_ref =
    let _ = Read.bin_read_int32 buf ~pos_ref in
    ()

  let int64 buf ~pos_ref =
    let _ = Read.bin_read_int64 buf ~pos_ref in
    ()

  let nativeint buf ~pos_ref =
    let _ = Read.bin_read_nativeint buf ~pos_ref in
    ()

  let char buf ~pos_ref = static buf ~pos_ref 1

  let float buf ~pos_ref =
    let _ = Read.bin_read_float buf ~pos_ref in
    ()

  let string buf ~pos_ref =
    let start_pos = !pos_ref in
    let len = (Read.bin_read_nat0 buf ~pos_ref :> int) in
    if len > Sys.max_string_length then
      raise_read_error ReadError.String_too_long start_pos;
    let next = !pos_ref + len in
    check_next buf next;
    pos_ref := next;
  ;;

  let bool buf ~pos_ref = static buf ~pos_ref 1

  let unit buf ~pos_ref = static buf ~pos_ref 1

  let option read_el buf ~pos_ref =
    let pos = safe_get_pos buf pos_ref in
    match buf.{pos} with
    | '\000' ->
      pos_ref := pos + 1
    | '\001' ->
      pos_ref := pos + 1;
      read_el buf ~pos_ref;
    | _ -> raise_read_error ReadError.Option_code pos

  let list read_el buf ~pos_ref =
    let len = (Read.bin_read_nat0 buf ~pos_ref :> int) in
    for _i = 1 to len do
      read_el buf ~pos_ref
    done

  let array = list

  let lazy_t read_el = read_el

  let ref_ read_el = read_el

  (* bin_io does *NOT* support serialization of functions *)
  let function_ _ = assert false

  let tuple2 read_a read_b =
    (fun buf ~pos_ref ->
      read_a buf ~pos_ref;
      read_b buf ~pos_ref;
    )

  let tuple3 read_a read_b read_c =
    (fun buf ~pos_ref ->
      read_a buf ~pos_ref;
      read_b buf ~pos_ref;
      read_c buf ~pos_ref;
    )

  let tuple4 read_a read_b read_c read_d =
    (fun buf ~pos_ref ->
      read_a buf ~pos_ref;
      read_b buf ~pos_ref;
      read_c buf ~pos_ref;
      read_d buf ~pos_ref;
    )

  let tuple5 read_a read_b read_c read_d read_e =
    (fun buf ~pos_ref ->
      read_a buf ~pos_ref;
      read_b buf ~pos_ref;
      read_c buf ~pos_ref;
      read_d buf ~pos_ref;
      read_e buf ~pos_ref;
    )

  let record record buf ~pos_ref =
    Record.fold record ~init:() ~f:(fun () -> function
    | Record.Field field -> Field.traverse field buf ~pos_ref
    )

  let variant variant =
    let length = Variant.length variant in
    let is_polymorphic = Variant.is_polymorphic variant in
    let extract_key  =
      if is_polymorphic
      then Tag.ocaml_repr
      else Tag.index
    in
    let tags = Flat_map.Flat_int_map.init length ~f:(fun index ->
      match Variant.tag variant index with
      | (Variant.Tag tag) as data -> extract_key tag, data
    ) in
    let repr_reader =
      if is_polymorphic
      then Read.bin_read_variant_int
      else
        if length < 256
        then
          Read.bin_read_int_8bit
        else
          Read.bin_read_int_16bit
    in
    let bin_size_t buf ~pos_ref =
      let repr = repr_reader buf ~pos_ref in
      match Flat_map.Flat_int_map.find tags repr with
      | Some (Variant.Tag tag) ->
        let arity = Tag.arity tag in
        if arity <> 0 then Tag.traverse tag buf ~pos_ref
      | None ->
        Bin_prot.Common.raise_read_error
          (Bin_prot.Common.ReadError.Sum_tag "Bin_read_size.variant") !pos_ref
    in
    bin_size_t

  module Named = Type_generic.Make_named_for_closure(struct
    type 'a input = buf
    type 'a output = pos_ref: pos_ref -> unit
    type 'a t = 'a size_reader
  end)
end

include Type_generic.Make(struct
  include Computation_impl
  let name = "bin_size_reader"
  let required = [
    Type_struct.Generic.ident;
    Binrep_sizer.ident;
    Binrep_reader.ident;
    Binrep_writer.ident;
  ]
end)

module Children = struct
  type 'a reader =  buf -> pos_ref : pos_ref -> 'a

  let read_option buf ~pos_ref =
    let pos = safe_get_pos buf pos_ref in
    match buf.{pos} with
    | '\000' ->
      pos_ref := pos + 1;
      `none
    | '\001' ->
      pos_ref := pos + 1;
      `some
    | _ -> raise_read_error ReadError.Option_code pos

  let read_sequence buf ~pos_ref =
    let len = (Read.bin_read_nat0 buf ~pos_ref :> int) in
    len

  let read_usual_variant buf ~pos_ref =
    Read.bin_read_int_8bit buf ~pos_ref

  let read_polymorphic_variant buf ~pos_ref =
    Read.bin_read_variant_int buf ~pos_ref
end
