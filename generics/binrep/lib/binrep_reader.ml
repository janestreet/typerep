open Bin_prot
open Typerep_extended.Std

let make_vtag_read_err () =
  let str_name = "Reader.unsafe_vtag_read" in
  fun _buf ~pos_ref:_ -> Bin_prot.Common.raise_variant_wrong_type str_name

module Computation_impl = struct
  type 'a t = 'a Type_class.reader
  include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

  let int        = Type_class.bin_reader_int
  let int32      = Type_class.bin_reader_int32
  let int64      = Type_class.bin_reader_int64
  let nativeint  = Type_class.bin_reader_nativeint
  let char       = Type_class.bin_reader_char
  let float      = Type_class.bin_reader_float
  let string     = Type_class.bin_reader_string
  let bool       = Type_class.bin_reader_bool
  let unit       = Type_class.bin_reader_unit
  let option     = Type_class.bin_reader_option
  let list       = Type_class.bin_reader_list
  let array      = Type_class.bin_reader_array
  let lazy_t     = Type_class.bin_reader_lazy
  let ref_       = Type_class.bin_reader_ref

  (* bin_io does *NOT* support serialization of functions *)
  let function_ _ = assert false

  let tuple2 ra rb =
    (* beware of (expr1, expr2) notation, expr1 has to be executed before expr2
       thus, we use there let a = expr1 in let b = expr2 in a, b *)
    let read buf ~pos_ref =
      let a = ra.Type_class.read buf ~pos_ref in
      let b = rb.Type_class.read buf ~pos_ref in
      (a,b)
    in
    let vtag_read = make_vtag_read_err () in
    { Type_class. read ; vtag_read }

  let tuple3 ra rb rc =
    let read buf ~pos_ref =
      let a = ra.Type_class.read buf ~pos_ref in
      let b = rb.Type_class.read buf ~pos_ref in
      let c = rc.Type_class.read buf ~pos_ref in
      (a,b,c)
    in
    let vtag_read = make_vtag_read_err () in
    { Type_class. read ; vtag_read }

  let tuple4 ra rb rc rd =
    let read buf ~pos_ref =
      let a = ra.Type_class.read buf ~pos_ref in
      let b = rb.Type_class.read buf ~pos_ref in
      let c = rc.Type_class.read buf ~pos_ref in
      let d = rd.Type_class.read buf ~pos_ref in
      (a,b,c,d)
    in
    let vtag_read = make_vtag_read_err () in
    { Type_class. read ; vtag_read }

  let tuple5 ra rb rc rd re =
    let read buf ~pos_ref =
      let a = ra.Type_class.read buf ~pos_ref in
      let b = rb.Type_class.read buf ~pos_ref in
      let c = rc.Type_class.read buf ~pos_ref in
      let d = rd.Type_class.read buf ~pos_ref in
      let e = re.Type_class.read buf ~pos_ref in
      (a,b,c,d,e)
    in
    let vtag_read = make_vtag_read_err () in
    { Type_class. read ; vtag_read }

  let record record =
    let length = Record.length record in
    let read buf ~pos_ref =
      let current_field_index = ref 0 in
      let s = ref "" in
      let get field =
        let index = Field.index field in
        let label = Field.label field in
        s := !s ^ (Printf.sprintf "read %S index %d\n" label index);
        if index <> !current_field_index then (
          s := !s^ (Printf.sprintf "current=%d\n" !current_field_index);
          raise (Failure !s)
        );
        current_field_index := (succ index) mod length;
        (Field.traverse field).Type_class.read buf ~pos_ref
      in
      let t = Record.create record { Record.get } in
      if current_field_index.contents <> 0 then assert false;
      t
    in
    let vtag_read = make_vtag_read_err () in
    { Type_class. read ; vtag_read }

  let variant variant =
    let length = Variant.length variant in
    let is_polymorphic = Variant.is_polymorphic variant in
    let repr_reader =
      if is_polymorphic
      then
        Bin_prot.Read.bin_read_variant_int
      else
        if length < 256
        then
          Bin_prot.Read.bin_read_int_8bit
        else
          Bin_prot.Read.bin_read_int_16bit
    in
    let read_with_repr =
      let extract_key =
        if is_polymorphic
        then Tag.ocaml_repr
        else Tag.index
      in
      let tags = Flat_map.Flat_int_map.init length ~f:(fun index ->
        match Variant.tag variant index with
        | (Variant.Tag tag) as data -> extract_key tag, data)
      in
      (fun buf ~pos_ref repr ->
        match Flat_map.Flat_int_map.find tags repr with
        | Some (Variant.Tag tag) -> begin
          match Tag.create tag with
          | Tag.Const const -> const
          | Tag.Args create ->
            let value = (Tag.traverse tag).Type_class.read buf ~pos_ref in
            create value
        end
        | None ->
          Bin_prot.Common.raise_read_error
            (Bin_prot.Common.ReadError.Sum_tag "Binrep.Reader.variant") !pos_ref)
    in
    let vtag_read buf ~pos_ref vint =
      if is_polymorphic
      then
        read_with_repr buf ~pos_ref vint
      else
        Bin_prot.Common.raise_variant_wrong_type "Binrep.Reader.variant" !pos_ref
    in
    let read buf ~pos_ref =
      let repr = repr_reader buf ~pos_ref in
      read_with_repr buf ~pos_ref repr
    in
    { Type_class. read ; vtag_read }

  module Named = struct
    module Reader_named = Type_generic.Make_named_for_closure(struct
      open Bin_prot.Common
      type 'a input = buf
      type 'a output = pos_ref:pos ref -> 'a
      type 'a t = 'a Read.reader
    end)

    module Vtag_reader_named = Type_generic.Make_named_for_closure(struct
      open Bin_prot.Common
      type 'a input = buf
      type 'a output = pos_ref:pos ref -> (int -> 'a)
      type 'a t = (int -> 'a) Read.reader
    end)

    module Context = struct
      type t = {
        reader_ctx      : Reader_named.Context.t ;
        vtag_reader_cxt : Vtag_reader_named.Context.t ;
      }
      let create () = {
        reader_ctx      = Reader_named.Context.create () ;
        vtag_reader_cxt = Vtag_reader_named.Context.create () ;
      }
    end

    type 'a t = {
      reader_named      : 'a Reader_named.t;
      vtag_reader_named : 'a Vtag_reader_named.t;
    }

    let init ctx name = let open Context in {
      reader_named =
        Reader_named.init ctx.reader_ctx name;
      vtag_reader_named =
        Vtag_reader_named.init ctx.vtag_reader_cxt name;
    }

    let get_wip_computation t = {
      Type_class.
      read =
        Reader_named.get_wip_computation t.reader_named;
      vtag_read =
        Vtag_reader_named.get_wip_computation t.vtag_reader_named;
    }

    let set_final_computation t comp = {
      Type_class.
      read =
        Reader_named.set_final_computation
          t.reader_named comp.Type_class.read;
      vtag_read =
        Vtag_reader_named.set_final_computation
          t.vtag_reader_named comp.Type_class.vtag_read;
    }

    let share _ = true
  end
end

include Type_generic.Make(struct
  include Computation_impl
  let name = "bin_reader"
  let required = [
    Type_struct.Generic.ident;
    Binrep_sizer.ident;
    Binrep_writer.ident;
  ]
end)
