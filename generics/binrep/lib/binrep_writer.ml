open Bin_prot
open Typerep_extended.Std

module Computation_impl = struct
  type 'a t = 'a Type_class.writer
  include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

  let int       = Type_class.bin_writer_int
  let int32     = Type_class.bin_writer_int32
  let int64     = Type_class.bin_writer_int64
  let nativeint = Type_class.bin_writer_nativeint
  let char      = Type_class.bin_writer_char
  let float     = Type_class.bin_writer_float
  let string    = Type_class.bin_writer_string
  let bool      = Type_class.bin_writer_bool
  let unit      = Type_class.bin_writer_unit
  let option    = Type_class.bin_writer_option
  let list      = Type_class.bin_writer_list
  let array     = Type_class.bin_writer_array
  let lazy_t    = Type_class.bin_writer_lazy
  let ref_      = Type_class.bin_writer_ref

  (* bin_io does *NOT* support serialization of functions *)
  let function_ _ = assert false

  let tuple2 wa wb =
    let module TC = Type_class in
    let size (a,b) = wa.TC.size a + wb.TC.size b in
    let write buf ~pos (a,b) =
      let pos = wa.TC.write buf ~pos a
      in wb.TC.write buf ~pos b in
    { TC. size ; write }

  let tuple3 wa wb wc =
    let module TC = Type_class in
    let size (a,b,c) = wa.TC.size a + wb.TC.size b + wc.TC.size c in
    let write buf ~pos (a,b,c) =
      let pos = wa.TC.write buf ~pos a in
      let pos = wb.TC.write buf ~pos b in
      wc.TC.write buf ~pos c
    in
    { TC. size ; write }

  let tuple4 wa wb wc wd =
    let module TC = Type_class in
    let size (a,b,c,d) = wa.TC.size a + wb.TC.size b + wc.TC.size c + wd.TC.size d in
    let write buf ~pos (a,b,c,d) =
      let pos = wa.TC.write buf ~pos a in
      let pos = wb.TC.write buf ~pos b in
      let pos = wc.TC.write buf ~pos c in
      wd.TC.write buf ~pos d
    in
    { TC. size ; write }

  let tuple5 wa wb wc wd we =
    let module TC = Type_class in
    let size (a,b,c,d,e) =
      wa.TC.size a + wb.TC.size b + wc.TC.size c + wd.TC.size d + we.TC.size e
    in
    let write buf ~pos (a,b,c,d,e) =
      let pos = wa.TC.write buf ~pos a in
      let pos = wb.TC.write buf ~pos b in
      let pos = wc.TC.write buf ~pos c in
      let pos = wd.TC.write buf ~pos d in
      we.TC.write buf ~pos e
    in
    { TC. size ; write }

  let record record =
    let size value =
      let aggregate_fields_size acc = function
        | Record.Field field ->
          acc + (Field.traverse field).Type_class.size (Field.get field value)
      in
      Record.fold record ~init:0 ~f:aggregate_fields_size
    in
    let write buf ~pos value =
      let write_field pos = function
        | Record.Field field ->
          (Field.traverse field).Type_class.write buf ~pos (Field.get field value)
      in
      Record.fold record ~init:pos ~f:write_field
    in
    { Type_class. size ; write }

  let variant variant =
    let len = Variant.length variant in
    let tag_size =
      if Variant.is_polymorphic variant
      then 4
      else
        if len < 256 then 1 else 2
    in
    let size value =
      match Variant.value variant value with
      | Variant.Value (tag, args) ->
        let arity = Tag.arity tag in
        if arity = 0
        then tag_size
        else tag_size + (Tag.traverse tag).Type_class.size args
    in
    let write buf ~pos value =
      match Variant.value variant value with
      | Variant.Value (tag, args) ->
        let pos =
          if Variant.is_polymorphic variant
          then
            let ocaml_repr = Tag.ocaml_repr tag in
            Bin_prot.Write.bin_write_variant_int buf ~pos ocaml_repr
          else
            let index = Tag.index tag in
            if len < 256
            then
              Bin_prot.Write.bin_write_int_8bit buf ~pos index
            else
              Bin_prot.Write.bin_write_int_16bit buf ~pos index
        in
        if Tag.arity tag = 0
        then pos
        else (Tag.traverse tag).Type_class.write buf ~pos args
    in
    { Type_class. size ; write }

  module Named = struct
    open Bin_prot

    module Writer_named = Type_generic.Make_named_for_closure(struct
      type 'a input = Common.buf
      type 'a output = pos:Common.pos -> 'a -> Common.pos
      type 'a t = 'a Write.writer
    end)

    module Context = struct
      type t = {
        sizer  : Binrep_sizer.Computation.Named.Context.t ;
        writer : Writer_named.Context.t ;
      }
      let create () = {
        sizer  = Binrep_sizer.Computation.Named.Context.create () ;
        writer = Writer_named.Context.create () ;
      }
    end

    type 'a t = {
      sizer_named  : 'a Binrep_sizer.Computation.Named.t;
      writer_named : 'a Writer_named.t;
    }

    let init ctx name = let open Context in {
      sizer_named  = Binrep_sizer.Computation.Named.init ctx.sizer name ;
      writer_named = Writer_named.init ctx.writer name;
    }

    let get_wip_computation { sizer_named ; writer_named } = {
      Type_class.
      size  = Binrep_sizer.Computation.Named.get_wip_computation sizer_named ;
      write = Writer_named.get_wip_computation writer_named ;
    }

    let set_final_computation {sizer_named;writer_named} comp =
      let open Type_class in {
        size =
          Binrep_sizer.Computation.Named.set_final_computation sizer_named comp.size;
        write =
          Writer_named.set_final_computation writer_named comp.write;
      }

    let share _ = true
  end
end

include Type_generic.Make(struct
  include Computation_impl
  let name = "bin_writer"
  let required = [
    Type_struct.Generic.ident;
    Binrep_sizer.ident;
  ]
end)
