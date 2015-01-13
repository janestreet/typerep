open! Core_kernel.Std
open Typerep_extended.Std

type buf = Bin_prot.Common.buf
type pos_ref = Bin_prot.Common.pos_ref

module Type_class = Bin_prot.Type_class

module Sizer           = Binrep_sizer
module Writer          = Binrep_writer
module Reader          = Binrep_reader
module Size_reader     = Binrep_size_reader

let bin_size_t         = Sizer.of_typerep
let bin_writer_t       = Writer.of_typerep
let bin_reader_t       = Reader.of_typerep
let bin_size_reader_t  = Size_reader.of_typerep

type 'a size_reader = buf -> pos_ref : pos_ref -> unit

module Make_binable(X:sig type t val typerep_of_t : t Typerep.t end) = struct
  type t = X.t
  let bin_size_t =
    let `generic clos = bin_size_t X.typerep_of_t in
    clos

  let bin_writer_t =
    let `generic clos = bin_writer_t X.typerep_of_t in
    clos

  let bin_reader_t =
    let `generic clos = bin_reader_t X.typerep_of_t in
    clos

  let bin_write_t = bin_writer_t.Type_class.write
  let bin_read_t = bin_reader_t.Type_class.read
  let __bin_read_t__ = bin_reader_t.Type_class.vtag_read
  let bin_t =
    { Type_class.
      reader = bin_reader_t;
      writer = bin_writer_t; }
end

let make_binable (type a) (typerep_of_t : a Typerep.t) =
  let module M = Make_binable(struct
    type t = a
    let typerep_of_t = typerep_of_t
  end) in
  (module M : Binable.S with type t = a)

module Tagged = struct
  let bin_size_t =
    let module Sizer =
      Tagged_generic.Make_output(struct type t = int end)(Sizer.Computation)
    in
    Sizer.of_typestruct

  let bin_size_reader_t =
    let module Size_reader =
      Tagged_generic.Make_reader(struct
        type 'a t = buf -> pos_ref:pos_ref -> unit
        let make _ fct = fct
      end)(Size_reader.Computation)
    in
    Size_reader.of_typestruct

  module Tagged_writer = struct
    module Computation = Writer.Computation
    module Builder = struct
      type 'a t = 'a Computation.t
      let make to_typed writer = { Type_class.
        size = (fun untyped -> writer.Type_class.size (to_typed untyped));
        write =
          (fun buf ~pos untyped -> writer.Type_class.write buf ~pos (to_typed untyped));
      }
    end

    include Tagged_generic.Make_writer(Builder)(Computation)
  end

  module Tagged_reader = struct
    module Computation = Reader.Computation
    module Builder = struct
      type 'a t = 'a Computation.t

      let make to_typed reader = { Type_class.
        read = (fun buf ~pos_ref -> to_typed (reader.Type_class.read buf ~pos_ref));
        vtag_read = (fun buf ~pos_ref i ->
          to_typed (reader.Type_class.vtag_read buf ~pos_ref i));
      }
    end

    include Tagged_generic.Make_reader(Builder)(Computation)
  end

  let bin_write_t str =
    let `generic writer = Tagged_writer.of_typestruct str in
    `generic writer.Type_class.write

  let bin_read_t str =
    let `generic reader = Tagged_reader.of_typestruct str in
    `generic reader.Type_class.read

  let __bin_read_t__ str =
    let `generic reader = Tagged_reader.of_typestruct str in
    `generic reader.Type_class.vtag_read

  let bin_writer_t str = Tagged_writer.of_typestruct str

  let bin_reader_t str = Tagged_reader.of_typestruct str

  let bin_t str =
    let `generic writer = Tagged_writer.of_typestruct str in
    let `generic reader = Tagged_reader.of_typestruct str in
    `generic { Type_class. reader ; writer }

  module Make_binable(X:sig val typestruct_of_t : Type_struct.t end) = struct
    type t = Tagged.t
    let bin_size_t =
      let `generic clos = bin_size_t X.typestruct_of_t in
      clos

    let bin_writer_t =
      let `generic clos = bin_writer_t X.typestruct_of_t in
      clos

    let bin_reader_t =
      let `generic clos = bin_reader_t X.typestruct_of_t in
      clos

    let bin_write_t = bin_writer_t.Type_class.write
    let bin_read_t = bin_reader_t.Type_class.read
    let __bin_read_t__ = bin_reader_t.Type_class.vtag_read
    let bin_t =
      { Type_class.
        reader = bin_reader_t;
        writer = bin_writer_t; }
  end

  let make_binable typestruct_of_t =
    let module M = Make_binable(struct
      let typestruct_of_t = typestruct_of_t
    end)
    in
    (module M : Bin_prot.Binable.S with type t = Tagged.t)
end
