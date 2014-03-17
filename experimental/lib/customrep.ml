open Typerep_extended.Std
open Typerep_sexp.Std
open Typerep_bin_io.Std

let register0 m =
  let module M = (val m : Customrep_intf.S0) in
  let typerep_of_t = M.typerep_of_t in
  let typename_of_t = M.typename_of_t in
  let module Sexp_of = struct
    type t = M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.sexp_of_t
  end in
  let module Of_sexp = struct
    type t = M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.t_of_sexp
  end in
  let module Sizer = struct
    type t = M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_size_t
  end in
  let module Writer = struct
    type t = M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_writer_t
  end in
  let module Reader = struct
    type t = M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_reader_t
  end in
  let module Struct = struct
    type t = M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.typestruct_of_t
  end in
  Type_struct.Generic.register0 (module Struct : Type_struct.Generic.S0);
  Sexprep.Sexp_of.register0 (module Sexp_of : Sexprep.Sexp_of.S0);
  Sexprep.Of_sexp.register0 (module Of_sexp : Sexprep.Of_sexp.S0);
  Binrep.Sizer.register0 (module Sizer : Binrep.Sizer.S0);
  Binrep.Writer.register0 (module Writer : Binrep.Writer.S0);
  Binrep.Reader.register0 (module Reader : Binrep.Reader.S0);
;;

let register1 m =
  let module M = (val m : Customrep_intf.S1) in
  let typerep_of_t = M.typerep_of_t in
  let typename_of_t = M.typename_of_t in
  let module Sexp_of = struct
    type 'a t = 'a M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.sexp_of_t
  end in
  let module Of_sexp = struct
    type 'a t = 'a M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.t_of_sexp
  end in
  let module Sizer = struct
    type 'a t = 'a M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_size_t
  end in
  let module Writer = struct
    type 'a t = 'a M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_writer_t
  end in
  let module Reader = struct
    type 'a t = 'a M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_reader_t
  end in
  let module Struct = struct
    type 'a t = 'a M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.typestruct_of_t
  end in
  Type_struct.Generic.register1 (module Struct : Type_struct.Generic.S1);
  Sexprep.Sexp_of.register1 (module Sexp_of : Sexprep.Sexp_of.S1);
  Sexprep.Of_sexp.register1 (module Of_sexp : Sexprep.Of_sexp.S1);
  Binrep.Sizer.register1 (module Sizer : Binrep.Sizer.S1);
  Binrep.Writer.register1 (module Writer : Binrep.Writer.S1);
  Binrep.Reader.register1 (module Reader : Binrep.Reader.S1);
;;

let register2 m =
  let module M = (val m : Customrep_intf.S2) in
  let typerep_of_t = M.typerep_of_t in
  let typename_of_t = M.typename_of_t in
  let module Sexp_of = struct
    type ('a, 'b) t = ('a, 'b) M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.sexp_of_t
  end in
  let module Of_sexp = struct
    type ('a, 'b) t = ('a, 'b) M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.t_of_sexp
  end in
  let module Sizer = struct
    type ('a, 'b) t = ('a, 'b) M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_size_t
  end in
  let module Writer = struct
    type ('a, 'b) t = ('a, 'b) M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_writer_t
  end in
  let module Reader = struct
    type ('a, 'b) t = ('a, 'b) M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.bin_reader_t
  end in
  let module Struct = struct
    type ('a, 'b) t = ('a, 'b) M.t
    let typerep_of_t = typerep_of_t
    let typename_of_t = typename_of_t
    let compute = M.typestruct_of_t
  end in
  Type_struct.Generic.register2 (module Struct : Type_struct.Generic.S2);
  Sexprep.Sexp_of.register2 (module Sexp_of : Sexprep.Sexp_of.S2);
  Sexprep.Of_sexp.register2 (module Of_sexp : Sexprep.Of_sexp.S2);
  Binrep.Sizer.register2 (module Sizer : Binrep.Sizer.S2);
  Binrep.Writer.register2 (module Writer : Binrep.Writer.S2);
  Binrep.Reader.register2 (module Reader : Binrep.Reader.S2);
;;

