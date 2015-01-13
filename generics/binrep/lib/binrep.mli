open! Core_kernel.Std
open Typerep_extended.Std

module Sizer : Type_generic.S with type 'a t = 'a Bin_prot.Size.sizer

(*
  Writer needs to be registered before Reader
*)
module Writer : Type_generic.S with type 'a t = 'a Bin_prot.Type_class.writer
module Reader : Type_generic.S with type 'a t = 'a Bin_prot.Type_class.reader

type 'a size_reader = Bin_prot.Common.buf -> pos_ref : Bin_prot.Common.pos_ref -> unit
module Size_reader : sig
  include Type_generic.S with type 'a t = 'a size_reader
  module Children : sig
    type 'a reader =  Bin_prot.Common.buf -> pos_ref : Bin_prot.Common.pos_ref -> 'a
    val read_option : [ `some | `none ] reader
    val read_sequence : int reader
    val read_polymorphic_variant : int reader
    val read_usual_variant : int reader
  end
end

val bin_size_t        : 'a Typerep.t -> [ `generic of 'a Bin_prot.Size.sizer ]
val bin_writer_t      : 'a Typerep.t -> [ `generic of 'a Bin_prot.Type_class.writer ]
val bin_reader_t      : 'a Typerep.t -> [ `generic of 'a Bin_prot.Type_class.reader ]
val bin_size_reader_t : 'a Typerep.t -> [ `generic of 'a size_reader ]

module Make_binable(X:Typerepable.S0) : Binable.S
  with type t := X.t
val make_binable : 'a Typerep.t -> (module Binable.S with type t = 'a)

module Tagged : sig
  open Bin_prot

  val bin_size_t     : Type_struct.t -> [ `generic of Tagged.t Size.sizer ]
  val bin_write_t    : Type_struct.t -> [ `generic of Tagged.t Write.writer ]
  val bin_read_t     : Type_struct.t -> [ `generic of Tagged.t Read.reader ]
  val __bin_read_t__ : Type_struct.t -> [ `generic of (int->Tagged.t) Read.reader]
  val bin_writer_t   : Type_struct.t -> [ `generic of Tagged.t Type_class.writer ]
  val bin_reader_t   : Type_struct.t -> [ `generic of Tagged.t Type_class.reader ]
  val bin_t          : Type_struct.t -> [ `generic of Tagged.t Type_class.t ]

  val bin_size_reader_t : Type_struct.t -> [ `generic of Tagged.t size_reader ]

  module Make_binable(X:sig val typestruct_of_t : Type_struct.t end) : Binable.S
    with type t := Tagged.t
  val make_binable : Type_struct.t -> (module Binable.S with type t = Tagged.t)
end
