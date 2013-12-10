(** Compute size of values from the binary protocol (lazy) *)

open Bin_prot.Common
open Typerep_kernel.Std

type 'a t = buf -> pos_ref : pos_ref -> unit
include Type_generic.S with type 'a t := 'a t

module Children : sig
  type 'a reader =  buf -> pos_ref : pos_ref -> 'a
  val read_option : [ `some | `none ] reader
  val read_sequence : int reader
  val read_polymorphic_variant : int reader
  val read_usual_variant : int reader
end
