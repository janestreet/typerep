open Bin_prot
open Typerep_extended.Std

module Computation_impl = struct

  type 'a t = 'a Size.sizer
  include Type_generic.Variant_and_record_intf.M(struct type nonrec 'a t = 'a t end)

  let int       = Size.bin_size_int
  let int32     = Size.bin_size_int32
  let int64     = Size.bin_size_int64
  let nativeint = Size.bin_size_nativeint
  let char      = Size.bin_size_char
  let float     = Size.bin_size_float
  let string    = Size.bin_size_string
  let bool      = Size.bin_size_bool
  let unit      = Size.bin_size_unit

  let option    = Size.bin_size_option
  let list      = Size.bin_size_list
  let array     = Size.bin_size_array
  let lazy_t    = Size.bin_size_lazy
  let ref_      = Size.bin_size_ref

  (* bin_io does *NOT* support serialization of functions *)
  let function_ _ = assert false

  let tuple2    = fun sizer_fst sizer_snd ->
    fun (fst, snd) -> (sizer_fst fst) + (sizer_snd snd)
  let tuple3    = fun sizer_a sizer_b sizer_c ->
    fun (a,b,c) -> sizer_a a + sizer_b b + sizer_c c
  let tuple4    = fun sizer_a sizer_b sizer_c sizer_d ->
    fun (a,b,c,d) -> sizer_a a + sizer_b b + sizer_c c + sizer_d d
  let tuple5    = fun sizer_a sizer_b sizer_c sizer_d sizer_e ->
    fun (a,b,c,d,e) -> sizer_a a + sizer_b b + sizer_c c + sizer_d d + sizer_e e

  let record record value =
    let aggregate_fields_size acc = function
      | Record.Field field -> acc + Field.traverse field (Field.get field value)
    in
    Record.fold record ~init:0 ~f:aggregate_fields_size

  let variant variant =
    let tag_size =
      if Variant.is_polymorphic variant
      then 4
      else
        let len = Variant.length variant in
        if len < 256 then 1 else 2
    in
    let bin_size_t value =
      match Variant.value variant value with
      | Variant.Value (tag, args) ->
        let arity = Tag.arity tag in
        if arity = 0 then tag_size
        else
          tag_size + Tag.traverse tag args
    in
    bin_size_t

  module Named = Type_generic.Make_named_for_closure(struct
    type 'a input = 'a
    type 'a output = int
    type 'a t = 'a Size.sizer
  end)
end

include Type_generic.Make(struct
  include Computation_impl
  let name = "bin_sizer"
  let required = [ Type_struct.Generic.ident ]
end)


