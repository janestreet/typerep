open Typerep_kernel.Std

module type S = sig
  type 'a t
  val of_typestruct : Type_struct.t -> [ `generic of Tagged.t t ]
end

module Make_input(Input:sig type t end)
  (X:Type_generic.Computation with type 'a t = Input.t -> 'a)
  : S with type 'a t = Input.t -> 'a

module Make_output(Output:sig type t end)
  (X:Type_generic.Computation with type 'a t = 'a -> Output.t)
  : S with type 'a t = 'a -> Output.t

module Make_reader
  (Builder:sig type 'a t val make : ('a -> Tagged.t) -> 'a t -> Tagged.t t end)
  (X:Type_generic.Computation with type 'a t = 'a Builder.t)
  : S with type 'a t = 'a X.t

module Make_writer
  (Builder:sig type 'a t val make : (Tagged.t -> 'a) -> 'a t -> Tagged.t t end)
  (X:Type_generic.Computation with type 'a t = 'a Builder.t)
  : S with type 'a t = 'a X.t

(* Advanced utilisation of [Make] *)

module type Adapter = sig

  type 'a t
  type 'a adapter = 'a t -> Tagged.t t

  val int    : int adapter
  val int32  : int32 adapter
  val int64  : int64 adapter
  val char   : char adapter
  val bool   : bool adapter
  val string : string adapter
  val float  : float adapter
  val unit   : unit adapter

  val option : Tagged.t option adapter
  val list   : Tagged.t list adapter
  val array  : Tagged.t array adapter
  val ref_   : Tagged.t ref adapter
  val lazy_t : Tagged.t lazy_t adapter

  val tuple2 : (Tagged.t * Tagged.t) adapter
  val tuple3 : (Tagged.t * Tagged.t * Tagged.t) adapter
  val tuple4 : (Tagged.t * Tagged.t * Tagged.t * Tagged.t) adapter
  val tuple5 : (Tagged.t * Tagged.t * Tagged.t * Tagged.t * Tagged.t) adapter
end

module Make_advanced
  (A:Adapter)
  (X:Type_generic.Computation with type 'a t = 'a A.t)
  : S with type 'a t = 'a X.t

module Input_adapter(Input:sig type t end) : Adapter with type 'a t = Input.t -> 'a
module Output_adapter(Output:sig type t end) : Adapter with type 'a t = 'a -> Output.t
module Reader_adapter(Builder:sig
  type 'a t
  val make : ('a -> Tagged.t) -> 'a t -> Tagged.t t
end) : Adapter with type 'a t = 'a Builder.t
module Writer_adapter(Builder:sig
  type 'a t
  val make : (Tagged.t -> 'a) -> 'a t -> Tagged.t t
end) : Adapter with type 'a t = 'a Builder.t
