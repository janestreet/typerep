module type S = sig
  type 'a t
  type key

  val of_alist : (key * 'a) list -> 'a t

  val of_array : f:('a -> key) -> 'a array -> 'a t

  val of_array_map : f:('a -> key * 'b) -> 'a array -> 'b t

  val init : int -> f:(int -> key * 'a) -> 'a t

  val find : 'a t -> key -> 'a option
end
