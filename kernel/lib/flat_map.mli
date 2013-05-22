(**
   Simple binary search array indexed by a comparable.

   This structure is used in the implementation of generic pattern matching and
   memoization of computation based on the index of a field in a record type, or a tag in
   a sum type.

   Some benchmarks have showed that this flat structure behaves better than trees for the
   use case we have (allocation, lookup, memory wise, etc.).
*)

module Make (Key : sig
  type t
  val compare : t -> t -> int
end) : Flat_map_intf.S with type key := Key.t

module Flat_string_map : Flat_map_intf.S with type key := string
module Flat_int_map    : Flat_map_intf.S with type key := int
