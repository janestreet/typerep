open Core.Std
open Typerep_experimental.Std

module type S = sig
  module Of_json : Type_generic.S with type 'a t = Json.Json_type.t -> 'a
  module Json_of : Type_generic.S with type 'a t = 'a -> Json.Json_type.t
  val t_of_json : 'a Typerep.t -> [`generic of Json.Json_type.t -> 'a]
  val json_of_t : 'a Typerep.t -> [`generic of 'a -> Json.Json_type.t]
  module Tagged : sig
    module Of_json : Tagged_generic.S with type 'a t = Json.Json_type.t -> 'a
    module Json_of : Tagged_generic.S with type 'a t = 'a -> Json.Json_type.t
    val t_of_json : Type_struct.t -> [ `generic of Json.Json_type.t -> Tagged.t ]
    val json_of_t : Type_struct.t -> [ `generic of Tagged.t -> Json.Json_type.t ]
  end
end

(**
   The difference between both implementation is their behavior while {de}serializing
   optional fields in records.
   Optional fields meaning a field in a record of type [_ option].

   v1 => will create the fields, putting Json.Null as the value. will expect also the
   field to be there with value Null during deserialization.
   v2 => the fields will not be present

   v1 was the original behavior, and is closer to a strictly typed kind of
   language, but it turns out it is actually a pain to use in practice.

   v2 is used by some serialization frameworks to indicate the absence of an object
   instead of Null.

   Note that the deserializer of V2 is backward compatible in that it can read json
   records serialized with V1. That is, V2 supports optional fields present with value
   Null.

   Example:
   {[
   type t = {
     a : int option;
   } with typerep

   V1.json_of_t { a = None } ==> Jt.Object [ "a",  Jt.Null ]
   V2.json_of_t { a = None } ==> Jt.Object [ ]

   V1.t_of_json (Jt.Object [ "a",  Jt.Null ]) ==> { a = None }
   V2.t_of_json (Jt.Object [ "a",  Jt.Null ]) ==> { a = None }

   V1.t_of_json (Jt.Object []) ==> FAIL
   V2.t_of_json (Jt.Object []) ==> { a = None }
   ]}
*)
module V1 : S
module V2 : S
