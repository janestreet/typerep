open Core.Std
open Typereplib.Std

module Of_json : sig
  exception Type_mismatch of string * Json.Json_type.t
  include Type_generic.S with type 'a t = Json.Json_type.t -> 'a
end
module Json_of : Type_generic.S with type 'a t = 'a -> Json.Json_type.t

val t_of_json : 'a Typerep.t -> [`generic of Json.Json_type.t -> 'a]
val json_of_t : 'a Typerep.t -> [`generic of 'a -> Json.Json_type.t]

module Tagged : sig
  module Of_json : Tagged_generic.S with type 'a t = Json.Json_type.t -> 'a
  module Json_of : Tagged_generic.S with type 'a t = 'a -> Json.Json_type.t

  val t_of_json : Type_struct.t -> [ `generic of Json.Json_type.t -> Tagged.t ]
  val json_of_t : Type_struct.t -> [ `generic of Tagged.t -> Json.Json_type.t ]
end
