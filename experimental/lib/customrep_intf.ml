open Typerep_extended.Std
open Pre_core.Std

module type S0 = sig
  type t
  include Typerepable.S0 with type t := t
  include Sexpable.S with type t := t
  include Binable.S with type t := t
  include Typestructable.S0 with type t := t
end

module type S1 = sig
  type 'a t
  include Typerepable.S1 with type 'a t := 'a t
  include Sexpable.S1 with type 'a t := 'a t
  include Binable.S1 with type 'a t := 'a t
  include Typestructable.S1 with type 'a t := 'a t
end

module type S2 = sig
  type ('a, 'b) t
  include Typerepable.S2 with type ('a, 'b) t := ('a, 'b) t
  include Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t
  include Binable.S2 with type ('a, 'b) t := ('a, 'b) t
  include Typestructable.S2 with type ('a, 'b) t := ('a, 'b) t
end
