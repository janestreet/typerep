open! Base
open Std_internal

module Make0 (X : sig
    type t

    include Named_intf.S0 with type t := t
  end) : Typerepable.S with type t := X.t = struct
  module M = Make_typename.Make0 (X)

  let typerep_of_t = Typerep.Named (M.named, Second Value)
  let typename_of_t = M.typename_of_t
end

module Make1 (X : sig
    type 'a t

    include Named_intf.S1 with type 'a t := 'a t
  end) : Typerepable.S1 with type 'a t := 'a X.t = struct
  module M = Make_typename.Make1 (X)

  let typerep_of_t of_p1 = Typerep.Named (M.named of_p1, Second Value)
  let typename_of_t = M.typename_of_t
end

module Make2 (X : sig
    type ('a, 'b) t

    include Named_intf.S2 with type ('a, 'b) t := ('a, 'b) t
  end) : Typerepable.S2 with type ('a, 'b) t := ('a, 'b) X.t = struct
  module M = Make_typename.Make2 (X)

  let typerep_of_t of_p1 of_p2 = Typerep.Named (M.named of_p1 of_p2, Second Value)
  let typename_of_t = M.typename_of_t
end

module Make3 (X : sig
    type ('a, 'b, 'c) t

    include Named_intf.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  end) : Typerepable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = struct
  module M = Make_typename.Make3 (X)

  let typerep_of_t of_p1 of_p2 of_p3 =
    Typerep.Named (M.named of_p1 of_p2 of_p3, Second Value)
  ;;

  let typename_of_t = M.typename_of_t
end

module Make4 (X : sig
    type ('a, 'b, 'c, 'd) t

    include Named_intf.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
  end) : Typerepable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) X.t = struct
  module M = Make_typename.Make4 (X)

  let typerep_of_t of_p1 of_p2 of_p3 of_p4 =
    Typerep.Named (M.named of_p1 of_p2 of_p3 of_p4, Second Value)
  ;;

  let typename_of_t = M.typename_of_t
end

module Make5 (X : sig
    type ('a, 'b, 'c, 'd, 'e) t

    include Named_intf.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
  end) : Typerepable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) X.t =
struct
  module M = Make_typename.Make5 (X)

  let typerep_of_t of_p1 of_p2 of_p3 of_p4 of_p5 =
    Typerep.Named (M.named of_p1 of_p2 of_p3 of_p4 of_p5, Second Value)
  ;;

  let typename_of_t = M.typename_of_t
end
