module Make (Key : sig
  type t
  val compare : t -> t -> int
end) = struct

  type 'a t = (Key.t * 'a) array

  let of_contents contents =
    Array.fast_sort (fun (k1, _d1) (k2, _d2) -> Key.compare k1 k2) contents;
    contents

  let of_alist alist        = of_contents (Array.of_list alist)

  let of_array_map ~f array = of_contents (Array.map f array)

  let of_array ~f:key array = of_array_map ~f:(fun elt -> (key elt, elt)) array

  let init len ~f           = of_contents (Array.init len f)

  (* benchmarks have showed that this needs to be a toplevel function to avoid closure
     applications during lookup *)
  let rec binary_search t key ~min ~max =
    if min > max then None else begin
      let mid = (min + max) / 2 in
      let (k, a) = Array.get t mid in
      let cmp = Key.compare key k in
      if cmp = 0 then
        Some a
      else if cmp < 0 then
        binary_search t key ~min ~max:(mid - 1)
      else
        binary_search t key ~min:(mid + 1) ~max
    end

  let find t key = binary_search t key ~min:0 ~max:(Array.length t - 1)

end

module Flat_string_map = Make (String)

module Flat_int_map = Make (struct
  type t = int
  let compare (x:int) y = Pervasives.compare x y
end)
