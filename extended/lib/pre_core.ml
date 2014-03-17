(* minimal set up since we do not have core available *)
open Typerep_lib.Std

module Std = struct
  include Sexplib.Std
  include Bin_prot.Std

  module Flat_int_map = Flat_map.Flat_int_map
  module Flat_string_map = Flat_map.Flat_string_map

  module Sexp = Sexplib.Sexp

  module Sexpable = struct
    module type S = sig
      type t with sexp
    end

    module type S1 = sig
      type 'a t with sexp
    end

    module type S2 = sig
      type ('a, 'b) t with sexp
    end

    module type S3 = sig
      type ('a, 'b, 'c) t with sexp
    end
  end

  module Binable = Bin_prot.Binable

  let phys_equal = Pervasives.(==)
  external ident : 'a -> 'a = "%identity"

  module Array = struct
    type 'a t = 'a array with sexp, bin_io, typerep

    include Array

    let is_empty t = length t = 0

    let rec list_length accu = function
      | [] -> accu
      | _ :: tl -> list_length (succ accu) tl

    let of_list_map ~f xs =
      match xs with
      | [] -> [||]
      | hd::tl ->
        let a = create (list_length 1 tl) (f hd) in
        let rec fill a i = function
          | [] -> a
          | hd::tl -> unsafe_set a i (f hd); fill a (i+1) tl in
        fill a 1 tl

    (* It can be written in a more simple (but less efficient) way with [fold_right] *)
    let to_list_map ~f arr =
      let rec aux_to_list_map arr f acc index =
        if index < 0 then acc
        else
          let elt = f (unsafe_get arr index) in
          aux_to_list_map arr f (elt::acc) (pred index)
      in
      aux_to_list_map arr f [] (pred (length arr))

    let map_stable ~f:map array =
      let rec aux map array size index =
        if index >= size then array else
          let elt = array.(index) in
          let felt = map elt in
          if phys_equal elt felt then aux map array size (succ index) else begin
            let new_array = copy array in
            new_array.(index) <- felt;
            for i = succ index to pred size do
              new_array.(i) <- map array.(i)
            done;
            new_array
          end
      in
      let size = Array.length array in
      aux map array size 0

    include ArrayLabels

    let iter2_exn ~f a b =
      if length a <> length b then invalid_arg "Array.iter2_exn";
      let f i a = f a b.(i) in
      iteri ~f a

    let findi ~f t =
      let length = length t in
      let rec loop i =
        if i >= length then None
        else
          let elt = t.(i) in
          match f i elt with
          | (Some _) as some -> some
          | None -> loop (succ i)
      in
      loop 0

    let exists ~f t =
      let length = length t in
      let rec loop i =
        if i >= length then false
        else
          let elt = t.(i) in
          f elt || loop (succ i)
      in
      loop 0

    let for_all ~f t =
      let length = length t in
      let rec loop i =
        if i >= length then true
        else
          let elt = t.(i) in
          f elt && loop (succ i)
      in
      loop 0
  end

  (* readonly array *)
  module Farray : sig
    type 'a t with sexp, bin_io, typerep
    val get : 'a t -> int -> 'a
    val length : _ t -> int
    val init : int -> f:(int -> 'a) -> 'a t
    val empty : unit -> 'a t
    val make1 : 'a -> 'a t
    val make2 : 'a -> 'a -> 'a t
    val make3 : 'a -> 'a -> 'a -> 'a t
    val make4 : 'a -> 'a -> 'a -> 'a -> 'a t
    val make5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a t
    val iter : f:('a -> unit) -> 'a t -> unit
    val map : f:('a -> 'b) -> 'a t -> 'b t
    val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
    val sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
    val for_all : f:('a -> bool) -> 'a t -> bool
    val exists : f:('a -> bool) -> 'a t -> bool
    val of_array : f:(int -> 'a -> 'b) -> 'a array -> 'b t
    val to_array : f:(int -> 'a -> 'b) -> 'a t -> 'b array
    val map_stable : f:('a -> 'a) -> 'a t -> 'a t
    val to_list : 'a t -> 'a list
    val of_list : 'a list -> 'a t
    val iter2_exn : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
    val is_empty : _ t -> bool
    val findi : f:(int -> 'a -> 'b option) -> 'a t -> 'b option
    val of_list_map : f:('a -> 'b) -> 'a list -> 'b t
    val to_list_map : f:('a -> 'b) -> 'a t -> 'b list
  end = struct
    include Array
    let empty () = [||]
    let make1 a = [|a|]
    let make2 a b = [|a ; b|]
    let make3 a b c = [|a ; b ; c|]
    let make4 a b c d = [|a ; b ; c ; d|]
    let make5 a b c d e = [|a ; b ; c ; d ; e|]
    let sort ~cmp a =
      let a = Array.copy a in
      stable_sort ~cmp a;
      a
    let of_array = mapi
    let to_array = mapi
  end

  module Hashable = struct
    module type Key = sig
      type t
      val compare : t -> t -> int
      val hash : t -> int
    end
    module type S = sig
      type key
      module Table : sig
        type 'a t
        val create : unit -> 'a t
        val set : 'a t -> key:key -> data:'a -> unit
        val find : 'a t -> key -> 'a option
        val length : 'a t -> int
        val iter : 'a t -> f:(key -> 'a -> unit) -> unit
        val to_array : 'a t -> f:(key -> 'a -> 'b) -> 'b array
        val to_init : (int -> f:(int -> 'b) -> 'c) -> 'a t -> f:(key -> 'a -> 'b) -> 'c
      end
      module Hash_set : sig
        type t
        val create : unit -> t
        val mem : t -> key -> bool
        val add : t -> key -> unit
      end
      module Map : sig
        type 'a t
        val empty : 'a t
        val mem : 'a t -> key -> bool
        val add : 'a t -> key:key -> data:'a -> 'a t
        val find : 'a t -> key -> 'a option
        val to_alist : 'a t -> (key * 'a) list
      end
    end
    module Make(Key:Key) : S with type key := Key.t = struct
      module T = Hashtbl.Make(struct include Key let equal a b = compare a b = 0 end)
      module Table = struct
        include T
        let create () = create 128
        let set a ~key ~data = replace a key data
        let find a key = try Some (find a key) with Not_found -> None
        let iter table ~f = iter f table
        let to_array table ~f =
          let length = length table in
          let result = ref None in
          let f key a =
            match !result with
            | Some (array, index) -> incr index; array.(!index) <- f key a
            | None ->
              let data = f key a in
              let array = Array.make length data in
              result := Some (array, ref 0)
          in
          iter table ~f;
          match !result with
          | None -> [||]
          | Some (array, index) -> assert (!index = pred length); array
        let to_init init table ~f =
          let array = to_array table ~f in
          init (Array.length array) ~f:(fun i -> array.(i))
      end
      module Hash_set = struct
        type t = unit T.t
        let create () = T.create 128
        let mem = T.mem
        let add t b = T.replace t b ()
      end
      module Map = struct
        include Map.Make(Key)
        let add map ~key ~data = add key data map
        let find map key = try Some (find key map) with Not_found -> None
        let mem map key = mem key map
        let to_alist = bindings
      end
    end
  end

  module Int = struct
    module T = struct
      type t = int with sexp, bin_io
      let hash (a:int) = Hashtbl.hash a
      let compare (a:int) b = Pervasives.compare a b
      let equal a b = compare a b = 0
    end
    include T
    include Hashable.Make(T)
  end

  module List = struct
    include List
    include ListLabels
    let is_empty = function
      | [] -> true
      | _ :: _ -> false
    let rev_filter ~f list =
      let rec aux acc = function
        | [] -> acc
        | hd :: tl ->
          let acc = if f hd then hd :: acc else acc in
          aux acc tl
      in
      aux [] list
    let nth list pos = try Some (nth list pos) with _ -> None
    let filter_map ~f list =
      let rec aux acc = function
        | [] -> List.rev acc
        | hd :: tl ->
          match f hd with
          | Some hd -> aux (hd::acc) tl
          | None -> aux acc tl
      in
      aux [] list
  end

  module Option = struct
    type 'a t = 'a option with sexp, bin_io
    let bind a ~f =
      match a with
      | None -> None
      | Some a -> f a
    let is_some = function Some _ -> true | None -> false
    let is_none = function None -> true | Some _ -> false
    let iter ~f = function
      | None -> ()
      | Some value -> f value
    let map ~f = function
      | None -> None
      | Some a -> Some (f a)
    let map_stable ~f t =
      match t with
      | Some arg ->
        let arg' = f arg in
        if phys_equal arg arg' then t else Some arg'
      | None -> t
  end

  module String = struct
    module T = struct
      type t = string with sexp, bin_io
      include (StringLabels : (module type of StringLabels with type t := t))
      let hash (a:string) = Hashtbl.hash a
      let equal a b = String.compare a b = 0
    end
    include T
    include Hashable.Make(T)
    let split str ~on =
      let len = length str in
      let rec loop acc last_pos pos =
        if pos = -1 then
          sub str ~pos:0 ~len:last_pos :: acc
        else
          if str.[pos] = on then
            let pos1 = pos + 1 in
            let sub_str = sub str ~pos:pos1 ~len:(last_pos - pos1) in
            loop (sub_str :: acc) pos (pos - 1)
          else loop acc last_pos (pos - 1)
      in
      loop [] len (len - 1)
  end
end
