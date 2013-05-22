type (_, _) t = T : ('a, 'a) t
type ('a, 'b) equal = ('a, 'b) t

let refl = T

let trans : type a b c. (a, b) t -> (b, c) t -> (a, c) t = fun T T -> T

let conv : type a b. (a, b) t -> a -> b = fun T x -> x

let symm : type a b. (a, b) t -> (b, a) t = fun T -> T

module Lift (X: sig
  type 'a t
end) = struct
  let lift (type a) (type b) (T : (a, b) t) = (T : (a X.t, b X.t) t)
end
