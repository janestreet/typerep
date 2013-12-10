(** Witnesses that express whether a type's values are always, sometimes, or never
    immediate.

    A value is immediate when it is internally represented unboxed, using up one word of
    memory, rather than a pointer to a heap-allocated block.

    Some examples:

    - All [int] values are by definition immediate, i.e. unboxed, and so [int]
    is always immediate.

    - A ['a list] is either [[]], which is internally represented as 0 (immediate), or a
    non-empty list, which is represented as a pointer to a heap block (boxed), which
    contains the first element and the pointer to the rest of the list.  Therefore ['a
    list] is sometimes immediate.

    - All values of type ['a ref] are represented as a pointer to a heap block, containing
    the actual values ['a].  Therefore ['a ref] is never immediate.

    The witness values can be used to perform safe optimizations such as allowing a more
    efficient ['a array] blit operations if ['a] is always immediate.  These witnesses can
    also be used to perform safe conversions between immediate values of type ['a] and
    [int] instead of using [Obj.magic].

    {1 Converting between values and ints}

    Consider an arbitrary type ['a] with the conversion functions [value_is_int],
    [value_as_int], [value_as_int_exn], [int_is_value], [int_as_value], and
    [int_as_value_exn], that were obtained from the module [Converter] using
    [typerep : 'a Typerep.t].

    There are three distinct cases for value [v : 'a] that can arise:

    - [v] is an immediate value.

    Let [i] be the [int] that internally represents [v].  The following need to be true:

    [value_is_int     v] evaluates to [true]
    [value_as_int     v] evaluates to [Some i]
    [value_as_int_exn v] evaluates to [i]

    We can also recover [v] by using the conversions that go the other way.  The following
    nee to be true:

    [int_is_value     i] evaluates to [true]
    [int_as_value     i] evaluates to [Some v]
    [int_as_value_exn i] evaluates to [v]

    - [v] is a boxed value that cannot be converted to an [int].

    The following needs to hold:

    [value_is_int     v] evaluates to [false]
    [value_as_int     v] evaluates to [None]
    [value_as_int_exn v] raises an exception.

    {1 Converting ints to values}

    Furthermore, there are two different cases for any [i:int] that can arise:

    + [i] internally represents some value [v:'a].

    [i] is [v]'s underlying runtime representation, i.e. [v] is a value of case 1).  We
    can thus convert [i] to [v] and [v] to [i] using the conversion functions, as
    described above.

    + [i] doesn't represent any value [v:'a]

    The following needs to hold:

    [int_is_value     v] evaluates to [false]
    [int_as_value     v] evaluates to [None]
    [int_as_value_exn v] raises an exception.

    {1 Example}

    For example, consider the following type:

    {[
      type test =
        | A
        | B
        | C of int
      with typerep
    ]}

    Type [test] is sometimes immediate, as [A] is represented as [0], [B] as [1], and [C]
    is a boxed value.  We can construct a witness of type [test Sometimes.t] by using
    [Sometimes.of_typerep] or [of_typerep] and extracting the witness.  Let's call the
    witness [w] here.  We can now use it to safely convert between values of [test] and
    [int]:

    [Sometimes.value_as_int w A]     evaluates to [Some 0]
    [Sometimes.value_as_int w B]     evaluates to [Some 1]
    [Sometimes.value_as_int w (C 1)] evaluates to [None]

    [Sometimes.int_as_value w 0] evaluates to [Some A]
    [Sometimes.int_as_value w 1] evaluates to [Some B]
    [Sometimes.int_as_value w n], for all other values evaluates to [None]

    [value_is_int] is a lightweight function that does not allocate and won't throw an
    exception.  It can be used with [value_as_int_exn] to avoid both allocation and
    throwing an exception, paying only some small amount of CPU time for calling
    [value_is_int].

    [value_is_int value] evaluates to [true] if and only if [value_as_int value]
    evaluates to [Some _] (which is the same as [value_as_int_exn value] evaluating
    successfully).

    Please note that [value_is_int] is not in one to one correspondence with
    [Obj.is_int].  For example, consider the following case:

    {[
      val w : int lazy_t t

      let value = lazy (1+2)
      ignore (Lazy.force value)
    ]}

    [value] is still boxed, however [value_is_int value] evaluates to true, as we can
    convert [value] to [3] ([value_as_int_exn value] evaluates to [3]).  If one used
    [Obj.magic value : int] instead, they would get an undefined, non-integer value.

    {1 N-ary types that are immediate independently of their type arguments}

    We also provide [For_all_parameters_S*] functors.  Those are useful when one has a
    type with type parameters, but knows that values of that type will always be immediate
    (for example) no matter what the actual parameter is.  They can use
    [Always.For_all_parameters_S*] to obtain access to a polymorphic witness.

    An exception is raised on functor application if such witness cannot be obtained.
    That happens either because the witness depends on the actual type parameter, or
    because the type has a different witness (e.g. Sometimes instead of Always).
*)

open Typerep_kernel.Std

type 'a t
type 'a immediate = 'a t

module Always : sig
  type 'a t

  val of_typerep : 'a Typerep.t -> 'a t option

  val int_as_value     : 'a t -> int -> 'a option
  val int_as_value_exn : 'a t -> int -> 'a
  val int_is_value     : 'a t -> int -> bool
  val value_as_int     : 'a t -> 'a  -> int

  module For_all_parameters_S1 (X : Typerepable.S1) : sig
    val witness : unit -> _ X.t t
  end
  module For_all_parameters_S2 (X : Typerepable.S2) : sig
    val witness : unit -> (_,_) X.t t
  end
  module For_all_parameters_S3 (X : Typerepable.S3) : sig
    val witness : unit -> (_,_,_) X.t t
  end
  module For_all_parameters_S4 (X : Typerepable.S4) : sig
    val witness : unit -> (_,_,_,_) X.t t
  end
  module For_all_parameters_S5 (X : Typerepable.S5) : sig
    val witness : unit -> (_,_,_,_,_) X.t t
  end

  val int  : int t
  val char : char t
  val bool : bool t
  val unit : unit t
end

module Sometimes : sig
  type 'a t

  val of_typerep : 'a Typerep.t -> 'a t option

  val int_as_value     : 'a t -> int -> 'a option
  val int_as_value_exn : 'a t -> int -> 'a
  val int_is_value     : 'a t -> int -> bool
  val value_as_int     : 'a t -> 'a  -> int option
  val value_as_int_exn : 'a t -> 'a  -> int
  val value_is_int     : 'a t -> 'a  -> bool

  module For_all_parameters_S1 (X : Typerepable.S1) : sig
    val witness : unit -> _ X.t t
  end
  module For_all_parameters_S2 (X : Typerepable.S2) : sig
    val witness : unit -> (_,_) X.t t
  end
  module For_all_parameters_S3 (X : Typerepable.S3) : sig
    val witness : unit -> (_,_,_) X.t t
  end
  module For_all_parameters_S4 (X : Typerepable.S4) : sig
    val witness : unit -> (_,_,_,_) X.t t
  end
  module For_all_parameters_S5 (X : Typerepable.S5) : sig
    val witness : unit -> (_,_,_,_,_) X.t t
  end

  val option : _ option t
  val list   : _ list   t
end

module Never : sig
  type 'a t

  val of_typerep : 'a Typerep.t -> 'a t option

  module For_all_parameters_S1 (X : Typerepable.S1) : sig
    val witness : unit -> _ X.t t
  end
  module For_all_parameters_S2 (X : Typerepable.S2) : sig
    val witness : unit -> (_,_) X.t t
  end
  module For_all_parameters_S3 (X : Typerepable.S3) : sig
    val witness : unit -> (_,_,_) X.t t
  end
  module For_all_parameters_S4 (X : Typerepable.S4) : sig
    val witness : unit -> (_,_,_,_) X.t t
  end
  module For_all_parameters_S5 (X : Typerepable.S5) : sig
    val witness : unit -> (_,_,_,_,_) X.t t
  end

  val int32      :               int32 t
  val int64      :               int64 t
  val nativeint  :           nativeint t
  val float      :               float t
  val string     :              string t
  val array      :             _ array t
  val ref_       :               _ ref t
  val tuple2     :             (_ * _) t
  val tuple3     :         (_ * _ * _) t
  val tuple4     :     (_ * _ * _ * _) t
  val tuple5     : (_ * _ * _ * _ * _) t
end

val of_typerep : 'a Typerep.t -> 'a t

type 'a dest =
  | Always of 'a Always.t
  | Sometimes of 'a Sometimes.t
  | Never of 'a Never.t

val dest : 'a t -> 'a dest

val int_as_value     : 'a t -> int -> 'a option
val int_as_value_exn : 'a t -> int -> 'a
val int_is_value     : 'a t -> int -> bool
val value_as_int     : 'a t -> 'a  -> int option
val value_as_int_exn : 'a t -> 'a  -> int
val value_is_int     : 'a t -> 'a  -> bool
