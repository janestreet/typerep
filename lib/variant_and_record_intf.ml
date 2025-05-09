(** Place holder for common Variants and Fields interface *)

open! Base

module type%template [@modality nonportable] X = sig
  type ('a : any) t
end

module type%template [@modality portable] X = sig
  type ('a : any) t : value mod contended portable
end

module%template [@modality p = nonportable] Types = struct
  module type Tag = sig
    type ('variant, 'args : any) create =
      | Args of ((unit -> 'args) -> 'variant)
      | Const of 'variant

    type ('variant, 'args : any) t
  end

  module Variant (Tag : Tag) = struct
    module type S = sig
      (** An existential type used to gather all the tags constituing a variant type. the
          ['variant] parameter is the variant type, it is the same for all the
          constructors of that variant type. The type of the parameters might be different
          for each constructor and is thus existential *)
      type _ tag = Tag : 'variant ('args : any). ('variant, 'args) Tag.t -> 'variant tag

      (** A similar existential constructor to [_ tag] but this one holds a value whose
          type is the arguments of the tag constructor. A value of type ['a value] is a
          pair of
          (1) a value of variant type ['a] along with (2) some information about the
              constructor within the type ['a] *)
      type _ value =
        | Value :
            'variant ('args : any).
            ('variant, 'args) Tag.t * (unit -> 'args)
            -> 'variant value

      (** Witness of a variant type. The parameter is the type of the variant type
          witnessed. *)
      type 'a t
    end
  end

  module type Field = sig
    type ('record, 'field : any) t
  end

  module Record (Field : Field) = struct
    module type S = sig
      (** An existential type used to gather all the fields constituing a record type. the
          ['record] parameter is the record type, it is the same for all the field of that
          record type. The type of the fields might be different for each field and is
          thus existential. *)
      type _ field = Field : 'record ('a : any). ('record, 'a) Field.t -> 'record field

      (** ['record fields] is a type isomorphic to ['record]. This gives a way to get the
          field value for each field of the record. The advantage of this representation
          is that it is convenient for writing generic computations. *)
      type 'record fields =
        { get : ('field : any). ('record, 'field) Field.t -> unit -> 'field }

      (** Witness of a record type. The parameter is the type of the record type
          witnessed. *)
      type 'a t
    end
  end

  module Tag_internal (X : X [@modality p]) = struct
    type ('variant, 'args : any) create =
      | Args of ((unit -> 'args) -> 'variant)
      | Const of 'variant

    type ('variant, 'args : any) t =
      { label : string
      ; rep : 'args X.t
      ; arity : int
      ; args_labels : string list
      ; index : int
      ; ocaml_repr : int
      ; tyid : 'args Typename.t
      ; create : ('variant, 'args) create
      }
  end

  module Variant_internal (Tag : Tag) = struct
    type _ tag = Tag : 'variant ('a : any). ('variant, 'a) Tag.t -> 'variant tag

    type _ value =
      | Value : 'variant ('a : any). ('variant, 'a) Tag.t * (unit -> 'a) -> 'variant value

    type 'a t =
      { typename : 'a Typename.t
      ; tags : 'a tag iarray
      ; polymorphic : bool
      ; value : 'a -> 'a value
      }
  end

  module Field_internal (X : X [@modality p]) = struct
    type ('record, 'field : any) t =
      { label : string
      ; rep : 'field X.t
      ; index : int
      ; tyid : 'field Typename.t
      ; get : 'record -> unit -> 'field
      ; is_mutable : bool
      }
  end

  module Record_internal (Field : Field) = struct
    type _ field = Field : 'record ('a : any). ('record, 'a) Field.t -> 'record field

    type 'record fields =
      { get : ('field : any). ('record, 'field) Field.t -> unit -> 'field }

    type 'a t =
      { typename : 'a Typename.t
      ; fields : 'a field iarray
      ; has_double_array_tag : bool Portable_lazy.t
      ; create : 'a fields -> 'a
      }
  end
end

(** Like [Types [@modality nonportable]] above. Makes (most of) the types cross
    portability and contention. *)
module%template [@modality p = portable] Types = struct
  module type Tag = sig
    type ('variant, 'args : any) create : value mod contended portable =
      | Args of ((unit -> 'args) -> 'variant) @@ portable
      | Const of (unit -> 'variant) @@ portable
    [@@unsafe_allow_any_mode_crossing]

    type ('variant, 'args : any) t : value mod contended portable
  end

  module Variant (Tag : Tag) = struct
    module type S = sig
      type _ tag : value mod contended portable =
        | Tag : 'variant ('args : any). ('variant, 'args) Tag.t -> 'variant tag
      [@@unsafe_allow_any_mode_crossing]

      type _ value =
        | Value :
            'variant ('args : any).
            ('variant, 'args) Tag.t * (unit -> 'args)
            -> 'variant value

      type 'a t : value mod contended portable
    end
  end

  module type Field = sig
    type ('record, 'field : any) t : value mod contended portable
  end

  module Record (Field : Field) = struct
    module type S = sig
      type _ field : value mod contended portable =
        | Field : 'record ('a : any). ('record, 'a) Field.t -> 'record field
      [@@unsafe_allow_any_mode_crossing]

      type 'record fields =
        { get : ('field : any). ('record, 'field) Field.t -> unit -> 'field }

      type 'a t : value mod contended portable
    end
  end

  module Tag_internal (X : X [@modality p]) = struct
    type ('variant, 'args : any) create : value mod contended portable =
      | Args of ((unit -> 'args) -> 'variant) @@ portable
      | Const of (unit -> 'variant) @@ portable
    [@@unsafe_allow_any_mode_crossing]

    type ('variant, 'args : any) t : value mod contended portable =
      { label : string
      ; rep : 'args X.t
      ; arity : int
      ; args_labels : string list
      ; index : int
      ; ocaml_repr : int
      ; tyid : 'args Typename.t
      ; create : ('variant, 'args) create
      }
  end

  module Variant_internal (Tag : Tag) = struct
    type _ tag : value mod contended portable =
      | Tag : 'variant ('a : any). ('variant, 'a) Tag.t -> 'variant tag
    [@@unsafe_allow_any_mode_crossing]

    type _ value =
      | Value : 'variant ('a : any). ('variant, 'a) Tag.t * (unit -> 'a) -> 'variant value

    type 'a t : value mod contended portable =
      { typename : 'a Typename.t
      ; tags : 'a tag iarray
      ; polymorphic : bool
      ; value : 'a -> 'a value @@ portable
      }
    [@@unsafe_allow_any_mode_crossing]
  end

  module Field_internal (X : X [@modality p]) = struct
    type ('record, 'field : any) t : value mod contended portable =
      { label : string
      ; rep : 'field X.t
      ; index : int
      ; tyid : 'field Typename.t
      ; get : 'record -> unit -> 'field @@ portable
      ; is_mutable : bool
      }
    [@@unsafe_allow_any_mode_crossing]
  end

  module Record_internal (Field : Field) = struct
    type _ field : value mod contended portable =
      | Field : 'record ('a : any). ('record, 'a) Field.t -> 'record field
    [@@unsafe_allow_any_mode_crossing]

    type 'record fields =
      { get : ('field : any). ('record, 'field) Field.t -> unit -> 'field }

    type 'a t : value mod contended portable =
      { typename : 'a Typename.t
      ; fields : 'a field iarray
      ; has_double_array_tag : bool Portable_lazy.t
      ; create : 'a fields -> 'a @@ portable
      }
    [@@unsafe_allow_any_mode_crossing]
  end
end

module%template
  [@modality p = (portable, nonportable)] M
    (X : (* This functor is essentially there because we use this same interface in
            different contexts, with different types for ['a t].

            1) One use case for it is where ['a X.t = 'a Typerep.t]. These interfaces are
               then part of the type witness built for a type containing a record or a
               variant in its structure. [traverse] will give a way of accessing the type
               representation for the arguments of a variant or record type.

            2) Another use case is for building "staged generic computations". In that
               case, the type ['a X.t] is the type of the computation that is being built.
               [traverse] returns the computation built for the argument. The interface no
               longer exports the typerep of the arguments in hopes of enforcing that no
               typerep traversal happens at runtime if the computation happen to be a
               function. *)
     X
  [@modality p]) =
struct
  open Types [@modality p]

  (* The functions prefixed by [internal] as well as the module suffixed by [_internal]
     are used by the code generated by the camlp4 extension [with typerep] as well as some
     internals of the typerep library. Do not consider using these somewhere else.  They
     should ideally not be exported outside the typerep library, but the generated code
     needs somehow to access this, even outside. *)

  module Tag_internal = Tag_internal (X)

  (** Witness of a tag, that is an item in a variant type, also called an "applied variant
      Constructor"

      The first parameter is the variant type, the second is the type of the tag
      parameters. Example:

      {[
        type t =
          | A of (int * string)
          | B of string
          | C of
              { x : int
              ; y : string
              }
      ]}

      this type has three constructors. For each of them we'll have a corresponding
      [Tag.t]:

      {[
        val tag_A : (t, #(int * string)) Tag.t
        val tag_B : (t, string) Tag.t
        val tag_C : (t, #(int * string)) Tag.t
      ]}

      Note, inline record in variant are typed as if their definition was using tuples,
      without the parenthesis. This is consistent with their runtime representation. But
      the distinction is carried and available for introspection as part of the [Tag.t].
      See [args_labels]. *)
  module Tag : sig @@ portable
    include Tag

    (** The name of the constructor as it is given in the concrete syntax Examples:

        {v
         Constructor        | label
         -------------------------
         | A of int         |  "A"
         | `a of int        |  "a"
         | `A of int        |  "A"
         | A of { x : int } |  "A"
        v}

        for standard variant, the ocaml syntax implies that this label will always starts
        with a capital letter. For polymorphic variants, this might be a lowercase char.
        For polymorphic variant, this label does not include the [`] character. *)
    val label : 'a ('b : any). ('a, 'b) t -> string

    (** The size of the ocaml heap block containing the arguments

        Examples:
        {v
          0: | A | 'A
          1: | A of int | `A of int | A of (int * int) | `A of (int * int)
             | `A of int * int
             | A of { x : int}
          2: | A of int * float
             | A of { x : int; y : string }
          etc.
        v} *)
    val arity : 'a ('b : any). ('a, 'b) t -> int

    (** The label of the fields for inline records. For other forms of tags, this is the
        empty list. When this returns a non empty list, the length of the returned list is
        equal to the arity.

        Example:

        {v
         (1) Empty:

           | A | 'A
           | A of int | `A of int | A of (int * int) | `A of (int * int)
           | `A of int * int
           | A of int * float

         (2) Non empty:

           | A of { x : int }               -> [ "x" ]
           | A of { x : int; y : string }   -> [ "x" ; "y" ]
        v} *)
    val args_labels : 'a ('b : any). ('a, 'b) t -> string list

    (** The index of the constructor in the list of all the variant type's constructors
        Examples:
        {[
          type t =
            | A of int (* 0 *)
            | B (* 1 *)
            | C of int (* 2 *)
            | D of char (* 3 *)
            | E of { x : int }
          (* 4 *)
        ]} *)
    val index : 'a ('b : any). ('a, 'b) t -> int

    (** {v
       ocaml_repr is related to the runtime of objects. this is essentially a way of
       giving one the ability to rebuild dynamically an [Obj.t] representing a tag.

       Polymorphic variants:
       ---------------------

       [ocaml_repr] is the hash of the label, as done by the compiler.
       Example:
       print_int (Obj.magic `bar)  (* 4895187 *)
       print_int (Obj.magic 'foo)  (* 5097222 *)

       Standards variants:
       -------------------

       [ocaml_repr] is the tag corresponding to the constructor within the type.
       the way it works in the ocaml runtime is by partitioning the constructors regarding
       if they have some arguments or not, preserving the order, then assign increasing
       index withing each partition.
       Example:

       {[
         type t =                  (* no arg *)  (* args *)
           | A                       (* 0 *)
           | B of int                              (* 0 *)
           | C                       (* 1 *)
           | D of (float * string)                 (* 1 *)
           | E                       (* 2 *)
           | F                       (* 3 *)
           | G of string                           (* 2 *)
           | H of { x : int }                      (* 3 *)
       ]}
        v} *)
    val ocaml_repr : 'a ('b : any). ('a, 'b) t -> int

    (** Give back a way of constructing a value of that constructor from its arguments.

        Examples:

        {[
          type t =
            | A of (int * string)
            | B of int * float
            | C
            | D of
                { x : int
                ; y : string
                }
        ]}

        [create] will return something equivalent to:
        - tag_A : [Args (fun (d : (int * string) -> A d)]
        - tag_B : [Args (fun #(i, f) -> B (i, f))]
        - tag_C : [Const C]
        - tag_D : [Args (fun #(x, y) -> D { x; y })] *)
    val create : 'variant ('args : any). ('variant, 'args) t -> ('variant, 'args) create

    (** return the type_name of the arguments. might be used to perform some lookup based
        on it while building a computation for example *)
    val tyid : 'variant ('args : any). ('variant, 'args) t -> 'args Typename.t

    (** get the representation/computation of the arguments *)
    val traverse : 'variant ('args : any). ('variant, 'args) t -> 'args X.t

    (* used by the camlp4 extension to build type witnesses, or by some internal parts of
       typerep. you should feel bad if you need to use it in some user code *)
    val internal_use_only : 'a ('b : any). ('a, 'b) Tag_internal.t -> ('a, 'b) t
  end = struct
    include Tag_internal

    let label t = t.label
    let arity t = t.arity
    let args_labels t = t.args_labels
    let index t = t.index
    let ocaml_repr t = t.ocaml_repr
    let create t = t.create
    let tyid t = t.tyid
    let traverse t = t.rep
    let internal_use_only t = t
  end

  module Variant_internal = Variant_internal (Tag)

  module Variant : sig @@ portable
    include Variant(Tag).S

    val typename_of_t : 'a t -> 'a Typename.t

    (** Returns the number of tags of this variant type definition. *)
    val length : 'a t -> int

    (** Get the nth tag of this variant type, indexed from 0. *)
    val tag : 'a t -> int -> 'a tag

    (** {v
       Distinguish polymorphic variants and standard variants. Typically, polymorphic
       variants tags starts with the [`] character.
       Example
       polymorphic variant: type t = [ `A | `B ]
       standard variant:    type t = A | B
        v} *)
    val is_polymorphic : _ t -> bool

    (** Pattern matching on a value of this variant type. *)
    val value : 'a t -> 'a -> 'a value

    (** folding along the tags of the variant type *)
    val fold : 'a t -> init:'acc -> f:('acc -> 'a tag -> 'acc) -> 'acc

    (* used by the camlp4 extension to build type witnesses, or by some internal parts of
       typerep. you should feel bad if you need to use it in some user code *)
    val internal_use_only : 'a Variant_internal.t -> 'a t
  end = struct
    include Variant_internal

    let typename_of_t t = t.typename
    let length t = Iarray.length t.tags
    let tag t index = Iarray.get t.tags index
    let is_polymorphic t = t.polymorphic
    let value t = t.value
    let fold t ~init ~f = Iarray.fold ~f ~init t.tags
    let internal_use_only t = t
  end

  module Field_internal = Field_internal (X)

  (** Witness of a field, that is an item in a record type. The first parameter is the
      record type, the second is the type of the field. Example:
      {[
        type t =
          { x : int
          ; y : string
          }
      ]}
      This type has two fields. for each of them we'll have a corresponding [Field.t]

      val field_x : (t, int) Field.t val field_y : (t, string) Field.t *)
  module Field : sig @@ portable
    include Field

    (** The name of the field as it is given in the concrete syntax Examples:
        {[
          { x : int; (* "x" *)
                     foo : string; (* "foo" *)
                                   bar : float (* "bar" *) }
        ]} *)
    val label : 'a ('b : any). ('a, 'b) t -> string

    (** The 0-based index of the field in the list of all fields for this record type.
        Example:
        {[
          type t =
            { x : int (* 0 *)
            ; foo : string (* 1 *)
            ; bar : string (* 2 *)
            }
        ]} *)
    val index : 'a ('b : any). ('a, 'b) t -> int

    (** Field accessors. This corresponds to the dot operation. [Field.get bar_field t]
        returns the field [bar] of the record value [t], just the same as [t.bar] *)
    val get : 'record ('field : any). ('record, 'field) t -> 'record -> unit -> 'field

    (** return whether the field is mutable, i.e. whether its declaration is prefixed with
        the keyword [mutable] *)
    val is_mutable : 'a ('b : any). ('a, 'b) t -> bool

    (** return the type_name of the arguments. Might be used to perform some lookup based
        on it *)
    val tyid : 'record ('field : any). ('record, 'field) t -> 'field Typename.t

    (** get the computation of the arguments *)
    val traverse : 'record ('field : any). ('record, 'field) t -> 'field X.t

    (* used by the camlp4 extension to build type witnesses, or by some internal parts of
       typerep. you should feel bad if you need to use it in some user code *)
    val internal_use_only : 'a ('b : any). ('a, 'b) Field_internal.t -> ('a, 'b) t
  end = struct
    include Field_internal

    let label t = t.label
    let index t = t.index
    let get t = t.get
    let is_mutable t = t.is_mutable
    let tyid t = t.tyid
    let traverse t = t.rep
    let internal_use_only t = t
  end

  module Record_internal = Record_internal (Field)

  module Record : sig @@ portable
    include Record(Field).S

    val typename_of_t : 'a t -> 'a Typename.t

    (** Returns the number of fields of this record type definition. *)
    val length : 'a t -> int

    (** Get the nth field of this record type, indexed from 0. *)
    val field : 'a t -> int -> 'a field

    (** This is a low level metadata regarding the way the ocaml compiler represent the
        array underneath that is the runtime value of a record of type ['a] given a
        witness of type ['a t]. [has_double_array_tag w] returns [true] if the array that
        represents runtime values of this type is an optimized ocaml float array.
        Typically, this will be true for record where all fields are statically known as
        to be [floats].

        Note that you can't get this information dynamically by inspecting the typerep
        once it is applied, because there is at this point no way to tell whether one of
        the field is polymorphic in the type definition.

        This is computed lazily so that a self-referential named record can inspect other
        details of its own typerep during the process. *)
    val has_double_array_tag : _ t -> bool Portable_lazy.t

    (** Expose one direction of the isomorphism between a value of type ['a] and a value
        of type ['a fields]. Basically, given an encoding way of accessing the value of
        all the fields of a record, create that record and return it. *)
    val create : 'a t -> 'a fields -> 'a

    (** folding along the tags of the variant type *)
    val fold : 'a t -> init:'acc -> f:('acc -> 'a field -> 'acc) -> 'acc

    (* used by the camlp4 extension to build type witnesses, or by some internal parts of
       typerep. you should feel bad if you need to use it in some user code *)
    val internal_use_only : 'a Record_internal.t -> 'a t
  end = struct
    include Record_internal

    let typename_of_t t = t.typename
    let length t = Iarray.length t.fields
    let field t index = Iarray.get t.fields index
    let has_double_array_tag t = t.has_double_array_tag
    let create t = t.create
    let fold t ~init ~f = Iarray.fold ~f ~init t.fields
    let internal_use_only t = t
  end
end

module type%template [@modality p = portable] S = sig
  type ('a : any) t : value mod contended portable

  include module type of M [@modality p] (struct
      type nonrec ('a : any) t = 'a t
    end)
end

module type%template [@modality p = nonportable] S = sig
  type ('a : any) t

  include module type of M [@modality p] (struct
      type nonrec ('a : any) t = 'a t
    end)
end
