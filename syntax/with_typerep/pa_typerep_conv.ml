open StdLabels
open Camlp4
open PreCast

module Rewrite_tds = Pa_type_conv.Rewrite_tds

module Gen = struct
  include Pa_type_conv.Gen
  let idp _loc id = <:patt< $lid:id$ >>
  let ide _loc id = <:expr< $lid:id$ >>
end

module Array = struct
  include Array
  let mapi ~f array =
    let index = ref (-1) in
    let f x = incr index; f !index x in
    map ~f array

  let map2 ~f a1 a2 =
    let len = Array.length a1 in
    if len <> Array.length a2 then invalid_arg "Array.map2";
    if len = 0 then
      [||]
    else begin
      let a = Array.make len (f a1.(0) a2.(0)) in
      for i = 1 to len - 1 do
        a.(i) <- f a1.(i) a2.(i)
      done;
      a
    end

  let reduce_left ~f array =
    match array with
    | [||] -> None
    | [| x |] -> Some x
    | _ ->
      let length = Array.length array in
      let rec aux acc index =
        if index >= length then acc
        else
          let acc = f acc array.(index) in
          aux acc (succ index)
      in
      Some (aux array.(0) 1)

  let reduce_right ~f array =
    match array with
    | [||] -> None
    | [| x |] -> Some x
    | _ ->
      let length = Array.length array in
      let rec aux acc index =
        if index < 0 then acc
        else
          let acc = f array.(index) acc in
          aux acc (pred index)
      in
      let pred_length = pred length in
      Some (aux array.(pred_length) (pred pred_length))

  let fold_righti arr ~f ~init =
    let r = ref (length arr) in
    let f elt acc = decr r; f !r elt acc in
    fold_right ~f arr ~init
end

module List = struct
  include List
  (* let init ~f n =
   *   let rec aux acc index =
   *     if index < 0 then acc else
   *       let acc = f index :: acc in
   *       aux acc (pred index)
   *   in
   *   aux [] (pred n) *)

  let filter_map ~f list =
    let rec aux acc = function
      | [] -> List.rev acc
      | hd :: tl -> begin
        match f hd with
        | None -> aux acc tl
        | Some hd -> aux (hd::acc) tl
      end
    in
    aux [] list
end

(* Utility functions *)
let mk_rev_bindings loc fps =
  let coll (i, bindings, patts, vars) expr =
    let name = "v" ^ string_of_int i in
    let var_expr = Gen.ide loc name in
    let patt = Gen.idp loc name in
    let bindings = <:binding@loc< $patt$ = $expr$ and $bindings$ >> in
    i - 1, bindings, patt :: patts, var_expr :: vars
  in
  let n = List.length fps in
  let _, bindings, patts, expr =
    List.fold_left ~f:coll ~init:(n, Ast.BiNil loc, [], []) fps
  in
  bindings, patts, expr

let mk_bindings loc fps = mk_rev_bindings loc (List.rev fps)

(*
  Ocamlp4 is very confusing with tuple generation
*)
module Tuple : sig
  val expr : Ast.loc -> int -> f:(int -> Ast.expr) -> Ast.expr
  val patt : Ast.loc -> int -> f:(int -> Ast.patt) -> Ast.patt
  val ctyp : Ast.loc -> int -> f:(int -> Ast.ctyp) -> Ast.ctyp
end = struct
  let make build last n ~f =
    match n with
    | 1 -> f 0
    | n when n > 1 ->
      let rec aux acc i =
        let fi = f i in
        if i = 0
        then last fi acc
        else aux (build fi acc) (pred i)
      in
      aux (f (pred n)) (n - 2)
    | _ -> assert false

  let expr _loc n =
    let build a b = <:expr< $a$ , $b$ >> in
    let last a b = <:expr< ( $a$ , $b$ ) >> in
    make build last n

  let ctyp _loc n =
    let build a b = <:ctyp< $a$ * $b$ >> in
    let last a b = <:ctyp< ( $a$ * $b$ ) >> in
    make build last n

  let patt _loc n =
    let build a b = <:patt< $a$ , $b$ >> in
    let last a b = <:patt< ( $a$ , $b$ ) >> in
    make build last n
end

module Field_case = struct
  type t = {
    label : string;
    ctyp : Ast.ctyp;
    index : int;
  }
end

module Variant_case = struct
  type t = {
    label : string;
    ctyp : Ast.ctyp option;
    poly : bool;
    arity : int;
    patt : Ast.patt;
    expr : Ast.expr;
    index : int;
    arity_index : int;
  }

  let ocaml_repr _loc { label ; poly ; arity_index ; _ } =
    if poly
    then <:expr< Typerep_obj.repr_of_poly_variant `$label$ >>
    else <:expr< $`int:arity_index$ >>
end

module Branches = struct
  type t =
  | Fields of Field_case.t array
  | Variants of Variant_case.t array

  let length = function
    | Fields a -> Array.length a
    | Variants a -> Array.length a

  let compute ctyp =
    let _loc = Ast.loc_of_ctyp ctyp in
    match ctyp with
    | <:ctyp< { $fields$ } >> ->
      let fields = Array.of_list (Ast.list_of_ctyp fields []) in
      let mapi index = function
        |  <:ctyp< $lid:label$ : mutable $ctyp$ >>
        |  <:ctyp< $lid:label$ : $ctyp$ >>
          ->
          { Field_case.label ; ctyp ; index }
        | ctyp -> Gen.unknown_type ctyp "Env.branches(record)"
      in
      Fields (Array.mapi fields ~f:mapi)

    | <:ctyp< [ $alts$ ] >>
    | <:ctyp< [< $alts$ ] >>
    | <:ctyp< [> $alts$ ] >>
    | <:ctyp< [= $alts$ ] >> ->
      let rec extract = function
        | (   <:ctyp< `$_$ >>
              | <:ctyp< `$_$ of $_$ >>
              | <:ctyp< $uid:_$ >>
              | <:ctyp< $uid:_$ of $_$ >>
        ) as ctyp -> [ctyp]
        | <:ctyp< [ $row_fields$ ] >>
        | <:ctyp< [< $row_fields$ ] >>
        | <:ctyp< [> $row_fields$ ] >>
        | <:ctyp< [= $row_fields$ ] >> ->
          extract row_fields
        | <:ctyp< $tp1$ | $tp2$ >> -> extract tp1 @ extract tp2
        | ctyp -> Gen.unknown_type ctyp "Env.branches(extract variant)"
      in
      let cases =
        List.concat (List.map ~f:extract (Ast.list_of_ctyp alts []))
      in
      let cases = Array.of_list cases in
      let mk_ident _loc str =
        let first = str.[0] in
        if first >= 'A' && first <= 'Z' then <:ident< $uid:str$ >>
        else <:ident< $lid:str$ >>
      in
      let no_arg = ref 0 in
      let with_arg = ref 0 in
      let mapi index = function
        | <:ctyp< `$label$ >> ->
          let arity_index = !no_arg in
          incr no_arg;
          let patt = <:patt< `$label$ >> in
          let expr = <:expr< `$label$ >> in
          { Variant_case.
            label;
            ctyp = None;
            poly = true;
            arity = 0;
            patt;
            expr;
            index;
            arity_index;
          }
        | <:ctyp< `$label$ of $ctyp$ >> ->
          let patt = <:patt< `$label$ >> in
          let expr = <:expr< `$label$ >> in
          let ctyp = Some ctyp in
          let arity_index = !with_arg in
          incr with_arg;
          { Variant_case.
            label;
            ctyp;
            poly = true;
            arity = 1;
            patt;
            expr;
            index;
            arity_index;
          }
        | <:ctyp< $uid:label$ >> ->
          let id = mk_ident _loc label in
          let patt = <:patt< $id:id$ >> in
          let expr = <:expr< $id:id$ >> in
          let arity_index = !no_arg in
          incr no_arg;
          { Variant_case.
            label;
            ctyp = None;
            poly = false;
            arity = 0;
            patt;
            expr;
            index;
            arity_index;
          }
        | <:ctyp< $uid:label$ of $ctyp$ >> ->
          let args = Ast.list_of_ctyp ctyp [] in
          let args = Array.of_list args in
          let arity = Array.length args in
          let ctyp =
            let f i = args.(i) in
            Tuple.ctyp _loc arity ~f
          in
          let ctyp = Some ctyp in
          let id = mk_ident _loc label in
          let patt = <:patt< $id:id$ >> in
          let expr = <:expr< $id:id$ >> in
          let arity_index = !with_arg in
          incr with_arg;
          { Variant_case.
            label;
            ctyp;
            poly = false;
            arity;
            patt;
            expr;
            index;
            arity_index;
          }
        | ctyp -> Gen.unknown_type ctyp "Env.branches(variant)"
      in
      Variants (Array.mapi cases ~f:mapi)

    | ctyp -> Gen.unknown_type ctyp "Env.branches"
end

module Typerep_signature = struct
  let sig_of_type_definitions ~sig_of_one_def ~ctyp =
    let rec aux = function
      | Ast.TyDcl (loc, type_name, params, rhs, cl) ->
        sig_of_one_def ~loc ~type_name ~params ~rhs ~cl
      | Ast.TyAnd (loc, tp1, tp2) ->
        <:sig_item@loc<
          $aux tp1$;
          $aux tp2$
        >>
      | _ -> assert false
    in
    aux ctyp

  let sig_of_typerep_of_t ~loc ~type_name ~params =
    let t_with_params =
      let fold acc param =
        <:ctyp@loc< $acc$ $param$ >>
      in
      List.fold_left ~f:fold ~init:<:ctyp@loc< $lid:type_name$ >> params
    in
    let returned = <:ctyp@loc< Typerep.t $t_with_params$ >> in
    match params with
    | [] -> returned
    | params ->
      let fold param acc =
        let param = Gen.drop_variance_annotations param in
        let loc = Ast.loc_of_ctyp param in
        <:ctyp@loc< ( Typerep.t $param$ ) -> $acc$ >>
      in
      List.fold_right ~f:fold ~init:returned params

  let sig_of_typename_of_t ~loc ~type_name ~params =
    let t_with_params =
      let fold acc param =
        <:ctyp@loc< $acc$ $param$ >>
      in
      List.fold_left ~f:fold ~init:<:ctyp@loc< $lid:type_name$ >> params
    in
    let returned = <:ctyp@loc< Type_name.t $t_with_params$ >> in
    match params with
    | [] -> returned
    | params ->
      let fold param acc =
        let param = Gen.drop_variance_annotations param in
        let loc = Ast.loc_of_ctyp param in
        <:ctyp@loc< ( Type_name.t $param$ ) -> $acc$ >>
      in
      List.fold_right ~f:fold ~init:returned params

  let sig_of_one_def ~loc ~type_name ~params ~rhs:_ ~cl:_ =
    let typerep_of = sig_of_typerep_of_t ~loc ~type_name ~params in
    let typerep_of_t =
      <:sig_item@loc< value $lid: "typerep_of_" ^ type_name$ : $typerep_of$ >>
    in
    let typename_of = sig_of_typename_of_t ~loc ~type_name ~params in
    let typename_of_t =
      <:sig_item@loc< value $lid: "typename_of_" ^ type_name$ : $typename_of$ >>
    in
    <:sig_item@loc<
      $typerep_of_t$;
      $typename_of_t$;
    >>

  let sig_generator _ ctyp =
    <:sig_item< $sig_of_type_definitions ~sig_of_one_def ~ctyp$ >>

  let () =
    Pa_type_conv.add_sig_generator "typerep" sig_generator
end

module Typerep_implementation = struct

  module Env : sig
    type t
    val create :
      loc:Ast.loc -> type_name:string -> params:Ast.ctyp list -> ctyp:Ast.ctyp
      -> t

    val name_of_t : t -> string

    val arg_of_param : t -> string -> string

    val params_names : t -> string array
    val params_patts : t -> Ast.patt array

    val type_name_module_definition : t -> Ast.str_item

    val with_named : t -> Ast.expr -> Ast.expr

    val typerep_of_t_coerce : t -> Ast.ctyp option

    val typerep_abstract : t -> Ast.str_item

    module Record : sig

      val field_n_ident : t -> int -> string

      val fields :
        typerep_of_inlined:(env:t -> Ast.ctyp -> Ast.expr) -> t
        -> (int * string * Ast.expr) array

      val create : t -> Ast.expr

      val has_double_array_tag : t -> Ast.expr
    end

    module Variant : sig

      val tag_n_ident : t -> int -> string

      val tags :
        typerep_of_inlined:(env:t -> Ast.ctyp -> Ast.expr) -> t
        -> (int * string * Ast.expr) array

      val value : t -> Ast.expr

      val polymorphic : t -> Ast.expr
    end

  end = struct

    type t = {
      loc : Ast.loc;
      type_name : string;
      params : Ast.ctyp array;
      ctyp : Ast.ctyp;
      mutable alias : string option;
      mutable alias_def : Ast.str_item option;
      mutable params_names : string array option;
      mutable params_patts : Ast.patt array option;
      mutable branches : Branches.t option;
      mutable type_name_module_name : string option;
      mutable str_item_type_and_name : Ast.module_expr option;
    }
    let create ~loc ~type_name ~params ~ctyp =
      let params = Array.of_list params in
      {
        loc;
        type_name;
        params;
        ctyp;
        alias = None;
        alias_def = None;
        params_names = None;
        params_patts = None;
        branches = None;
        type_name_module_name = None;
        str_item_type_and_name = None;
      }

    (* t is given for alpha renaming if needed *)
    let param_name _t (name:string) = name
    let arg_of_param _t name = "_of_" ^ name
    let name_of_t t = "name_of_"^t.type_name

    let type_name_module_name t =
      match t.type_name_module_name with
      | Some res -> res
      | None ->
        let res = "Typename_of_"^t.type_name in
        t.type_name_module_name <- Some res;
        res

    let params_names t =
      match t.params_names with
      | Some res -> res
      | None ->
        let res = Array.map t.params
          ~f:(fun ty -> param_name t (Gen.get_tparam_id ty))
        in
        t.params_names <- Some res;
        res

    let params_patts t =
      match t.params_patts with
      | Some res -> res
      | None ->
        let res =
          let names = params_names t in
          Array.map names ~f:(fun s -> Gen.idp t.loc (arg_of_param t s))
        in
        t.params_patts <- Some res;
        res

    let with_named t expr =
      let _loc = t.loc in
      let type_name_module_name = type_name_module_name t in
      let name_t =
        let init = <:expr< $uid:type_name_module_name$ . $lid:"named"$ >> in
        let names = params_names t in
        Array.fold_left names ~init ~f:(fun acc name ->
          let arg = arg_of_param t name in
          <:expr< $acc$ $lid:arg$>>
        )
      in
      let name_of_t = name_of_t t in
      let fst = <:expr< $lid:name_of_t$ >> in
      let snd = <:expr< Some (lazy $expr$) >> in
      let args =
        let pair = [|fst;snd|] in
        Tuple.expr _loc 2 ~f:(fun i -> pair.(i))
      in
      let named = <:expr< Typerep.Named $args$ >> in
      let bind = <:binding< $lid:name_of_t$ = $name_t$ >> in
      <:expr< let $bind$ in $named$ >>

    let typerep_of_t_coerce t =
      let params = params_names t in
      if Array.length params = 0 then None else begin
        let _loc = t.loc in
        let returned =
          let fold acc name = <:ctyp< $acc$ '$lid:name$ >> in
          let init = <:ctyp< $lid:t.type_name$ >> in
          let t = Array.fold_left ~f:fold ~init params in
          <:ctyp< Typerep.t $t$ >>
        in
        let coerce =
          let fold name acc =
            let arg = <:ctyp< Typerep.t '$lid:name$ >> in
            <:ctyp< $arg$ -> $acc$ >>
          in
          Array.fold_right ~init:returned ~f:fold params
        in
        let reduce =
          Array.reduce_left ~f:(fun a b -> <:ctyp< $a$ $b$>>)
            (Array.map params ~f:(fun name -> <:ctyp< '$name$ >>))
        in
        match reduce with
        | None -> None
        | Some typevars -> Some <:ctyp< ! $typevars$ . $coerce$ >> (* forall *)
      end

    let str_item_type_and_name t =
      match t.str_item_type_and_name with
      | Some res -> res
      | None ->
        let res =
          let _loc = t.loc in
          let params =
            let params = Array.map (params_names t)
              ~f:(fun name -> <:ctyp< '$lid:name$ >>) in
            Array.to_list params
          in
          let prototype =
            let fold acc name = <:ctyp< $acc$ '$lid:name$ >> in
            let init = <:ctyp< $lid:t.type_name$ >> in
            Array.fold_left ~f:fold ~init (params_names t)
          in
          let tds = Ast.TyDcl (_loc, "t", params, prototype, []) in
          let type_t = Rewrite_tds.str_ _loc false tds in
          let name_def =
            let full_type_name = Printf.sprintf "%s.%s"
              (Pa_type_conv.get_conv_path ()) t.type_name
            in
            let binding = <:binding< $lid:"name"$ = $str:full_type_name$ >> in
            <:str_item< value $rec:<:rec_flag<>>$ $binding$ >>
          in
          let str_items = Ast.stSem_of_list [
            type_t;
            name_def;
          ] in
          let type_name_struct = <:module_expr< struct $str_items$ end >> in
          type_name_struct
        in
        t.str_item_type_and_name <- Some res;
        res

    let type_name_module_definition t =
      let name = type_name_module_name t in
      let _loc = t.loc in
      let type_arity = Array.length t.params in
      let make = <:module_expr< Type_named.$uid:"Make"^(string_of_int type_arity)$ >> in
      let type_name_struct = str_item_type_and_name t in
      let type_name_module = <:module_expr< $make$ $type_name_struct$ >> in
      let module_def =
        <:str_item< module $uid:name$ = $type_name_module$ >>
      in
      let typename_of_t =
        let body = <:expr< $uid:name$ . typename_of_t >> in
        let bnd = Gen.idp _loc ("typename_of_" ^ t.type_name) in
        let binding = <:binding< $bnd$ = $body$ >> in
        let rec_flag = <:rec_flag<>> in
        <:str_item< value $rec:rec_flag$ $binding$ >>
      in
      <:str_item<
        $module_def$;
        $typename_of_t$;
      >>

    let typerep_abstract t =
      let type_name_struct = str_item_type_and_name t in
      let _loc = t.loc in
      let type_arity = Array.length t.params in
      let make =
        <:module_expr< Type_abstract.$uid:"Make"^(string_of_int type_arity)$ >>
      in
      let type_abstract_module = <:module_expr< $make$ $type_name_struct$ >> in
      <:str_item< include $type_abstract_module$ >>

    let branches t =
      match t.branches with
      | Some res -> res
      | None ->
        let res = Branches.compute t.ctyp in
        t.branches <- Some res;
        res

    let field_or_tag_n_ident mode t n =
      let prefix =
        match mode with
        | `tag -> "tag"
        | `field -> "field"
      in
      if n < 0 || n > Branches.length (branches t) then assert false;
      prefix^(string_of_int n)

    module Record = struct
      let field_n_ident = field_or_tag_n_ident `field

      let fields ~typerep_of_inlined t =
        let _loc = t.loc in
        match branches t with
        | Branches.Variants _ -> assert false
        | Branches.Fields fields ->
          let get label = <:expr< fun $lid:"t"$ -> $lid:"t"$ . $lid:label$ >> in
          let map { Field_case.ctyp ; label ; index } =
            let fields = [|
              "label", <:expr< $str:label$ >>;
              "index", <:expr< $`int:index$ >>;
              "rep", typerep_of_inlined ~env:t ctyp;
              "tyid", <:expr< Type_name.create () >>;
              "get", get label;
            |] in
            let fields_binding =
              let map (name, expr) =
                let ident = <:ident< Typerep.Field_internal. $lid:name$ >> in
                <:rec_binding< $id:ident$ = $expr$ >>
              in
              Array.map ~f:map fields
            in
            let fields_binding =
              let reduce = Array.reduce_right fields_binding
                ~f:(fun field acc -> <:rec_binding< $field$ ; $acc$ >>)
              in
              match reduce with
              | Some fields -> fields
              | None -> assert false
            in
            index, label, <:expr< Typerep.Field.internal_use_only { $fields_binding$ } >>
          in
          Array.map ~f:map fields

    let has_double_array_tag t =
      match branches t with
      | Branches.Variants _ -> assert false
      | Branches.Fields fields ->
        let _loc = t.loc in
        (* the value must be a float else this segfaults.
           this is tested by the unit tests in case this property changes *)
        let magic = <:expr< Typerep_obj.double_array_value >> in
        let fields_binding =
          let map { Field_case.label ; _ } =
            <:rec_binding< $lid:label$ = $magic$ >>
          in
          Array.map ~f:map fields
        in
        let fields_binding =
          let reduce = Array.reduce_right fields_binding
            ~f:(fun field acc -> <:rec_binding< $field$ ; $acc$ >>)
          in
          match reduce with
          | Some fields -> fields
          | None -> assert false
        in
        let record = <:expr< { $fields_binding$ } >> in
        <:expr< Typerep_obj.has_double_array_tag $record$ >>

      let create t =
        let _loc = t.loc in
        let arg = "get" in
        let record =
          match branches t with
          | Branches.Variants _ -> assert false
          | Branches.Fields fields ->
            let fields_binding =
              let map { Field_case.label ; _ } =
                <:rec_binding< $lid:label$ = $lid:label$ >>
              in
              Array.map ~f:map fields
            in
            let fields_binding =
              let reduce = Array.reduce_right fields_binding
                ~f:(fun field acc -> <:rec_binding< $field$ ; $acc$ >>)
              in
              match reduce with
              | Some fields -> fields
              | None -> assert false
            in
            let record = <:expr< { $fields_binding$ } >> in
            let foldi index { Field_case.label ; _ } acc =
              let rhs = <:expr< $lid:arg$ $lid:field_n_ident t index$ >> in
              let bind = <:binding< $lid:label$ = $rhs$ >> in
              <:expr< let $bind$ in $acc$ >>
            in
            Array.fold_righti fields ~f:foldi ~init:record
        in
        Gen.abstract _loc
          [ <:patt< { Typerep.Record_internal.get = $lid:arg$ } >> ] record
    end

    module Variant = struct
      (* tag_0, tag_1, etc. *)
      let tag_n_ident = field_or_tag_n_ident `tag

      let polymorphic t =
        match branches t with
        | Branches.Fields _ -> assert false
        | Branches.Variants variants ->
          let _loc = t.loc in
          let polymorphic =
            Array.length variants = 0
            || variants.(0).Variant_case.poly
          in
          <:expr< $`bool:polymorphic$ >>

      let tags ~typerep_of_inlined t =
        let _loc = t.loc in
        match branches t with
        | Branches.Fields _ -> assert false
        | Branches.Variants tags ->
          let create { Variant_case.arity ; poly ; expr ; _ } =
            if arity = 0
            then
              <:expr< Typerep.Tag_internal.Const $expr$ >>
            else
              let arg_tuple i = "v"^(string_of_int i) in
              let patt, expr =
                let n = if poly then 1 else arity in
                let patt =
                  let f i = <:patt<$lid:arg_tuple i$ >> in
                  Tuple.patt _loc n ~f
                in
                let expr =
                  let f i = <:expr<$lid:arg_tuple i$ >> in
                  let args = Tuple.expr _loc n ~f in
                  <:expr< $expr$ $args$ >>
                in
                patt, expr
              in
              <:expr< Typerep.Tag_internal.Args (fun $patt$ -> $expr$) >>
          in
          let mapi index ({ Variant_case.ctyp ; label ; arity ; _ } as variant) =
            let rep =
              match ctyp with
              | Some ctyp -> typerep_of_inlined ~env:t ctyp
              | None -> <:expr< $lid:"typerep_of_tuple0"$>>
            in
            let tyid =
              if arity = 0
              then <:expr< typename_of_tuple0 >>
              else <:expr< Type_name.create () >>;
            in
            let tags = [|
              "label", <:expr< $str:label$ >>;
              "rep", rep;
              "arity", <:expr< $`int:arity$>>;
              "index", <:expr< $`int:index$ >>;
              "ocaml_repr", Variant_case.ocaml_repr _loc variant;
              "tyid", tyid;
              "create", create variant;
            |] in
            let tags_binding =
              let map (name, expr) =
                let ident = <:ident< Typerep.Tag_internal. $lid:name$ >> in
                <:rec_binding< $id:ident$ = $expr$ >>
              in
              Array.map ~f:map tags
            in
            let tags_binding =
              let reduce = Array.reduce_right tags_binding
                ~f:(fun tag acc -> <:rec_binding< $tag$ ; $acc$ >>)
              in
              match reduce with
              | Some tags -> tags
              | None -> assert false
            in
            index, label, <:expr< Typerep.Tag.internal_use_only { $tags_binding$ } >>
          in
          Array.mapi ~f:mapi tags

      let value t =
        match branches t with
        | Branches.Fields _ -> assert false
        | Branches.Variants tags ->
          let _loc = t.loc in
          let match_cases =
            let arg_tuple i = "v"^(string_of_int i) in
            let mapi index { Variant_case.arity ; poly ; patt ; _ } =
              let patt, value =
                if arity = 0 then patt, <:expr< $lid:"value_tuple0"$ >>
                else
                  let n = if poly then 1 else arity in
                  let patt =
                    let f i = <:patt<$lid:arg_tuple i$ >> in
                    let args = Tuple.patt _loc n ~f in
                    <:patt< $patt$ $args$ >>
                  in
                  let expr =
                    let f i = <:expr<$lid:arg_tuple i$ >> in
                    Tuple.expr _loc n ~f
                  in
                  patt, expr
              in
              let tag = <:expr< $lid:tag_n_ident t index$ >> in
              let prod = <:expr< Typerep.Variant_internal.Value ($tag$, $value$) >> in
              <:match_case< $patt$ -> $prod$ >>
            in
            Array.mapi ~f:mapi tags
          in
          let matching =
            let reduce = Array.reduce_left match_cases
              ~f:(fun acc case ->
                <:match_case< $acc$ | $case$ >>
              )
            in
            match reduce with
            | Some matching -> matching
            | None -> assert false
          in
          <:expr< fun [ $matching$ ] >>
    end
  end

  let mk_abst_call loc tn rev_path =
    <:expr@loc< $id:Gen.ident_of_rev_path loc (("typerep_of_" ^ tn) :: rev_path)$ >>

  (* Conversion of type paths *)
  let typerep_of_path_fun loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call loc tn rev_path
    | [] -> assert false  (* impossible *)

  let rec typerep_of_type ~env = function
    | <:ctyp@loc< $ty$ $param$ >> ->
      typerep_of_parametric ~typerep_of_type ~env  loc ~ty ~param
    | <:ctyp@loc< '$parm$ >> -> Gen.ide loc (Env.arg_of_param env parm)
    | <:ctyp@loc< $id:id$ >> -> typerep_of_path_fun loc id
    | <:ctyp< [ $row_fields$ ] >>
    | <:ctyp< [< $row_fields$ ] >>
    | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> -> typerep_of_variant ~env row_fields
    | <:ctyp< ( $tup:tuple$ ) >> -> typerep_of_tuple ~typerep_of_type ~env tuple
    | ctyp -> Gen.unknown_type ctyp "typerep_of_type"

  (* inlined is to be called on a type of a field or a variant case *)
  and typerep_of_inlined ~env = function
    | <:ctyp@loc< $ty$ $param$ >> ->
      typerep_of_parametric ~typerep_of_type:typerep_of_inlined ~env  loc ~ty ~param
    | <:ctyp@loc< '$parm$ >> -> Gen.ide loc (Env.arg_of_param env parm)
    | <:ctyp@loc< $id:id$ >> -> typerep_of_path_fun loc id
    | <:ctyp< ( $tup:tuple$ ) >> ->
      typerep_of_tuple ~typerep_of_type:typerep_of_inlined ~env tuple
    | ctyp -> Gen.unknown_type ctyp "unsupported inlined construction"

  and typerep_of_parametric ~typerep_of_type ~env loc ~ty ~param =
    let typerep_of_ty = typerep_of_type ~env ty in
    let typerep_of_param = typerep_of_type ~env param in
    <:expr@loc< $typerep_of_ty$ $typerep_of_param$ >>

  and typerep_of_tuple ~typerep_of_type ~env tuple =
    let loc = Ast.loc_of_ctyp tuple in
    let typereps =
      List.map ~f:(typerep_of_type ~env) (Ast.list_of_ctyp tuple [])
    in
    match typereps with
    | [single] -> single
    | _ ->
    let typerep_of_tuple =
      let len = List.length typereps in
      if len < 2 || len > 5
      then
        Gen.error tuple ~fn:"typerep impl_gen"
          ~msg:(Printf.sprintf "unsupported tuple arity %d. must be in [2,3,4,5]" len)
      else
        Gen.ide loc ("typerep_of_tuple"^(string_of_int len))
    in
    let bindings, _patts, vars = mk_bindings loc typereps in
    let in_expr = Gen.apply loc typerep_of_tuple vars in
    let expr = <:expr@loc< let $bindings$ in $in_expr$ >> in
    expr

  and typerep_of_record ~env ctyp =
    let _loc = Ast.loc_of_ctyp ctyp in
    let fields = Env.Record.fields ~typerep_of_inlined env in
    let field_ident i = Env.Record.field_n_ident env i in
    let fields_array =
      let fields =
        Array.map ~f:(fun (index,_,_) ->
          <:expr< Typerep.Record_internal.Field $lid:field_ident index$ >>
        ) fields
      in
      <:expr< [| $list:Array.to_list fields$ |] >>
    in
    let bindings = [|
      "typename", <:expr< Typerep.Named.typename_of_t $lid:Env.name_of_t env$ >>;
      "has_double_array_tag", Env.Record.has_double_array_tag env;
      "fields", fields_array;
      "create", Env.Record.create env;
    |] in
    let fields_binding =
      let map (name, _) =
        let ident = <:ident< Typerep.Record_internal. $lid:name$ >> in
        <:rec_binding< $id:ident$ = $lid:name$ >>
      in
      Array.map ~f:map bindings
    in
    let fields_binding =
      let reduce = Array.reduce_right fields_binding
        ~f:(fun field acc -> <:rec_binding< $field$ ; $acc$ >>)
      in
      match reduce with
      | Some fields -> fields
      | None -> assert false
    in
    let record =
      let fields = <:expr< Typerep.Record.internal_use_only { $fields_binding$ } >> in
      <:expr< Typerep.Record $fields$ >>
    in
    let record = Array.fold_right bindings ~f:(fun (id, expr) acc ->
      let bind = <:binding< $lid:id$ = $expr$ >> in
      <:expr< let $bind$ in $acc$ >>
    ) ~init:record
    in
    let record = Array.fold_right fields ~f:(fun (index, _, expr) acc ->
      let bind = <:binding< $lid:field_ident index$ = $expr$ >> in
      <:expr< let $bind$ in $acc$ >>
    ) ~init:record
    in
    record

  and typerep_of_variant ~env ctyp =
    let _loc = Ast.loc_of_ctyp ctyp in
    let tags = Env.Variant.tags ~typerep_of_inlined env in
    let tag_ident i = Env.Variant.tag_n_ident env i in
    let tags_array =
      let tags =
        Array.map ~f:(fun (index,_,_) ->
          <:expr< Typerep.Variant_internal.Tag $lid:tag_ident index$ >>
        ) tags
      in
      <:expr< [| $list:Array.to_list tags$ |] >>
    in
    let bindings = [|
      "typename", <:expr< Typerep.Named.typename_of_t $lid:Env.name_of_t env$ >>;
      "tags", tags_array;
      "polymorphic", Env.Variant.polymorphic env;
      "value", Env.Variant.value env;
    |] in
    let tags_binding =
      let map (name, _) =
        let ident = <:ident< Typerep.Variant_internal. $lid:name$ >> in
        <:rec_binding< $id:ident$ = $lid:name$ >>
      in
      Array.map ~f:map bindings
    in
    let tags_binding =
      let reduce = Array.reduce_right tags_binding
        ~f:(fun tag acc -> <:rec_binding< $tag$ ; $acc$ >>)
      in
      match reduce with
      | Some tags -> tags
      | None -> assert false
    in
    let variant =
      let tags = <:expr< Typerep.Variant.internal_use_only { $tags_binding$ } >> in
      <:expr< Typerep.Variant $tags$ >>
    in
    let variant = Array.fold_right bindings ~f:(fun (id, expr) acc ->
      let bind = <:binding< $lid:id$ = $expr$ >> in
      <:expr< let $bind$ in $acc$ >>
    ) ~init:variant
    in
    let variant = Array.fold_right tags ~f:(fun (index, _, expr) acc ->
      let bind = <:binding< $lid:tag_ident index$ = $expr$ >> in
      <:expr< let $bind$ in $acc$ >>
    ) ~init:variant
    in
    variant

  let impl_of_one_def ~is_recursive ~loc ~type_name ~params ~rhs:ctyp =
    let env = Env.create ~loc ~type_name ~params ~ctyp in
    let body =
      let switch ctyp =
        Gen.switch_tp_def ctyp
          ~alias:(fun (_:Loc.t) ctyp -> typerep_of_type ~env ctyp)
          ~sum:(fun (_:Loc.t) ctyp -> typerep_of_variant ~env ctyp)
          ~record:(fun (_:Loc.t) ctyp -> typerep_of_record ~env ctyp)
          ~variants:(fun (_:Loc.t) ctyp -> typerep_of_variant ~env ctyp)
          ~mani:(fun (_:Loc.t) _tp1 _tp2 ->
            Gen.error ctyp ~fn:"typerep impl_gen" ~msg:"unsupported mani type def"
          )
          ~nil:(fun (_:Loc.t) ->
            Gen.error ctyp ~fn:"typerep impl_gen" ~msg:"unsupported nil type def"
          )
      in
      <:expr@loc< $switch ctyp$ >>
    in
    let body = Env.with_named env body in
    let params_names = Env.params_names env in
    let params_patts = Env.params_patts env in
    (* Add type annotations to parameters, at least to avoid the unused type warning. *)
    let arguments = Array.map2 params_names params_patts ~f:(fun name patt ->
      let loc = Ast.loc_of_patt patt in
      <:patt@loc< ($patt$ : Typerep.t $lid:name$) >>)
    in
    let body = Gen.abstract loc (Array.to_list arguments) body in
    let body = Array.fold_right params_names ~init:body ~f:(fun name acc ->
      <:expr@loc< fun (type $name$) -> $acc$ >> )
    in
    let body =
      match Env.typerep_of_t_coerce env with
      | Some coerce ->
        <:expr@loc< ($body$ : $coerce$) >>
      | None -> body
    in
    let bnd = Gen.idp loc ("typerep_of_" ^ type_name) in
    let binding = <:binding@loc< $bnd$ = $body$ >> in
    let rec_flag =
      if is_recursive
      then <:rec_flag< rec >>
      else <:rec_flag<>>
    in
    let typerep = <:str_item@loc< value $rec:rec_flag$ $binding$ >> in
    Ast.stSem_of_list [
      Env.type_name_module_definition env;
      typerep;
    ]

  let with_typerep rec_ ctyp =
    match ctyp with
    | Ast.TyDcl (loc, type_name, params, rhs, _cl) ->
      let is_recursive = rec_ && Gen.type_is_recursive type_name rhs in
      impl_of_one_def ~is_recursive ~loc ~type_name ~params ~rhs
    | _ ->
      Gen.error ctyp ~fn:"typerep impl_gen" ~msg:"unsupported type def"

  let with_typerep_abstract _ ctyp =
    match ctyp with
    | Ast.TyDcl (loc, type_name, params, ctyp, _cl) ->
      let env = Env.create ~loc ~type_name ~params ~ctyp in
      Env.typerep_abstract env
    | _ ->
      Gen.error ctyp ~fn:"typerep impl_gen" ~msg:"unsupported type def"

  module Config = struct
    type t = {
      abstract : bool;
      warn_23_field : unit;
    }

    let default = {
      abstract = false;
      warn_23_field = ();
    }

    let gram_entry : t Gram.Entry.t  = Gram.Entry.mk "typerep_arguments"

    EXTEND Gram
      GLOBAL: gram_entry;
      typerep_arg: [[
        LIDENT "abstract" -> (fun acc -> { acc with abstract = true })
      | id = LIDENT ->
        failwith (Printf.sprintf "Unknown typerep argument %S" id);
      ]];
      gram_entry: [[
        v = LIST0 typerep_arg SEP ";" ; `EOI ->
        List.fold_left v
          ~f:(fun acc f -> f acc)
          ~init:default
      ]];
    END

  end

  let () = Pa_type_conv.add_generator_with_arg "typerep" Config.gram_entry
    (fun conf rec_ ctyp ->
      let config = match conf with None -> Config.default | Some conf -> conf in
      if config.Config.abstract
      then with_typerep_abstract rec_ ctyp
      else with_typerep rec_ ctyp
    )
end
