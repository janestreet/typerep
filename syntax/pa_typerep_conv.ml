open StdLabels
open Camlp4
open PreCast

module Rewrite_tds = Pa_type_conv.Rewrite_tds


module Gen = struct
  include Pa_type_conv.Gen
  let idp loc id = <:patt< $lid:id$ >>
  let ide loc id = <:expr< $lid:id$ >>
  let let_in loc list_lid_expr body =
    List.fold_right list_lid_expr ~init:body ~f:(fun (lid, expr) body ->
      <:expr< let $lid:lid$ = $expr$ in $body$ >>)
end

module List = struct
  include List
  let init ~f n =
    let rec aux acc index =
      if index < 0 then acc else
        let acc = f index :: acc in
        aux acc (pred index)
    in
    aux [] (pred n)

  let fold_righti list ~f ~init =
    let length = length list in
    let (acc, _) =
      fold_right
        ~f:(fun el (acc, index) -> let acc = f index el acc in (acc, pred index))
        ~init:(init, pred length)
        list
    in
    acc

  let mapi ~f list =
    let rev, _ =
      fold_left
        ~f:(fun (acc, index) el -> f index el :: acc, succ index)
        ~init:([], 0)
        list
    in
    List.rev rev
end

(* camlp4 is very confusing with its tuple representation *)
module Tuple : sig
  val expr : Ast.loc -> Ast.expr list -> Ast.expr
  val patt : Ast.loc -> Ast.patt list -> Ast.patt
  val ctyp : Ast.loc -> Ast.ctyp list -> Ast.ctyp
end = struct
  let make fct = function
    | [] -> assert false
    | [ hd ] -> hd
    | (_ :: _) as list -> fct list
  let expr loc = make (fun list -> <:expr< ($tup:Ast.exCom_of_list list$) >>)
  let patt loc = make (fun list -> <:patt< ($tup:Ast.paCom_of_list list$) >>)
  let ctyp loc = make (fun list -> <:ctyp< ($tup:Ast.tyCom_of_list list$) >>)
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
    index : int;
    arity_index : int;
  }

  let patt ~loc t =
    let label = t.label in
    if t.poly then <:patt< `$label$ >> else <:patt< $uid:label$ >>

  let expr ~loc t =
    let label = t.label in
    if t.poly then <:expr< `$label$ >> else <:expr< $uid:label$ >>

  let ocaml_repr ~loc { label ; poly ; arity_index ; _ } =
    if poly
    then <:expr< Typerep_lib.Std.Typerep_obj.repr_of_poly_variant `$label$ >>
    else <:expr< $`int:arity_index$ >>
end

module Branches = struct
  let fields fields =
    let fields = Ast.list_of_ctyp fields [] in
    let mapi index = function
      |  <:ctyp< $lid:label$ : mutable $ctyp$ >>
      |  <:ctyp< $lid:label$ : $ctyp$ >>
        ->
        { Field_case.label ; ctyp ; index }
      | ctyp -> Gen.unknown_type ctyp "Util.branches(record)"
    in
    List.mapi fields ~f:mapi

  let variants alts =
      (* duplicates like [ `A | `B | `A ] cause warnings in the generated code (duplicated
         patterns), so we don't have to deal with them. *)
      let rec extract = function
        | <:ctyp< [ $row_fields$ ] >>
        | <:ctyp< [< $row_fields$ ] >>
        | <:ctyp< [> $row_fields$ ] >>
        | <:ctyp< [= $row_fields$ ] >> ->
          extract row_fields
        | <:ctyp< $tp1$ | $tp2$ >> -> extract tp1 @ extract tp2
        | ctyp -> [ctyp]
      in
      let cases = extract alts in
      let no_arg = let r = ref (-1) in fun () -> incr r; !r in
      let with_arg = let r = ref (-1) in fun () -> incr r; !r in
      let mapi index = function
        | <:ctyp< `$label$ >> ->
          { Variant_case.
            label;
            ctyp = None;
            poly = true;
            arity = 0;
            index;
            arity_index = no_arg ();
          }
        | <:ctyp< `$label$ of $ctyp$ >> ->
          { Variant_case.
            label;
            ctyp = Some ctyp;
            poly = true;
            arity = 1;
            index;
            arity_index = with_arg ();
          }
        | <:ctyp< $uid:label$ >> ->
          { Variant_case.
            label;
            ctyp = None;
            poly = false;
            arity = 0;
            index;
            arity_index = no_arg ();
          }
        | <:ctyp@loc< $uid:label$ of $ctyp$ >> ->
          let args = Ast.list_of_ctyp ctyp [] in
          let arity = List.length args in
          let ctyp = Tuple.ctyp loc args in
          { Variant_case.
            label;
            ctyp = Some ctyp;
            poly = false;
            arity;
            index;
            arity_index = with_arg ();
          }
        | ctyp -> Gen.unknown_type ctyp "Util.branches(variant)"
      in
      List.mapi cases ~f:mapi
end

module Typerep_signature = struct
  let sig_of_type_definitions ~sig_of_one_def ~ctyp =
    let rec aux = function
      | Ast.TyDcl (loc, type_name, params, rhs, cl) ->
        sig_of_one_def ~loc ~type_name ~params ~rhs ~cl
      | Ast.TyAnd (loc, tp1, tp2) ->
        <:sig_item<
          $aux tp1$;
          $aux tp2$
        >>
      | _ -> assert false
    in
    aux ctyp

  let sig_of_of_t make_ty ~loc ~type_name ~params =
    let t_with_params =
      let fold acc param =
        <:ctyp< $acc$ $param$ >>
      in
      List.fold_left ~f:fold ~init:<:ctyp< $lid:type_name$ >> params
    in
    let returned = <:ctyp< $make_ty t_with_params$ >> in
    let fold param acc =
      let param = Gen.drop_variance_annotations param in
      let loc = Ast.loc_of_ctyp param in
      <:ctyp< $make_ty param$ -> $acc$ >>
    in
    List.fold_right ~f:fold ~init:returned params

  let sig_of_typerep_of_t ~loc =
    let make_ty params = <:ctyp< Typerep_lib.Std.Typerep.t $params$ >> in
    sig_of_of_t make_ty ~loc

  let sig_of_typename_of_t ~loc =
    let make_ty params = <:ctyp< Typerep_lib.Std.Typename.t $params$ >> in
    sig_of_of_t make_ty ~loc

  let sig_of_one_def ~loc ~type_name ~params ~rhs:_ ~cl:_ =
    let typerep_of = sig_of_typerep_of_t ~loc ~type_name ~params in
    let typename_of = sig_of_typename_of_t ~loc ~type_name ~params in
    <:sig_item<
      value $lid: "typerep_of_"  ^ type_name$ : $typerep_of$;
      value $lid: "typename_of_" ^ type_name$ : $typename_of$;
    >>

  let sig_generator _rec ctyp =
    sig_of_type_definitions ~sig_of_one_def ~ctyp

  let () =
    Pa_type_conv.add_sig_generator "typerep" sig_generator
end

module Typerep_implementation = struct

  module Util : sig

    val typename_field : loc:Ast.loc -> type_name:string option -> Ast.expr

    val arg_of_param : string -> string

    val params_names : params:Ast.ctyp list -> string list
    val params_patts : loc:Ast.loc -> params_names:string list -> Ast.patt list

    val type_name_module_definition :
      loc:Ast.loc -> type_name:string -> params_names:string list -> Ast.str_item

    val with_named :
      loc:Ast.loc -> type_name:string -> params_names:string list -> Ast.expr -> Ast.expr

    val typerep_of_t_coerce :
      loc:Ast.loc -> type_name:string -> params_names:string list -> Ast.ctyp option

    val typerep_abstract :
      loc:Ast.loc -> type_name:string -> params_names:string list -> Ast.str_item

    module Record : sig

      val field_n_ident : fields:(Field_case.t list) -> int -> string

      val fields :
        loc:Ast.loc
        -> typerep_of_type:(Ast.ctyp -> Ast.expr)
        -> fields:Field_case.t list
        -> (int * string * Ast.expr) list

      val create : loc:Ast.loc -> fields:Field_case.t list -> Ast.expr

      val has_double_array_tag : loc:Ast.loc -> fields:Field_case.t list -> Ast.expr
    end

    module Variant : sig

      val tag_n_ident : variants:(Variant_case.t list) -> int -> string

      val tags :
        loc:Ast.loc
        -> typerep_of_type:(Ast.ctyp -> Ast.expr)
        -> variants:Variant_case.t list
        -> (int * Ast.expr) list

      val value : loc:Ast.loc -> variants:Variant_case.t list -> Ast.expr

      val polymorphic : loc:Ast.loc -> variants:Variant_case.t list -> Ast.expr
    end

  end = struct

    let str_item_type_and_name loc ~params_names ~type_name =
      let params =
        List.map params_names
          ~f:(fun name -> <:ctyp< '$lid:name$ >>)
      in
      let prototype =
        let fold acc name = <:ctyp< $acc$ '$lid:name$ >> in
        let init = <:ctyp< $lid:type_name$ >> in
        List.fold_left ~f:fold ~init params_names
      in
      let tds = Ast.TyDcl (loc, "t", params, prototype, []) in
      let type_t = Rewrite_tds.str_ loc false tds in
      let name_def =
        let full_type_name = Printf.sprintf "%s.%s"
          (Pa_type_conv.get_conv_path ()) type_name
        in
        <:str_item< value name = $str:full_type_name$ >>
      in
      <:module_expr< struct $type_t$; $name_def$; end >>

    let arg_of_param name = "_of_" ^ name
    let name_of_t ~type_name = "name_of_" ^ type_name

    let typename_field ~loc ~type_name =
      match type_name with
      | None ->
        <:expr< Typerep_lib.Std.Typename.create () >>
      | Some type_name ->
        <:expr< Typerep_lib.Std.Typerep.Named.typename_of_t
          $lid:name_of_t ~type_name$ >>

    let params_names ~params =
      List.map params ~f:(fun ty -> Gen.get_tparam_id ty)

    let params_patts ~loc ~params_names =
      List.map params_names ~f:(fun s -> Gen.idp loc (arg_of_param s))

    let type_name_module_name ~type_name = "Typename_of_" ^ type_name

    let with_named ~loc ~type_name ~params_names expr =
      let name_t =
        let init = <:expr< $uid:type_name_module_name ~type_name$.named >> in
        List.fold_left params_names ~init ~f:(fun acc name ->
          let arg = arg_of_param name in
          <:expr< $acc$ $lid:arg$>>
        )
      in
      let name_of_t = name_of_t ~type_name in
      let args = <:expr< ( $lid:name_of_t$, Some (lazy $expr$) ) >> in
      <:expr< let $lid:name_of_t$ = $name_t$ in
              Typerep_lib.Std.Typerep.Named $args$ >>

    let typerep_of_t_coerce ~loc ~type_name ~params_names =
      match params_names with
      | [] -> None
      | hd :: tl ->
        let returned =
          let fold acc name = <:ctyp< $acc$ '$lid:name$ >> in
          let init = <:ctyp< $lid:type_name$ >> in
          let t = List.fold_left ~f:fold ~init params_names in
          <:ctyp< Typerep_lib.Std.Typerep.t $t$ >>
        in
        let coerce =
          let fold name acc =
            let arg = <:ctyp< Typerep_lib.Std.Typerep.t '$lid:name$ >> in
            <:ctyp< $arg$ -> $acc$ >>
          in
          List.fold_right ~init:returned ~f:fold params_names
        in
        let f name = <:ctyp< '$name$ >> in
        let typevars =
          List.fold_left ~f:(fun a b -> <:ctyp< $a$ $f b$>>) ~init:(f hd) tl
        in
        Some <:ctyp< ! $typevars$ . $coerce$ >> (* forall *)

    let type_name_module_definition ~loc ~type_name ~params_names =
      let name = type_name_module_name ~type_name in
      let type_arity = List.length params_names in
      let make = <:module_expr< Typerep_lib.Std.Make_typename.$uid:"Make"
        ^ (string_of_int type_arity)$ >>
      in
      let type_name_struct =
        str_item_type_and_name loc ~params_names ~type_name
      in
      let type_name_module = <:module_expr< $make$ $type_name_struct$ >> in
      let module_def =
        <:str_item< module $uid:name$ = $type_name_module$ >>
      in
      let typename_of_t =
        let lid = "typename_of_" ^ type_name in
        <:str_item< value $lid:lid$ = $uid:name$.typename_of_t >>
      in
      <:str_item<
        $module_def$;
        $typename_of_t$;
      >>

    let typerep_abstract ~loc ~type_name ~params_names =
      let type_name_struct =
        str_item_type_and_name loc ~params_names ~type_name
      in
      let type_arity = List.length params_names in
      let make =
        <:module_expr< Typerep_lib.Std.Type_abstract.$uid:"Make"
        ^ (string_of_int type_arity)$ >>
      in
      <:str_item< include $make$ $type_name_struct$ >>

    let field_or_tag_n_ident prefix ~list n =
      if n < 0 || n > List.length list then assert false;
      prefix ^ string_of_int n

    module Record = struct
      let field_n_ident ~fields:list = field_or_tag_n_ident "field" ~list

      let fields ~loc ~typerep_of_type ~fields =
          let map { Field_case.ctyp ; label ; index } =
            let rep = typerep_of_type ctyp in
            index, label, <:expr<
             Typerep_lib.Std.Typerep.Field.internal_use_only
               { Typerep_lib.Std.Typerep.Field_internal.
                 label  = $str:label$;
                 index  = $`int:index$;
                 rep    = $rep$;
                 tyid   = Typerep_lib.Std.Typename.create ();
                 get    = (fun t -> t.$lid:label$);
               }
            >>
          in
          List.map ~f:map fields

      let has_double_array_tag ~loc ~fields =
          let fields_binding =
            let map { Field_case.label ; _ } =
              (* The value must be a float else this segfaults.  This is tested by the
                 unit tests in case this property changes. *)
              <:rec_binding< $lid:label$ =
              Typerep_lib.Std.Typerep_obj.double_array_value >>
            in
            List.map ~f:map fields
          in
          <:expr< Typerep_lib.Std.Typerep_obj.has_double_array_tag
            { $list:fields_binding$ } >>

      let create ~loc ~fields =
        let record =
          (* Calling [get] on the fields from left to right matters, so that iteration
             goes left to right too. *)
          let fields_binding =
            let map { Field_case.label ; _ } = <:rec_binding< $lid:label$ >> in
            List.map ~f:map fields
          in
          let record = <:expr< { $list:fields_binding$ } >> in
          let foldi index' { Field_case.label ; index; _ } acc =
            if index <> index' then assert false;
            let rhs = <:expr< get $lid:field_n_ident ~fields index$ >> in
            <:expr< let $lid:label$ = $rhs$ in $acc$ >>
          in
          List.fold_righti fields ~f:foldi ~init:record
        in
        <:expr< fun { Typerep_lib.Std.Typerep.Record_internal.get = get }
        -> $record$ >>
    end

    module Variant = struct
      (* tag_0, tag_1, etc. *)
      let tag_n_ident ~variants:list = field_or_tag_n_ident "tag" ~list

      let polymorphic ~loc ~variants =
          let polymorphic =
            match variants with
            | []      -> true
            | hd :: _ -> hd.Variant_case.poly
          in
          <:expr< $`bool:polymorphic$ >>

      let tags ~loc ~typerep_of_type ~variants =
          let create ({ Variant_case.arity ; _ } as variant) =
            let constructor = Variant_case.expr ~loc variant in
            if arity = 0
            then
              <:expr< Typerep_lib.Std.Typerep.Tag_internal.Const $constructor$ >>
            else
              let arg_tuple i = "v" ^ string_of_int i in
              let patt, expr =
                let patt =
                  let f i = <:patt< $lid:arg_tuple i$ >> in
                  Tuple.patt loc (List.init arity ~f)
                in
                let expr =
                  let f i = <:expr< $lid:arg_tuple i$ >> in
                  let args = Tuple.expr loc (List.init arity ~f) in
                  <:expr< $constructor$ $args$ >>
                in
                patt, expr
              in
              <:expr< Typerep_lib.Std.Typerep.Tag_internal.Args
                (fun $patt$ -> $expr$) >>
          in
          let mapi index' ({ Variant_case.ctyp ; label ; arity ; index ; _ } as variant) =
            if index <> index' then assert false;
            let rep, tyid =
              match ctyp with
              | Some ctyp ->
                typerep_of_type ctyp, <:expr< Typerep_lib.Std.Typename.create () >>
              | None ->
                <:expr< typerep_of_tuple0 >>, <:expr< typename_of_tuple0 >>
            in
            let label_string = Pa_type_conv.Gen.regular_constr_of_revised_constr label in
            index, <:expr<
             Typerep_lib.Std.Typerep.Tag.internal_use_only
               { Typerep_lib.Std.Typerep.Tag_internal.
                 label       = $str:label_string$;
                 rep         = $rep$;
                 arity       = $`int:arity$;
                 index       = $`int:index$;
                 ocaml_repr  = $Variant_case.ocaml_repr ~loc variant$;
                 tyid        = $tyid$;
                 create      = $create variant$;
               }
            >>
          in
          List.mapi ~f:mapi variants

      let value ~loc ~variants =
          let match_cases =
            let arg_tuple i = "v" ^ string_of_int i in
            let mapi index' ({ Variant_case.arity ; index ; _ } as variant) =
              if index <> index' then assert false;
              let constructor = Variant_case.patt ~loc variant in
              let patt, value =
                if arity = 0 then constructor, <:expr< value_tuple0 >>
                else
                  let patt =
                    let f i = <:patt< $lid:arg_tuple i$ >> in
                    let args = Tuple.patt loc (List.init arity ~f) in
                    <:patt< $constructor$ $args$ >>
                  in
                  let expr =
                    let f i = <:expr< $lid:arg_tuple i$ >> in
                    Tuple.expr loc (List.init arity ~f)
                  in
                  patt, expr
              in
              let tag = <:expr< $lid:tag_n_ident ~variants index$ >> in
              let prod = <:expr< Typerep_lib.Std.Typerep.Variant_internal.Value
                ($tag$, $value$) >>
              in
              <:match_case< $patt$ -> $prod$ >>
            in
            List.mapi ~f:mapi variants
          in
          <:expr< fun [ $list:match_cases$ ] >>
    end
  end

  let mk_abst_call loc tn rev_path =
    <:expr< $id:Gen.ident_of_rev_path loc (("typerep_of_" ^ tn) :: rev_path)$ >>

  (* Conversion of type paths *)
  let typerep_of_path_fun loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call loc tn rev_path
    | [] -> assert false

  let rec typerep_of_type = function
    | <:ctyp@loc< $ty$ $param$ >> ->
      typerep_of_type_app loc ~ty ~param
    | <:ctyp@loc< '$parm$ >> -> Gen.ide loc (Util.arg_of_param parm)
    | <:ctyp@loc< $id:id$ >> -> typerep_of_path_fun loc id
    | <:ctyp< [< $row_fields$ ] >>
    | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> -> typerep_of_variant ~type_name:None row_fields
    | <:ctyp< ( $tup:tuple$ ) >> -> typerep_of_tuple tuple
    | ctyp -> Gen.unknown_type ctyp "typerep_of_type"

  and typerep_of_type_app loc ~ty ~param =
    let typerep_of_ty = typerep_of_type ty in
    let typerep_of_param = typerep_of_type param in
    <:expr< $typerep_of_ty$ $typerep_of_param$ >>

  and typerep_of_tuple tuple =
    let loc = Ast.loc_of_ctyp tuple in
    let typereps = List.map (Ast.list_of_ctyp tuple []) ~f:typerep_of_type in
    let typerep_of_tuple =
      let len = List.length typereps in
      if len < 2 || len > 5
      then
        Gen.error tuple ~fn:"typerep impl_gen"
          ~msg:(Printf.sprintf "unsupported tuple arity %d. must be in {2,3,4,5}" len)
      else
        Gen.ide loc ("typerep_of_tuple" ^ string_of_int len)
    in
    Gen.apply loc typerep_of_tuple typereps


  and typerep_of_record ~type_name ctyp =
    let loc = Ast.loc_of_ctyp ctyp in
    let fields = Branches.fields ctyp in
    let field_ident i = Util.Record.field_n_ident ~fields i in
    let indexed_fields = Util.Record.fields ~loc ~typerep_of_type ~fields in
    let fields_array =
      let fields =
        List.map ~f:(fun (index,_,_) ->
          <:expr< Typerep_lib.Std.Typerep.Record_internal.Field
            $lid:field_ident index$ >>
        ) indexed_fields
      in
      <:expr< [| $list:fields$ |] >>
    in
    let bindings = [
      "typename", Util.typename_field ~loc ~type_name:(Some type_name);
      "has_double_array_tag", Util.Record.has_double_array_tag ~loc ~fields;
      "fields", fields_array;
      "create", Util.Record.create ~loc ~fields;
    ] in
    let fields_binding =
      let map (name, _) =
        <:rec_binding< Typerep_lib.Std.Typerep.Record_internal.$lid:name$ >>
      in
      List.map ~f:map bindings
    in
    let record =
      let fields =
        <:expr< Typerep_lib.Std.Typerep.Record.internal_use_only
          { $list:fields_binding$ } >>
      in
      <:expr< Typerep_lib.Std.Typerep.Record $fields$ >>
    in
    let record = Gen.let_in loc bindings record in
    let record = List.fold_right indexed_fields ~f:(fun (index, _, expr) acc ->
      <:expr< let $lid:field_ident index$ = $expr$ in $acc$ >>
    ) ~init:record
    in
    record

  and typerep_of_variant ~type_name ctyp =
    let loc = Ast.loc_of_ctyp ctyp in
    let variants = Branches.variants ctyp in
    let tags = Util.Variant.tags ~loc ~typerep_of_type ~variants in
    let tag_ident i = Util.Variant.tag_n_ident ~variants i in
    let tags_array =
      let tags =
        List.map ~f:(fun (index,_) ->
          <:expr< Typerep_lib.Std.Typerep.Variant_internal.Tag $lid:tag_ident index$ >>
        ) tags
      in
      <:expr< [| $list:tags$ |] >>
    in
    let bindings = [
      "typename", Util.typename_field ~loc ~type_name;
      "tags", tags_array;
      "polymorphic", Util.Variant.polymorphic ~loc ~variants;
      "value", Util.Variant.value ~loc ~variants;
    ] in
    let tags_binding =
      let map (name, _) =
        <:rec_binding< Typerep_lib.Std.Typerep.Variant_internal.
          $lid:name$ = $lid:name$ >>
      in
      List.map ~f:map bindings
    in
    let variant =
      let tags = <:expr< Typerep_lib.Std.Typerep.Variant.internal_use_only
        { $list:tags_binding$ } >> in
      <:expr< Typerep_lib.Std.Typerep.Variant $tags$ >>
    in
    let variant = Gen.let_in loc bindings variant in
    let variant = List.fold_right tags ~f:(fun (index, expr) acc ->
      <:expr< let $lid:tag_ident index$ = $expr$ in $acc$ >>
    ) ~init:variant
    in
    variant

  let impl_of_one_def ~loc ~type_name ~params ~rhs:ctyp =
    let rec body ctyp =
      Gen.switch_tp_def ctyp
        ~alias:(fun (_:Loc.t) ctyp -> typerep_of_type ctyp)
        ~sum:(fun (_:Loc.t) -> typerep_of_variant ~type_name:(Some type_name))
        ~record:(fun (_:Loc.t) -> typerep_of_record ~type_name)
        ~variants:(fun (_:Loc.t) -> typerep_of_variant ~type_name:(Some type_name))
        ~mani:(fun (_:Loc.t) _tp1 ctyp -> body ctyp)
        ~nil:(fun loc ->
          Loc.raise loc (Failure "typerep cannot be applied on abstract types, except \
                                  like 'type t with typerep(abstract)'")
        )
    in
    let body = body ctyp in
    let params_names = Util.params_names ~params in
    let params_patts = Util.params_patts ~loc ~params_names in
    let body = Util.with_named ~loc ~type_name ~params_names body in
    let arguments = List.map2 params_names params_patts ~f:(fun name patt ->
      (* Add type annotations to parameters, at least to avoid the unused type warning. *)
      let loc = Ast.loc_of_patt patt in
      <:patt< ($patt$ : Typerep_lib.Std.Typerep.t $lid:name$) >>)
    in
    let body = Gen.abstract loc arguments body in
    let body = List.fold_right params_names ~init:body ~f:(fun name acc ->
      <:expr< fun (type $name$) -> $acc$ >> )
    in
    let body =
      match Util.typerep_of_t_coerce ~loc ~type_name ~params_names with
      | Some coerce ->
        <:expr< ($body$ : $coerce$) >>
      | None -> body
    in
    let bnd = Gen.idp loc ("typerep_of_" ^ type_name) in
    let binding = <:binding< $bnd$ = $body$ >> in
    Util.type_name_module_definition ~loc ~type_name ~params_names, binding

  let rec with_typerep_aux = function
    | Ast.TyDcl (loc, type_name, params, rhs, _cl) ->
      [impl_of_one_def ~loc ~type_name ~params ~rhs]
    | <:ctyp< $ctyp1$ and $ctyp2$ >> ->
      with_typerep_aux ctyp1 @ with_typerep_aux ctyp2
    | _ -> assert false

  let with_typerep rec_ ctyp =
    let loc, rec_ =
      match ctyp with
      | Ast.TyDcl (loc, type_name, _, rhs, _) ->
        loc, rec_ && Gen.type_is_recursive type_name rhs
      | <:ctyp@loc< $_$ and $_$ >> ->
        loc, rec_
      | _ -> assert false
    in
    let rec_flag =
      match rec_ with
      | true -> <:rec_flag< rec >>
      | false -> <:rec_flag< >>
    in
    let prelude, bindings = List.split (with_typerep_aux ctyp) in
    <:str_item<
      $list:prelude$;
      value $rec:rec_flag$ $list:bindings$;
    >>

  let rec with_typerep_abstract rec_ ctyp =
    match ctyp with
    | Ast.TyDcl (loc, type_name, params, _ctyp, _cl) ->
      ignore rec_;
      let params_names = Util.params_names ~params in
      Util.typerep_abstract ~loc ~type_name ~params_names
    | <:ctyp@loc< $ctyp1$ and $ctyp2$ >> ->
      <:str_item<
        $with_typerep_abstract rec_ ctyp1$;
        $with_typerep_abstract rec_ ctyp2$;
      >>
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
        Loc.raise loc (Failure (Printf.sprintf "Unknown typerep argument %S" id));
      ]];
      gram_entry: [[
        v = LIST0 typerep_arg SEP "," ; `EOI ->
        ignore loc; (* don't know how to ignore it otherwise *)
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

  let typerep_of_quote (loc : Ast.loc) (_loc_name_opt : string option) (cnt_str : string) =
    Pa_type_conv.set_conv_path_if_not_set loc;
    let ctyp = Gram.parse_string Syntax.ctyp_quot loc cnt_str in
    typerep_of_type ctyp

  let () =
    Quotation.add "typerep_of" Quotation.DynAst.expr_tag typerep_of_quote
end
