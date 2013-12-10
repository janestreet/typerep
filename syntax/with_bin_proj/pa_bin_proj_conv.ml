open StdLabels

open Camlp4
open PreCast

module Gen = Pa_type_conv.Gen

include (struct
  open Pa_typerep_conv
  module List = List
  module Tuple = Tuple
  module Field_case = Field_case
end)

module Variant_case = struct
  include Pa_typerep_conv.Variant_case

  let index ~loc { index ; _ } =
    <:expr< $`int:index$ >>

  let label ~loc { label ; _ } =
    <:expr< $`str:label$ >>
end

module Branches = struct
  include Pa_typerep_conv.Branches

  type t =
  | Fields   of Field_case.t   list
  | Variants of Variant_case.t list

  let compute = function
    | <:ctyp< { $ctyp$ } >>  -> Fields (fields ctyp)

    | <:ctyp< [ $alts$ ]  >>
    | <:ctyp< [< $alts$ ] >>
    | <:ctyp< [> $alts$ ] >>
    | <:ctyp< [= $alts$ ] >> -> Variants (variants alts)

    | ctyp -> Gen.unknown_type ctyp "Util.branches"
end

module Bin_projection : sig
  val t : loc:Ast.loc -> (Ast.ctyp * Ast.ctyp) -> Ast.ctyp
  module Magic : sig
    val variant    : loc:Ast.loc -> kind:[ `Usual | `Polymorphic ] -> Ast.expr -> Ast.expr
    val field      : loc:Ast.loc -> int -> Ast.expr
    val is_variant : loc:Ast.loc -> repr:Ast.expr -> Ast.expr
  end
end = struct
  let t ~loc (a, b) =
    let t = <:ctyp< Bin_projection . $lid:"t"$ >> in
    let t = <:ctyp< $t$ $a$ >> in
    let t = <:ctyp< $t$ $b$ >> in
    t

  module Magic = struct
    let variant ~loc ~kind args =
      let kind =
        match kind with
        | `Usual -> <:expr< Type_struct.Variant.Kind.Usual >>
        | `Polymorphic -> <:expr< Type_struct.Variant.Kind.Polymorphic >>
      in
      let fct = <:expr< Bin_projection.Magic. $lid:"variant"$ >> in
      let kind = <:expr< ~ $lid:"kind"$ : $kind$ >> in
      <:expr< $fct$ $kind$ $args$ >>

    let is_variant ~loc ~repr =
      let fct = <:expr< Bin_projection.Magic. $lid:"is_variant"$ >> in
      let repr = <:expr< ~ $lid:"repr"$ : $repr$ >> in
      <:expr< $fct$ $repr$ >>

    let field ~loc value =
      let fct = <:expr< Bin_projection.Magic. $lid:"field"$ >> in
      let value = <:expr< $`int:value$ >> in
      <:expr< $fct$ $value$ >>
  end
end

module Util : sig
  (* t with its parameters, e.g. [('a, 'b) t] *)
  val ty_with_type_params :
    loc:Ast.loc
    -> type_name:string
    -> params:Ast.ctyp list
    -> Ast.ctyp

  module Record : sig
    module Gen_struct : sig
      val field :
        loc:Ast.loc
        -> arity:int
        -> ty_with_type_params:Ast.ctyp
        -> Field_case.t -> Ast.str_item
    end
    module Gen_sig : sig
      val field :
        loc:Ast.loc
        -> arity:int
        -> ty_with_type_params:Ast.ctyp
        -> Field_case.t -> Ast.sig_item
    end
  end

  module Variant : sig
    module Gen_struct : sig
      val is_case :
        loc:Ast.loc
        -> arity:int
        -> ty_with_type_params:Ast.ctyp
        -> Variant_case.t -> Ast.str_item

      val magic_V : loc:Ast.loc -> Variant_case.t -> Ast.expr

      val match_with :
        loc:Ast.loc
        -> ty_with_type_params:Ast.ctyp
        -> params:Ast.ctyp list
        -> variants:Variant_case.t list
        -> Ast.str_item
    end
    module Gen_sig : sig
      val is_case :
        loc:Ast.loc
        -> arity:int
        -> ty_with_type_params:Ast.ctyp
        -> Variant_case.t -> Ast.sig_item
      val match_with :
        loc:Ast.loc
        -> ty_with_type_params:Ast.ctyp
        -> variants:Variant_case.t list
        -> Ast.sig_item
    end
  end
end = struct
  let ty_with_type_params ~loc ~type_name ~params =
    let fold acc param =
      let name = Gen.get_tparam_id param in
      <:ctyp< $acc$ '$lid:name$ >>
    in
    let init = <:ctyp< $lid:type_name$ >> in
    List.fold_left ~f:fold ~init params

  module Record = struct
    module Gen_struct = struct
      let field ~loc ~arity ~ty_with_type_params { Field_case.label ; index ; ctyp } =
        let body = Bin_projection.Magic.field ~loc index in
        let ctyp = Bin_projection.t ~loc (ty_with_type_params, ctyp) in
        let body = <:expr< ( $body$ : $ctyp$ ) >> in
        let body =
          if arity = 0 then
            body
          else
            let args = [ <:patt< () >> ] in
            Gen.abstract loc args body
        in
        let id = <:patt< $lid:label$ >> in
        let bind = <:binding< $id$ = $body$ >> in
        <:str_item< value $rec:<:rec_flag<>>$ $bind$ >>
    end

    module Gen_sig = struct
      let field ~loc ~arity ~ty_with_type_params { Field_case.label ; ctyp ; index=_ } =
        let type_of_proj = Bin_projection.t ~loc (ty_with_type_params, ctyp) in
        let type_of_proj =
          if arity = 0 then
            type_of_proj
          else
            <:ctyp< unit -> $type_of_proj$ >>
        in
        <:sig_item< value $lid:label$ : $type_of_proj$ >>
    end
  end

  module Variant = struct
    let is_case_label label = "is_"^(String.lowercase label)

    module Gen_struct = struct
      let is_case ~loc ~arity ~ty_with_type_params
          ({ Variant_case.label ; poly ; _ } as case)
          =
        let repr =
          if poly
          then Variant_case.ocaml_repr ~loc case
          else Variant_case.index ~loc case
        in
        let body = Bin_projection.Magic.is_variant ~loc ~repr in
        let ctyp =
          Bin_projection.t ~loc (ty_with_type_params, <:ctyp< $lid:"bool"$>>)
        in
        let body = <:expr< ( $body$ : $ctyp$ ) >> in
        let body =
          if arity = 0 then body
          else
            let args = [ <:patt< () >> ] in
            Gen.abstract loc args body
        in
        let id = <:patt< $lid:is_case_label label$ >> in
        let bind = <:binding< $id$ = $body$ >> in
        <:str_item< value $rec:<:rec_flag<>>$ $bind$ >>

      let param_name name = "pa_bin_proj__p_"^name

      (* should map recursively inside ty *)
      class ['a] param_subst ~loc = object
        inherit Ast.map as super
        method _Loc_t (_ : 'a) = (loc : Ast.loc)
        method! ctyp =
          function
          | <:ctyp@loc< '$name$ >> -> <:ctyp@loc< $lid:param_name name$ >>
          | ctyp -> super#ctyp ctyp
      end

      let magic_V ~loc ({ Variant_case.label ; ctyp ; _ } as case) =
        let proj = <:expr< $lid:String.lowercase label$ >> in
        let index = Variant_case.index ~loc case in
        let ocaml_repr = Variant_case.ocaml_repr ~loc case in
        let label = Variant_case.label ~loc case in
        let td_a =
          let ctyp =
            match ctyp with
            | Some ctyp ->
              (* should map the parameters inside ctyp *)
              let map = new param_subst ~loc in
              map#ctyp ctyp
            | None -> <:ctyp< $lid:"unit"$ >>
          in
          Ast.TyDcl (loc, "a", [], ctyp, [])
        in
        let td_b =
          let ctyp = <:ctyp< $lid:"y"$ >> in
          Ast.TyDcl (loc, "b", [], ctyp, [])
        in
        let type_def td = <:str_item< type $td$ >> in
        let value lid body =
          let bind = <:binding< $lid:lid$ = $body$ >> in
          <:str_item< value $rec:<:rec_flag<>>$ $bind$ >>
        in
        let str_items = [
          type_def td_a;
          type_def td_b;
          value "index" index;
          value "ocaml_repr" ocaml_repr;
          value "label" label;
          value "proj" proj;
        ] in
        let gen_sig =
          let module_type = <:module_type< Bin_projection.Magic. $uid:"V"$ >> in
          let with_constr = <:with_constr< type $lid:"b"$ = $lid:"y"$ >> in
          <:module_type< $module_type$ with $with_constr$ >>
        in
        let gen_str = <:module_expr< struct $Ast.stSem_of_list str_items$ end >> in
        <:expr< (module $gen_str$ : $gen_sig$) >>

      let array ~loc cases =
        let args = List.map ~f:(magic_V ~loc) cases in
        <:expr< [| $list:args$ |] >>

      let match_with ~loc ~ty_with_type_params ~params ~variants:cases =
        let type_of_proj =
          let map = new param_subst ~loc in
          Bin_projection.t ~loc (<:ctyp<$lid:"x"$>>, map#ctyp ty_with_type_params)
        in
        let arguments =
          let labels =
            let map { Variant_case.label ; _ } =
              let label = String.lowercase label in
              <:patt< ~ $lid:label$>>
            in
            List.fold_right cases ~f:(fun case acc -> map case ::acc) ~init:[]
          in
          <:patt< ( $lid:"proj"$ : $type_of_proj$ ) >> :: labels in
        let args = array ~loc cases in
        let magic_variant =
          let kind =
            match cases with
            | [] -> `Usual
            | hd :: _ ->
              if hd.Variant_case.poly
              then `Polymorphic
              else `Usual
          in
          Bin_projection.Magic.variant ~loc ~kind args
        in
        let body =
          let fct = <:expr< Bin_projection. $lid:"pipe"$ >> in
          <:expr< $fct$ $lid:"proj"$ $magic_variant$ >>
        in
        let body = Gen.abstract loc arguments body in
        let body = <:expr< fun (type $lid:"y"$) -> $body$ >> in
        let body = <:expr< fun (type $lid:"x"$) -> $body$ >> in
        (* adding as many type as arguments *)
        let body =
          List.fold_right params ~init:body ~f:(fun param body ->
            let name = Gen.get_tparam_id param in
            let lid = param_name name in
            <:expr< fun (type $lid:lid$) -> $body$ >>)
        in
        let bind = <:binding< $lid:"match_with"$ = $body$ >> in
        <:str_item< value $rec:<:rec_flag<>>$ $bind$ >>
    end

    module Gen_sig = struct
      let is_case ~loc ~arity ~ty_with_type_params { Variant_case.label ; _ } =
        let type_of_proj =
          Bin_projection.t ~loc (ty_with_type_params, <:ctyp< $lid:"bool"$>>)
        in
        let type_of_proj =
          if arity = 0
          then
            type_of_proj
          else
            <:ctyp< unit -> $type_of_proj$ >>
        in
        <:sig_item< value $lid:is_case_label label$ : $type_of_proj$ >>

      let match_with ~loc ~ty_with_type_params ~variants:cases =
        let unit = <:ctyp< $lid:"unit"$ >> in
        let input = <:ctyp< '$lid:"pa_bin_proj_input"$ >> in
        let output = <:ctyp< '$lid:"pa_bin_proj_output"$ >> in
        let proj = Bin_projection.t ~loc (input, ty_with_type_params) in
        let ty =
          let fold { Variant_case.label ; ctyp ; _ } acc =
            let label = String.lowercase label in
            let ctyp =
              match ctyp with
              | Some ctyp -> ctyp
              | None -> unit
            in
            let ctyp = Bin_projection.t ~loc (ctyp, output) in
            let ctyp = <:ctyp< ~ $lid:label$ : $ctyp$ >> in
            <:ctyp< $ctyp$ -> $acc$ >>
          in
          let returned = Bin_projection.t ~loc (input, output) in
          let acc = List.fold_right cases ~init:returned ~f:fold in
          <:ctyp< $proj$ -> $acc$ >>
        in
        <:sig_item< value $lid:"match_with"$ : $ty$ >>
    end
  end
end

module Gen_struct = struct
  let impl_of_one_def ~loc ~type_name ~params ~ctyp =
    let ty_with_type_params = Util.ty_with_type_params ~loc ~type_name ~params in
    let arity = List.length params in
    let items_list =
      match Branches.compute ctyp with
      | Branches.Fields fields ->
        List.map fields ~f:(Util.Record.Gen_struct.field ~loc ~arity ~ty_with_type_params)
      | Branches.Variants variants ->
        let str_items =
          List.map variants
            ~f:(Util.Variant.Gen_struct.is_case ~loc ~arity ~ty_with_type_params)
        in
        let match_with =
          Util.Variant.Gen_struct.match_with ~loc ~ty_with_type_params ~params ~variants
        in
        match_with :: str_items
    in
    let str_items = Ast.stSem_of_list items_list in
    let module_expr = <:module_expr< struct $str_items$ end >> in
    let module_name = "Bin_proj_"^type_name in
    <:str_item< module $uid:module_name$ = $module_expr$ >>

  let with_bin_proj _rec_ ctyp =
    match ctyp with
    | Ast.TyDcl (loc, type_name, params, ctyp, _cl) ->
      impl_of_one_def ~loc ~type_name ~params ~ctyp
    | _ ->
      Gen.error ctyp ~fn:"bin_proj impl_gen" ~msg:"unsupported type def"

  let () =
    Pa_type_conv.add_generator "bin_proj" with_bin_proj
end

module Gen_sig = struct

  let sig_of_one_def ~loc ~type_name ~params ~ctyp =
    let ty_with_type_params = Util.ty_with_type_params ~loc ~type_name ~params in
    let arity = List.length params in
    let items_list =
      match Branches.compute ctyp with
      | Branches.Fields fields ->
        List.map fields ~f:(Util.Record.Gen_sig.field ~loc ~arity ~ty_with_type_params)
      | Branches.Variants variants ->
        let sig_items =
          List.map variants
            ~f:(Util.Variant.Gen_sig.is_case ~loc ~arity ~ty_with_type_params)
        in
        let match_with =
          Util.Variant.Gen_sig.match_with ~loc ~ty_with_type_params ~variants
        in
        match_with :: sig_items
    in
    let sig_items = Ast.sgSem_of_list items_list in
    let module_type = <:module_type< sig $sig_items$ end >> in
    let module_name = "Bin_proj_"^type_name in
    <:sig_item< module $uid:module_name$ : $module_type$ >>

  let with_bin_proj _ ctyp =
    match ctyp with
    | Ast.TyDcl (loc, type_name, params, ctyp, _cl) ->
      sig_of_one_def ~loc ~type_name ~params ~ctyp
    | _ ->
      Gen.error ctyp ~fn:"bin_proj impl_gen" ~msg:"unsupported type def"

  let () =
    Pa_type_conv.add_sig_generator "bin_proj" with_bin_proj
end
