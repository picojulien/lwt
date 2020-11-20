open Ppxlib
open Common

let mapper =
  object
    inherit Ast_traverse.map as super

    method! module_expr_desc mexprd =
      let mexprd = super#module_expr_desc mexprd in
      match mexprd with
      | Ppxlib__.Import.Ast.Pmod_ident {txt; loc} ->
          Ppxlib__.Import.Ast.Pmod_ident
            { txt = snd @@ replace_module_prefix_id ~replacement_prefix_id txt;
              loc }
      | _ ->
          mexprd

    method! expression_desc exprd =
      let exprd = super#expression_desc exprd in
      match exprd with
      | Ppxlib__.Import.Ast.Pexp_ident {txt; loc} ->
          Ppxlib__.Import.Ast.Pexp_ident
            { txt = snd @@ replace_module_prefix_id ~replacement_prefix_id txt;
              loc }
      | _ ->
          exprd
  end

let header_insertion _ = ([], [])

let register () =
  Ppxlib.Driver.register_transformation
    "replace_modules"
    ~impl:mapper#structure
    ~intf:mapper#signature
    ~enclose_impl:header_insertion

(* using enclose_impl to ensure this pass come after the rules above *)
