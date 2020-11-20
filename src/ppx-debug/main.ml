open Ppxlib

let pos_of_location (loc : location) =
  ( loc.loc_start.pos_fname,
    loc.loc_start.pos_lnum,
    loc.loc_start.pos_bol,
    loc.loc_start.pos_cnum )

(**For a given AST node,
   [%print_location expression] returns an instrumented node printing the expression location and time of entering and exit*)

let instr ~loc ~path:_ (node : Parsetree.expression) =
  match node with [%expr p >>= f] -> [%expr Lwt.bind p f] | _ -> node

let exp_of_string ~loc str =
  { pexp_desc = Pexp_constant (Pconst_string (str, None));
    pexp_loc = loc;
    pexp_attributes = [];
    pexp_loc_stack = [] }

let exp_of_int ~loc i =
  { pexp_desc = Pexp_constant (Pconst_integer (string_of_int i, None));
    pexp_loc = loc;
    pexp_attributes = [];
    pexp_loc_stack = [] }

let exp_of_position ~loc (fname, line, bol, col) =
  { pexp_desc =
      Pexp_tuple
        [ exp_of_string ~loc (Filename.basename fname);
          exp_of_int ~loc line;
          exp_of_int ~loc bol;
          exp_of_int ~loc col ];
    pexp_loc = loc;
    pexp_attributes = [];
    pexp_loc_stack = [] }

let rewriter exp (node : Parsetree.expression) =
  let {pexp_desc; pexp_loc; _} = node in
  let loc = pexp_loc in
  let exp = exp loc in
  let pos_arg = (Labelled "pos", exp_of_position ~loc (pos_of_location loc)) in
  match pexp_desc with
  | Pexp_ident _id ->
      None
  | Pexp_apply (_exp, args) ->
      Option.fold
        ~some:(fun _ -> None)
        ~none:
          (let pexp_desc = Pexp_apply (exp, pos_arg :: args) in
           Some {node with pexp_desc})
        (List.assoc_opt (Labelled "pos") args)
  | _ ->
      None

let try_remove_prefix ~prefix lst =
  let rec aux p l =
    match (p, l) with
    | ([], _) ->
        (true, l)
    | (h :: p, h' :: l) when h = h' ->
        aux p l
    | _ ->
        (false, lst)
  in
  aux prefix lst

let rec pp_longident ppf (id : longident) =
  match id with
  | Ppxlib.Lident s ->
      Format.fprintf ppf "%s" s
  | Ppxlib.Ldot (id, s) ->
      Format.fprintf ppf "%a.%s" pp_longident id s
  | Ppxlib.Lapply (id, id') ->
      Format.fprintf ppf "%a.%a" pp_longident id pp_longident id'

let rec list_of_ident id =
  match id with
  | Ppxlib.Lident s ->
      [s]
  | Ppxlib.Ldot (id, s) ->
      list_of_ident id @ [s]
  | Ppxlib.Lapply (id, id') ->
      list_of_ident id @ list_of_ident id'

let replace_module_prefix ?replace_prefix ?replacement_prefix id =
  let rep = Option.map (String.split_on_char '.') replace_prefix in
  let split = String.split_on_char '.' id in
  let (removed_prefix, split) =
    Option.fold
      ~none:(false, split)
      ~some:(fun prefix -> try_remove_prefix ~prefix split)
      rep
  in
  Longident.parse
    ( if removed_prefix then
      Option.value ~default:"" replacement_prefix ^ String.concat "." split
    else id )

let rec replace_module_prefix_id ?replace_prefix_id ?replacement_prefix_id id =
  match (replace_prefix_id, replacement_prefix_id) with
  | (None, _) ->
      (false, id)
  | (_, None) ->
      (false, id)
  | (Some replace_prefix_id, Some replacement_prefix_id) ->
      let rec replace id =
        if id = replace_prefix_id then (true, replacement_prefix_id)
        else
          match id with
          | Ppxlib.Lident _ ->
              (false, id)
          | Ppxlib.Ldot (p, s) ->
              let (acted, rep) = replace p in
              (acted, Ppxlib.Ldot (rep, s))
          | Ppxlib.Lapply (id, id') ->
              let (acted, rep) = replace id in
              let (acted', rep') = replace id' in
              (acted || acted', Ppxlib.Lapply (rep, rep'))
      in
      replace id

let exp_of_fun_id ?replace_prefix_id ?replacement_prefix_id id =
  let txt =
    snd
    @@ replace_module_prefix_id ?replace_prefix_id ?replacement_prefix_id id
  in
  fun loc ->
    { pexp_desc = Pexp_ident {txt; loc};
      pexp_loc = loc;
      pexp_attributes = [];
      pexp_loc_stack = [] }

(* let extension = Extension.declare
 *     ("print_location")
 *     Extension.Context.expression
 *     Ast_pattern.(single_expr_payload (__))
 *     instr *)

let replace_prefix_id = Some (Longident.parse "Lwt")

let replacement_prefix_id = Some (Longident.parse "Lwt_debug")

let rules =
  List.map (fun id ->
      let rewriter =
        rewriter
          (exp_of_fun_id
             (Longident.parse id)
             ?replace_prefix_id
             ?replacement_prefix_id)
      in
      Context_free.Rule.special_function id rewriter)
  @@
  let positionnable =
    [ "wait";
      "return";
      "return_ok";
      "fail";
      "bind";
      "catch";
      "finalize";
      "try_bind";
      "both";
      "join";
      "all";
      "pick";
      "choose";
      "npick";
      "nchoose";
      "nchoose_split";
      "task";
      "protected";
      "no_cancel";
      "map";
      "of_result";
      "add_task_r";
      "add_task_l";
      "pause";
      "fail_with";
      "fail_invalid_arg";
      "backtrace_bind";
      "backtrace_catch";
      "backtrace_finalize";
      "backtrace_try_bind" ]
  in
  positionnable
  @ List.map (fun s -> "Lwt." ^ s) positionnable
  @ [">>="; ">|="; "<?>"; "<&>"; "=<<"; "=|<"]
  @ List.map (fun s -> "Error_monad." ^ s) positionnable
  @ [">>=?"; ">|=?"]

let replace_modules =
  object
    inherit Ast_traverse.map as super

    method! module_expr_desc mexprd =
      let mexprd = super#module_expr_desc mexprd in
      match mexprd with
      | Ppxlib__.Import.Ast.Pmod_ident {txt; loc} ->
          Ppxlib__.Import.Ast.Pmod_ident
            { txt =
                snd
                @@ replace_module_prefix_id
                     ?replace_prefix_id
                     ?replacement_prefix_id
                     txt;
              loc }
      | _ ->
          mexprd
  end

let header_insertion _ = ([], [])

let () =
  Driver.register_transformation ~rules "my_transformation" ;
  Ppxlib.Driver.register_transformation
    "replace_modules"
    ~impl:replace_modules#structure
    ~intf:
      replace_modules#signature
      (* using enclose_impl to ensure this pass come after the rules above *)
    ~enclose_impl:header_insertion
