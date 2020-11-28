open Ppxlib
open Common

let pos_of_location (loc : location) =
  ( loc.loc_start.pos_fname,
    (loc.loc_start.pos_lnum, loc.loc_start.pos_bol, loc.loc_start.pos_cnum),
    (loc.loc_end.pos_lnum, loc.loc_end.pos_bol, loc.loc_end.pos_cnum) )

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

let make_tuple loc ls =
  { pexp_desc = Pexp_tuple ls;
    pexp_loc = loc;
    pexp_attributes = [];
    pexp_loc_stack = [] }

let exp_of_position ~loc (fname, (line, bol, col), (eline, ebol, ecol)) =
  { pexp_desc =
      Pexp_tuple
        [ exp_of_string ~loc (Filename.basename fname);
          make_tuple loc [exp_of_int ~loc line; exp_of_int ~loc (col - bol)];
          make_tuple loc [exp_of_int ~loc eline; exp_of_int ~loc (ecol - ebol)]
        ];
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

let exp_of_fun_id ~replacement_prefix_id id =
  let txt = snd @@ replace_module_prefix_id ~replacement_prefix_id id in
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

let rules =
  List.map (fun id ->
      let rewriter =
        rewriter (exp_of_fun_id (Longident.parse id) ~replacement_prefix_id)
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

let register () = Driver.register_transformation ~rules "my_transformation"
