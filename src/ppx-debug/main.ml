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
  {
    pexp_desc = Pexp_constant (Pconst_string (str, None));
    pexp_loc = loc;
    pexp_attributes = [];
  }

let exp_of_int ~loc i =
  {
    pexp_desc = Pexp_constant (Pconst_integer (string_of_int i, None));
    pexp_loc = loc;
    pexp_attributes = [];
  }

let exp_of_position ~loc (fname, line, bol, col) =
  {
    pexp_desc =
      Pexp_tuple
        [
          exp_of_string ~loc fname;
          exp_of_int ~loc line;
          exp_of_int ~loc bol;
          exp_of_int ~loc col;
        ];
    pexp_loc = loc;
    pexp_attributes = [];
  }

let rewriter exp (node : Parsetree.expression) =
  let { pexp_desc; pexp_loc; pexp_attributes = _ } = node in
  let loc = pexp_loc in
  let exp = exp loc in
  let pos_arg = (Labelled "pos", exp_of_position ~loc (pos_of_location loc)) in
  match pexp_desc with
  | Pexp_ident _id -> None
  | Pexp_apply (_exp, args) ->
      Option.fold
        ~some:(fun _ -> None)
        ~none:
          (let pexp_desc = Pexp_apply (exp, pos_arg :: args) in
           Some { node with pexp_desc })
        (List.assoc_opt (Labelled "pos") args)
  | _ -> None

(**declaration of the extension*)
let exp_of_fun_id loc id =
  let ident_of_str str =
    let split = String.split_on_char '.' str in
    match split with
    | [] -> failwith ("invalid_arg" ^ __LOC__)
    | [id] -> Lident id
    | h :: next :: t ->
        List.fold_left
          (fun ident next -> Ldot (ident, next))
          (Ldot (Lident h, next))
          t
  in

  {
    pexp_desc = Pexp_ident { txt = ident_of_str id; loc };
    pexp_loc = loc;
    pexp_attributes = [];
  }

(* let extension = Extension.declare
 *     ("print_location")
 *     Extension.Context.expression
 *     Ast_pattern.(single_expr_payload (__))
 *     instr *)

let rules =
  List.map (fun id ->
      let rewriter = rewriter (fun loc -> exp_of_fun_id loc id) in
      Context_free.Rule.special_function id rewriter)
  @@
  let positionnable =
    [
      "wait";
      "return";
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
      "backtrace_try_bind";
    ]
  in
  positionnable
  @ List.map (fun s -> "Lwt." ^ s) positionnable
  @ [">>="; ">|="; "<?>"; "<&>"; "=<<"; "=|<"]

(* @ (let rewriter = rewriter (fun loc -> [%expr Lwt.return]) in
 *    List.map
 *      (fun id -> Context_free.Rule.special_function id rewriter)
 *      ["return"; "Lwt.return"])
 * @ (let rewriter = rewriter (fun loc -> [%expr Lwt.fail]) in
 *    List.map
 *      (fun id -> Context_free.Rule.special_function id rewriter)
 *      ["fail"; "Lwt.fail"])
 * @ (let rewriter = rewriter (fun loc -> [%expr Lwt.return]) in
 *    List.map
 *      (fun id -> Context_free.Rule.special_function id rewriter)
 *      ["return"; "Lwt.return"])
 * @ (let rewriter = rewriter (fun loc -> [%expr Lwt.bind]) in
 *    List.map
 *      (fun id -> Context_free.Rule.special_function id rewriter)
 *      [">>="; "bind"; "Lwt.bind"])
 * @ (let rewriter = rewriter (fun loc -> [%expr Lwt.catch]) in
 *    List.map
 *      (fun id -> Context_free.Rule.special_function id rewriter)
 *      ["catch"; "Lwt.catch"])
 * @ (let rewriter = rewriter (fun loc -> [%expr Lwt.finalize]) in
 *    List.map
 *      (fun id -> Context_free.Rule.special_function id rewriter)
 *      ["finalize"; "Lwt.finalize"])
 * @ (let rewriter = rewriter (fun loc -> [%expr Lwt.try_bind]) in
 *    List.map
 *      (fun id -> Context_free.Rule.special_function id rewriter)
 *      ["try_bind"; "Lwt.try_bind"])
 * @ (let rewriter = rewriter (fun loc -> [%expr Lwt.both]) in
 *    List.map
 *      (fun id -> Context_free.Rule.special_function id rewriter)
 *      ["both"; "Lwt.both"])
 * @ [] *)

(* let rule_join =
 *   [
 *     Context_free.Rule.special_function
 *       "join"
 *       (rewriter (fun loc -> [%expr Lwt.join]));
 *     Context_free.Rule.special_function
 *       "Lwt.join"
 *       (rewriter (fun loc -> [%expr Lwt.join]));
 *   ] *)

let () = Driver.register_transformation ~rules "my_transformation"
