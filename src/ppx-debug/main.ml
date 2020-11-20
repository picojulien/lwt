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
    pexp_loc_stack = []
  }


let exp_of_int ~loc i =
  {
    pexp_desc = Pexp_constant (Pconst_integer (string_of_int i, None));
    pexp_loc = loc;
    pexp_attributes = [];
    pexp_loc_stack = []
  }

let exp_of_position ~loc (fname, line, bol, col) =
  {
    pexp_desc =
      Pexp_tuple
        [
          exp_of_string ~loc (Filename.basename fname);
          exp_of_int ~loc line;
          exp_of_int ~loc bol;
          exp_of_int ~loc col;
        ];
    pexp_loc = loc;
    pexp_attributes = [];
    pexp_loc_stack = [];
  }

let rewriter exp (node : Parsetree.expression) =
  let { pexp_desc; pexp_loc;  _ } = node in
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


let try_remove_prefix ~prefix lst =
  let rec aux p l =
    match p,l with
      [] , _ -> (true,l)
    | h::p , h'::l when h = h' -> aux p l
    | _ -> (false,lst)
  in aux prefix lst

let rec pp_longident ppf (id:longident) =
  match id with
  | Ppxlib.Lident s -> Format.fprintf ppf "%s" s
  | Ppxlib.Ldot (id, s) -> Format.fprintf ppf "%a.%s" pp_longident id s
  | Ppxlib.Lapply (id, id') ->
     Format.fprintf ppf
       "%a.%a"
       pp_longident id
       pp_longident id'
(**declaration of the extension*)
let exp_of_fun_id ?replace_prefix ?replacement_prefix id =
  let rep = Option.map (String.split_on_char '.') replace_prefix in
  let split = String.split_on_char '.' id in
  (* Format.printf
   *   "replace_prefix:%s and add prefix %s in :%s@."
   *   (Option.fold ~none:"" ~some:(String.concat ".") rep)
   *   (Option.value ~default:"" prefix)
   *   (String.concat "." split)
   * ; *)
  let removed_prefix,split = Option.fold
                  ~none:(false,split)
                  ~some:(fun prefix ->
                    try_remove_prefix ~prefix split)
                  rep in
  let ident_of_id =
    Longident.parse
      (if removed_prefix then
        Option.value ~default:"" replacement_prefix ^ String.concat "." (split)
      else id) in
  let txt  = ident_of_id in
  (* Format.printf
   *   "%s will be replaced by %a@."
   *   id
   *   pp_longident txt
   * ; *)
  fun loc ->
  {
    pexp_desc = Pexp_ident { txt  ; loc };
    pexp_loc = loc;
    pexp_attributes = [];
    pexp_loc_stack = [];
  }

(* let extension = Extension.declare
 *     ("print_location")
 *     Extension.Context.expression
 *     Ast_pattern.(single_expr_payload (__))
 *     instr *)

let replacement_prefix = "Lwt_debug."
let replace_prefix = Some "Lwt"

let rules =
  List.map (fun id ->
      let rewriter = rewriter (exp_of_fun_id id ?replace_prefix ~replacement_prefix) in
      Context_free.Rule.special_function id rewriter)
  @@
  let positionnable =
    [
      "wait";
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
      "backtrace_try_bind";
    ]
  in
  positionnable
  @ List.map (fun s -> "Lwt." ^ s) positionnable
  @ [">>="; ">|="; "<?>"; "<&>"; "=<<"; "=|<"]
  @ List.map (fun s -> "Error_monad." ^ s) positionnable
  @ [">>=?"; ">|=?"]



let () = Driver.register_transformation ~rules "my_transformation"
