open Lwt

let section = ref 0

let margin = 2

let begin_section () = section := !section + margin

let end_section () = section := !section - margin

let end_chapter () = Format.printf "@.======================================@."

(* let printf format =
 *   Format.printf "%s" (String.make !section ' ') ;
 *   Format.printf format *)

let printf format =
  Format.ifprintf Format.std_formatter "%s" (String.make !section ' ') ;
  Format.ifprintf Format.std_formatter format

let print_pos prefix pcked_list =
  Format.printf
    "%s: @[%a@]@."
    prefix
    (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_position_option)
    (List.map (fun p -> match p with P p -> Lwt.def_position p) pcked_list)

module Option = struct
  include Option

  let pp ?(default = "None") pp ppf optv =
    match optv with
    | None ->
        Format.fprintf ppf "%s" default
    | Some v ->
        Format.fprintf ppf "%a" pp v
end

(* let inspect_obj descr obj =
 *   let repr = Obj.repr obj in
 *   if not @@ Obj.is_block repr then (
 *     printf "Not an obj at %s@." descr ;
 *     raise (Invalid_argument "inspect_obj") )
 *   else
 *     let size = Obj.size repr in
 *     let tag = Obj.tag repr in
 *     printf "Inspecting %s with size %d tag %d@." descr size tag ;
 *     (repr, size, tag)
 *
 * let rec inspect_promise ?(first = true) id (p : 'a Lwt.t) =
 *   let (repr, size, _tag) = inspect_obj ("promise" ^ id) p in
 *   begin_section () ;
 *   if size = 1 then (
 *     printf "boxed promise@." ;
 *     inspect_promise ~first:false id (Obj.obj (Obj.field repr 0)) )
 *   else if size <> 3 then printf "wrong size@."
 *   else (
 *     if first then printf "unboxed promise" ;
 *     let (pos, _size, _tag) = inspect_obj "pos_0" (Obj.field repr 0) in
 *     printf "pos : %a@." pp_position_short_option (Obj.obj pos) ;
 *     let (parents, _size, _tag) = inspect_obj "parents" (Obj.field repr 1) in
 *     let parents : PackedWeakSet.t = Obj.obj parents in
 *     ignore (inspect_obj "parents" parents) ;
 *     (\* printf "pos : %a@." pp_position_short_option (Obj.obj pos) ; *\)
 *     let (_state, _size, _tag) = inspect_obj "state" (Obj.field repr 2) in
 *     () ) ;
 *   end_section () *)

let print str = Lwt_io.printf "%s" str

(* let rec fonction_bidon x =
 *   let acc = ref 0 in
 *   for i = 1 to x do
 *     acc := !acc + (fonction_bidon i)
 *   done ;
 *   !acc *)

(* let loc = Owee_location.extract fonction_bidon
 *
 * let () = assert (loc <> Owee_location.none) *)

let bindings () =
  Lwt.pause ()
  (* ******************* *)
  >>= fun () ->
  (* ******************* *)
  Lwt_unix.sleep 2.
  (* *********************************** *)
  >>= fun () -> print "titi"

(* let rec next_postionned : type a. a Lwt_debug.t -> Lwt_debug.packed option =
 *  fun p ->
 *   match Lwt_debug.def_position p with
 *   | None -> (
 *     match Lwt_debug.predecessors p with
 *     | [] ->
 *         None
 *     | [P p] ->
 *         next_postionned p
 *     | _ ->
 *         None )
 *   | Some _ ->
 *       Some (P p) *)

let parents : 'a Lwt_debug.t -> Lwt_debug.packed list =
 fun p ->
  printf "getting parents@." ;
  let pred = Lwt_debug.predecessors p in
  printf "got %d parents@." (List.length pred) ;
  pred

(* let pp_parents_tree : type a. 'b -> a Lwt_debug.t -> unit =
 *  fun ppf promise -> Format.fprintf ppf "@[<v 2>%a@]@." pp_parents_tree promise *)

let print_tree s p =
  begin_section () ;
  Format.printf
    "@[<v 2>tree for %s @, @[%a@]@]@."
    s
    (pp_dependencies_tree ~human:true)
    p ;
  end_section () ;
  end_chapter ()

let _ =
  Lwt_main.run
  @@
  let p = bindings () in
  let p1 = Lwt.bind p bindings in
  let p2 =
    bindings () (* *********************************** *)
    >>= (* *********************************** *)
        bindings
  in
  let p3 =
    p1 (* *********************************** *)
    >>= (* *********************************** *)
    fun () -> p2
  in
  let p4 = p >>= bindings in
  let p_join = Lwt.join [p1; p2; p3] in
  (* inspect_promise "p" p ; *)
  print_tree "p" p ;
  print_tree "p1" p1 ;
  print_tree "p1" p1 ;
  print_tree "p2" p2 ;
  print_tree "p3" p3 ;
  print_tree "p4" p4 ;
  print_tree "p_join" p_join ;
  (* let parents_1 = Lwt.predecessors p1 in
   * let parents_2 = Lwt.predecessors p2 in
   * let parents_3 = Lwt.predecessors p3 in
   * let parents_join = Lwt.predecessors p_join in
   * let successors = Lwt.successors p1 in
   * print_pos "par_1" parents_1 ;
   * print_pos "par_2" parents_2 ;
   * print_pos "par_3" parents_3 ;
   * print_pos "par_join" parents_join ;
   * print_pos "succ" successors ; *)
  p1
  >>= fun () ->
  print_endline "" ;
  print_tree "p1" p1 ;
  print_tree "p2" p2 ;
  print_tree "p3" p3 ;
  print_tree "p_join" p_join ;
  (* print_pos "par_1" parents_1 ;
   * print_pos "par_2" parents_2 ;
   * print_pos "par_3" parents_3 ;
   * print_pos "par_join" parents_join ;
   * print_pos "succ" successors ; *)
  p3
