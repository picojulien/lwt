
let print str  = Lwt_io.printf "%s" str


(* let rec fonction_bidon x =
 *   let acc = ref 0 in
 *   for i = 1 to x do
 *     acc := !acc + (fonction_bidon i)
 *   done ;
 *   !acc *)

(* let loc = Owee_location.extract fonction_bidon
 *
 * let () = assert (loc <> Owee_location.none) *)
open Lwt

let bindings ()  =
  print "toto"
  >>=
    fun () -> Lwt_unix.sleep 2.
  >>=
    fun () -> print "titi"

module Option = struct
  include Option
  let pp ?(default="None") pp ppf optv =
    match optv with
      None -> Format.fprintf ppf "%s"
                default
    | Some v ->  Format.fprintf ppf "%a"
                   pp
                   v
end

let pp_postion ppf (file,line,bol,col) =
  Format.fprintf ppf
    "file %s, line %d, col %d"
    file
    line
    (col-bol)

let print_pos prefix pcked_list =
  Format.printf "%s: @[%a@]@."
    prefix
    (Format.pp_print_list pp_postion)
  (List.map (fun p ->
    match p with P p ->
        Lwt.def_position p)
  pcked_list)

let _ =
  Lwt_main.run @@
    let p1 = Lwt.bind (bindings ()) bindings   in
    let p2 = bindings () >>= bindings
    in
    let p3 =
      p1 >>=
        fun () -> p2  in
    let p_join =
      Lwt.join [p1 ; p2 ; p3]  in

    let parents_1 = Lwt.predecessors p1 in
    let parents_2 = Lwt.predecessors p2 in
    let parents_3 = Lwt.predecessors p3 in
    let parents_join = Lwt.predecessors p_join in
    let successors = Lwt.successors p1 in
    print_pos "par_1" parents_1 ;
    print_pos "par_2" parents_2 ;
    print_pos "par_3" parents_3 ;
    print_pos "par_join" parents_join ;
    print_pos "succ" successors ;
    p1 >>= fun () ->
    print_endline "";
    print_pos "par_1" parents_1 ;
    print_pos "par_2" parents_2 ;
    print_pos "par_3" parents_3 ;
    print_pos "par_join" parents_join ;
    print_pos "succ" successors ;
    p3
