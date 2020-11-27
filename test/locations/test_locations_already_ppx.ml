[@@@ocaml.ppx.context
{ tool_name = "ppxlib_driver";
  include_dirs = [];
  load_path = [];
  open_modules = [];
  for_package = None;
  debug = false;
  use_threads = false;
  use_vmthreads = false;
  recursive_types = false;
  principal = false;
  transparent_modules = false;
  unboxed_types = false;
  unsafe_string = false;
  cookies = [] }]

let section = ref 0

let margin = 2

let begin_section () = section := !section + margin

let end_section () = section := !section - margin

let end_chapter () = Format.printf "@.======================================@."

let printf format =
  Format.ifprintf Format.std_formatter "%s" (String.make !section ' ') ;
  Format.ifprintf Format.std_formatter format

let pp_position_short_option ppf v =
  match v with
  | Some (file, line, bol, col) ->
      Format.fprintf ppf "%s:%d:%d" file line (col - bol)
  | None ->
      Format.fprintf ppf "unknown location"
  [@@ocaml.warning "-32"]

let print str = Lwt_io.printf "%s" str

open Lwt_debug

let bindings () =
  ( >>= )
    ~pos:("test_locations.ml", 74, 2313, 2315)
    (Lwt_debug.pause ~pos:("test_locations.ml", 76, 2313, 2315) ())
    (fun () ->
      ( >>= )
        ~pos:("test_locations.ml", 78, 2400, 2402)
        (Lwt_unix.sleep 2.)
        (fun () -> print "titi"))

module Option = struct
  include Option

  let pp ?(default = "None") pp ppf optv =
    match optv with
    | None ->
        Format.fprintf ppf "%s" default
    | Some v ->
        Format.fprintf ppf "%a" pp v
end

let pp_position ppf (file, line, bol, col) =
  Format.fprintf ppf "file %s, line %d, col %d" file line (col - bol)
  [@@ocaml.warning "-32"]

let pp_position_short ppf (file, line, bol, col) =
  Format.fprintf ppf "%s:%d:%d" file line (col - bol)

let pp_position_short_option ppf v =
  match v with
  | Some (file, line, bol, col) ->
      Format.fprintf ppf "%s:%d:%d" file line (col - bol)
  | None ->
      Format.fprintf ppf "unknown location"

let print_pos prefix pcked_list =
  Format.printf
    "%s: @[%a@]@."
    prefix
    (Format.pp_print_list
       ~pp_sep:Format.pp_print_space
       pp_position_short_option)
    (List.map
       (fun p -> match p with P p -> Lwt_debug.def_position p)
       pcked_list)

let pp_pos ppf (file, line, bol, col) =
  Format.fprintf ppf "@[<h>%s:%d:%d@]" file line (col - bol)

let next_known_pos p =
  let visited = Hashtbl.create 10 in
  let rec next_known_pos :
      type a. int -> a Lwt_debug.t -> int * Lwt_debug.pos option =
   fun len p ->
    printf "next_known_pos: iter %d@." len ;
    if Hashtbl.mem visited (Lwt_debug.P p) then (len, None)
    else (
      Hashtbl.add visited (Lwt_debug.P p) () ;
      match Lwt_debug.def_position p with
      | None -> (
        match Lwt_debug.predecessors p with
        | [] ->
            (len, None)
        | [P p] ->
            next_known_pos (len + 1) p
        | _ ->
            (len, None) )
      | Some _ as res ->
          (len, res) )
  in
  next_known_pos 0 p

let parents : 'a Lwt_debug.t -> Lwt_debug.packed list =
 fun p ->
  printf "getting parents@." ;
  let pred = Lwt_debug.predecessors p in
  printf "got %d parents@." (List.length pred) ;
  pred

let stop = "\203\167"

let branch = "\203\171"

let pp_sep ppf () = Format.fprintf ppf "@ %s" branch

let pp_status ppf p =
  Format.fprintf
    ppf
    "%s"
    ( match Lwt_debug.state p with
    | Lwt_debug.Return _ ->
        "fulfilled"
    | Lwt_debug.Fail _ ->
        "failed"
    | Lwt_debug.Sleep ->
        "sleep" )

let pp_parents_tree =
  let rec pp_parents_tree : type a. 'b -> a Lwt_debug.t -> unit =
   fun ppf promise ->
    let pp_parents ppf ps =
      match ps with
      | [] ->
          Format.fprintf ppf "%s" stop
      | _ ->
          Format.fprintf ppf "@,|@,%s" branch ;
          Format.fprintf
            ppf
            "%a"
            (fun ppf ps ->
              Format.pp_print_list
                ~pp_sep
                (fun ppf (P p) -> pp_parents_tree ppf p)
                ppf
                ps)
            ps
    in
    begin_section () ;
    Format.fprintf
      ppf
      "@[<v 2>%a(stat:%a) %a@]"
      (fun ppf (len, v) ->
        let path = String.make len '.' in
        match v with
        | None ->
            Format.fprintf ppf "%s" path
        | Some v ->
            Format.fprintf ppf "%s %a" path pp_pos v)
      (next_known_pos promise)
      pp_status
      promise
      pp_parents
      (parents promise) ;
    end_section ()
  in
  pp_parents_tree

let print_tree s p =
  begin_section () ;
  Format.printf "@[<v 2>tree for %s @ @[%a@]@]@." s pp_parents_tree p ;
  end_section () ;
  end_chapter ()

let _ =
  Lwt_main.run
  @@
  let p = bindings () in
  let p1 =
    Lwt_debug.bind ~pos:("test_locations.ml", 249, 6779, 6790) p bindings
  in
  let p2 =
    ( >>= ) ~pos:("test_locations.ml", 251, 6824, 6828) (bindings ()) bindings
  in
  let p3 =
    ( >>= ) ~pos:("test_locations.ml", 256, 6965, 6969) p1 (fun () -> p2)
  in
  let p4 = ( >>= ) ~pos:("test_locations.ml", 260, 7086, 7097) p bindings in
  let p_join =
    Lwt_debug.join ~pos:("test_locations.ml", 261, 7115, 7130) [p1; p2; p3]
  in
  print_tree "p" p ;
  print_tree "p1" p1 ;
  print_tree "p1" p1 ;
  print_tree "p2" p2 ;
  print_tree "p3" p3 ;
  print_tree "p4" p4 ;
  print_tree "p_join" p_join ;
  ( >>= ) ~pos:("test_locations.ml", 280, 7764, 7766) p1 (fun () ->
      print_endline "" ;
      print_tree "p1" p1 ;
      print_tree "p2" p2 ;
      print_tree "p3" p3 ;
      print_tree "p_join" p_join ;
      p3)
