(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Module [Lwt_result]: explicit error handling *)

open Result

type (+'a, +'b) t = ('a, 'b) Result.result Lwt.t

type pos = string*int*int*int

let return ?pos x = Lwt.return ?pos  (Ok x)
let fail ?pos  e = Lwt.return ?pos (Error e)

let lift = Lwt.return
let ok  ?pos x = Lwt.map ?pos (fun y -> Ok y) x

let map ?pos f e =
  Lwt.map ?pos
    (function
      | Error e -> Error e
      | Ok x -> Ok (f x))
    e

let map_err ?pos f e =
  Lwt.map ?pos
    (function
      | Error e -> Error (f e)
      | Ok x -> Ok x)
    e

let catch ?pos e =
  Lwt.catch ?pos
    (fun () -> ok e)
    (fail ?pos)

let get_exn ?pos e =
  Lwt.bind ?pos e
    (function
      | Ok x -> Lwt.return ?pos x
      | Error e -> Lwt.fail ?pos e)

let bind ?pos e f =
  Lwt.bind ?pos e
    (function
      | Error e -> Lwt.return ?pos (Error e)
      | Ok x -> f x)

let bind_lwt ?pos e f =
  Lwt.bind ?pos e
    (function
      | Ok x -> ok ?pos (f x)
      | Error e -> fail ?pos e)

let bind_result ?pos e f =
  Lwt.map ?pos
    (function
      | Error e -> Error e
      | Ok x -> f x)
    e

let bind_lwt_err ?pos  e f =
  Lwt.bind ?pos e
    (function
      | Error e -> Lwt.bind ?pos  (f e) (fail ?pos)
      | Ok x -> return ?pos x)

let both ?pos a b =
  let s = ref None in
  let set_once e =
    match !s with
    | None -> s:= Some e
    | Some _ -> ()
  in
  let (a,b) = map_err ?pos  set_once a,map_err set_once b in
  let some_assert = function
    | None -> assert false
    | Some e -> Error e
  in
  Lwt.map ?pos
    (function
      | Ok x, Ok y -> Ok (x,y)
      | Error _, Ok _
      | Ok _,Error _
      | Error _, Error _ -> some_assert !s)
    (Lwt.both a b)

module Infix = struct
  let (>>=) e ?pos f = bind ?pos e f
  let (>|=) e ?pos f = map ?pos f e
end

module Syntax = struct
  let (let*) a b = bind ?pos:None a b
  let (and*) a b = both ?pos:None a b

  let (let+) x f = map ?pos:None f x
  let (and+) a b  = both ?pos:None a b
end

include Infix
