(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Explicit error handling

    @since 2.6.0 *)

(** This module provides helpers for values of type [('a, 'b) result Lwt.t].
    The module is experimental and may change in the future. *)

type (+'a, +'b) t = ('a, 'b) Result.result Lwt.t

type pos = Lwt_debug.pos

val return : ?pos:pos -> 'a -> ('a, _) t

val fail : ?pos:pos -> 'b -> (_, 'b) t

val lift : ?pos:pos -> ('a, 'b) Result.result -> ('a, 'b) t

val ok : ?pos:pos -> 'a Lwt.t -> ('a, _) t

val catch : ?pos:pos -> 'a Lwt.t -> ('a, exn) t
(** [catch x] behaves like [return y] if [x] evaluates to [y],
    and like [fail e] if [x] raises [e] *)

val get_exn : ?pos:pos -> ('a, exn) t -> 'a Lwt.t
(** [get_exn] is the opposite of {!catch}: it unwraps the result type,
    returning the value in case of success, calls {!Lwt.fail} in
    case of error. *)

val map : ?pos:pos -> ('a -> 'b) -> ('a,'e) t -> ('b,'e) t

val map_err : ?pos:pos -> ('e1 -> 'e2) -> ('a,'e1) t -> ('a,'e2) t

val bind : ?pos:pos -> ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t

val bind_lwt : ?pos:pos -> ('a,'e) t -> ('a -> 'b Lwt.t) -> ('b,'e) t

val bind_lwt_err : ?pos:pos -> ('a,'e1) t -> ('e1 -> 'e2 Lwt.t) -> ('a,'e2) t

val bind_result : ?pos:pos -> ('a,'e) t -> ('a -> ('b,'e) Result.result) -> ('b,'e) t

val both : ?pos:pos -> ('a,'e) t -> ('b,'e) t -> ('a * 'b,'e) t
(** [Lwt.both p_1 p_2] returns a promise that is pending until {e both} promises
    [p_1] and [p_2] become {e resolved}.
    If only [p_1] is [Error e], the promise is resolved with [Error e],
    If only [p_2] is [Error e], the promise is resolved with [Error e],
    If both [p_1] and [p_2] resolve with [Error _], the promise is resolved with
    the error that occurred first. *)


module Infix : sig
  val (>|=) : ('a,'e) t -> ?pos:pos -> ('a -> 'b) -> ('b,'e) t
  val (>>=) : ('a,'e) t -> ?pos:pos -> ('a -> ('b,'e) t) -> ('b,'e) t
end

(** {3 Let syntax} *)
module Syntax : sig

  (** {1 Monadic syntax} *)

  val (let*) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
  (** Syntax for {!bind}. *)

  val (and*) : ('a,'e) t -> ('b,'e) t -> ('a * 'b,'e) t
  (** Syntax for {!both}. *)

  (** {1 Applicative syntax} *)

  val (let+) : ('a,'e) t -> ('a -> 'b) -> ('b, 'e) t
  (** Syntax for {!map}. *)

  val (and+) : ('a,'e) t -> ('b,'e) t -> ('a * 'b,'e) t
  (** Syntax for {!both}. *)
end

include module type of Infix
