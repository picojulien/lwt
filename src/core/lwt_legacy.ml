include Lwt_debug


let  wait () = wait ?pos:None ()

let return a = return ?pos:None a
(** [Lwt.return v] creates a new {{: #TYPEt} promise} that is {e already
    fulfilled} with value [v].

    This is needed to satisfy the type system in some cases. For example, in a
    [match] expression where one case evaluates to a promise, the other cases
    have to evaluate to promises as well:

{[
match need_input with
| true -> Lwt_io.(read_line stdin)   (* Has type string Lwt.t... *)
| false -> Lwt.return ""             (* ...so wrap empty string in a promise. *)
]}

    Another typical usage is in {{: #VALbind} [let%lwt]}. The expression after
    the “[in]” has to evaluate to a promise. So, if you compute an ordinary
    value instead, you have to wrap it:

{[
let%lwt line = Lwt_io.(read_line stdin) in
Lwt.return (line ^ ".")
]} *)

let fail a = fail ?pos:None a
(** [Lwt.fail exn] is like {!Lwt.return}, except the new {{: #TYPEt} promise}
    that is {e already rejected} with [exn].

    Whenever possible, it is recommended to use [raise exn] instead, as [raise]
    captures a backtrace, while [Lwt.fail] does not. If you call [raise exn] in
    a callback that is expected by Lwt to return a promise, Lwt will
    automatically wrap [exn] in a rejected promise, but the backtrace will have
    been recorded by the OCaml runtime. Use [Lwt.fail] only when you
    specifically want to create a rejected promise, to pass to another function,
    or store in a data structure. *)



(** {3 Callbacks} *)

let bind p callback = bind ?pos:None p callback

let catch f e = catch ?pos:None f e

let finalize f ff  = finalize ?pos:None f ff

let try_bind p c = try_bind ?pos:None p c

let both a b = both ?pos:None a b

let join = join ?pos:None

let all ps = all ?pos:None ps

let pick ps = pick ?pos:None ps

let choose ps = choose ?pos:None ps

let npick ps = npick ?pos:None ps

let nchoose ps = nchoose ?pos:None ps

let nchoose_split ps = nchoose_split ?pos:None ps

let task () = task ?pos:None ()


let protected p = protected ?pos:None p

let no_cancel p = no_cancel ?pos:None p

let map ps = map ?pos:None ps


module Infix =
struct
  let (>>=) p f = bind p f
  let (=<<) f p = bind p f
  let (>|=) p f = map  f p
  let (=|<) f p  = map f p
  let (<&>) p p' = join [p; p']
  let (<?>) p p' = choose [p; p']

  module Let_syntax =
  struct
    let return  = return
    let map  t ~f = map f t
    let bind t ~f = bind t f
    let both = both

    module Open_on_rhs =
    struct
    end
  end
end
include Infix

let of_result r = of_result ?pos:None r



(** {3 Linked lists of promises} *)

[@@@ocaml.warning "-3"]

let add_task_r s  = add_task_r ?pos:None s
  [@@ocaml.deprecated]

let add_task_l s = add_task_l ?pos:None s
  [@@ocaml.deprecated]

[@@@ocaml.warning "+3"]



(** {3 Yielding} *)

let pause = pause ?pos:None

let wrap f = wrap ?pos:None f

let wrap1 f x1 = wrap1 ?pos:None f x1
let wrap2 f x1 x2 = wrap2 ?pos:None f x1 x2
let wrap3 f x1 x2 x3 = wrap3 ?pos:None f x1 x2 x3
let wrap4 f x1 x2 x3 x4 = wrap4 ?pos:None f x1 x2 x3 x4
let wrap5 f x1 x2 x3 x4 x5 = wrap5 ?pos:None f x1 x2 x3 x4 x5
let wrap6 f x1 x2 x3 x4 x5 x6 = wrap6 ?pos:None f x1 x2 x3 x4 x5 x6
let wrap7 f x1 x2 x3 x4 x5 x6 x7 = wrap7 ?pos:None f x1 x2 x3 x4 x5 x6 x7

(** {3 Trivial promises} *)

let return_some v = return_some ?pos:None v

let return_ok v = return_ok ?pos:None v

let return_error e = return_error ?pos:None e

let fail_with e = fail_with ?pos:None e

let fail_invalid_arg a = fail_invalid_arg ?pos:None a

(** {3 Unscoped infix operators} *)

(** {3 Miscellaneous} *)
let apply f p  = apply ?pos:None f p

let backtrace_bind c p f  = backtrace_bind ?pos:None c p f
let backtrace_catch a b c  = backtrace_catch ?pos:None a b c
let backtrace_finalize a b c = backtrace_finalize ?pos:None a b c
let backtrace_try_bind a b c = backtrace_try_bind ?pos:None a b c
