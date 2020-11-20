open Ppxlib

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

let replacement_prefix_id =
  [ (Longident.parse "Lwt", Longident.parse "Lwt_debug");
    (Longident.parse "Error_monad", Longident.parse "Error_monad_debug");
    ( Longident.parse "Tezos_error_monad.Error_monad",
      Longident.parse "Tezos_error_monad.Error_monad_debug" ) ]

let replace_module_prefix_id ~replacement_prefix_id longid =
  let rec loop replacements changed id =
    match replacements with
    | [] ->
        (changed, id)
    | (replace_prefix_id, replacement_prefix_id) :: next ->
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
        let (new_changed, new_id) = replace id in
        loop next (changed || new_changed) new_id
  in
  loop replacement_prefix_id false longid
