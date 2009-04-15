open Utils
open Printf
module LS=Lifedb_schema
module SS=Sync_schema

(* return the set of uids which the remote user doesnt have *)
let filter_new (user:SS.User.t) =
  Log.logmod "Filter" "Filtering new entries -> %s" user#uid;
  let has_uids = List.map (fun g -> g#guid) (user#has_guids @ user#sent_guids) in
  List.filter (fun e -> not (List.mem e#uid has_uids))

(* return the set of uids which are addressed to the remote user *)
let filter_recipients user =
  Log.logmod "Filter" "Filtering entries addressed to -> %s" user#uid;
  List.filter (fun e ->
    List.length (
      List.find_all (fun s ->
        s#name = "email" && s#uid = user#uid
      ) e#recipients
    ) > 0
  )

(* apply a single filter and return a set of entries *)
let apply_filter lifedb syncdb (user:SS.User.t) (entries:LS.Entry.t list) (filter:SS.Filter_rule.t) =
  match filter#body with
  |"add *" -> begin
    (* no need to preserve incoming uids as this just adds them all to output *)
    filter_new user (LS.Entry.get lifedb)
  end
  |"add * where #remote in recipients" -> begin
    filter_recipients user (filter_new user (LS.Entry.get lifedb)) @ entries
  end
  |_ -> failwith "unknown filter rule"

(* given a user record, return a list of entries which need to go to the user *)
let apply_filters lifedb syncdb (user:SS.User.t) =
  (* by default, we do not send any data to the remote user *)
  let entries = [] in
  (* retrieve filters in descending zorder to apply *)
  let filters = List.sort (fun a b -> compare a#zorder b#zorder) user#filters in
  List.fold_left (apply_filter lifedb syncdb user) entries filters
