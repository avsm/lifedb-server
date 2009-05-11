(* Copyright (C) 2009 Anil Madhavapeddy <anil@recoil.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*)

open Utils
open Printf
module LS=Lifedb_schema
module SS=Sync_schema

(* return the set of uids which the remote user doesnt have *)
let filter_new (user:SS.User.t) es =
  let has_uids = (guids_of_blob user#has_guids) @ (guids_of_blob user#sent_guids) in
  let f = List.filter (fun e -> not (List.mem e#uid has_uids)) es in
  Log.logmod "Filter" "Filtering new entries -> %s (%d orig, %d results)" user#uid (List.length es) (List.length f);
  f

(* return the set of uids which are addressed to the remote user *)
let filter_recipients user es =
  let f = List.filter (fun e ->
    List.length (
      List.find_all (fun s ->
        s#name = "email" && s#uid = user#uid
      ) e#recipients
    ) > 0
  ) es in
  Log.logmod "Filter" "Filtering entries addressed to -> %s (%d results)" user#uid (List.length f);
  f

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
