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

open Nethttpd_types
open Nethttpd_services
open Nethttpd_reactor
open Printf

let http_config lifedb syncdb =
  let mime_types = read_media_types_file "./mime.types" in
  let static = {
    file_docroot = Lifedb_config.Dir.static ();
    file_uri = "/static";
    file_suffix_types = mime_types;
    file_default_type = "application/octet-stream";
    file_options = [ `Enable_gzip ]
  } in
  let srv = host_distributor
    [ default_host ~pref_name:"localhost" ~pref_port: (Lifedb_config.port ()) (),
      uri_distributor [
        "*", (options_service());
        "/static", (file_service static);
        "/", (dynamic_service { 
          dyn_handler = Lifedb_dispatch.handler lifedb syncdb;
          dyn_activation = std_activation `Std_activation_buffered;
          dyn_uri = None;
          dyn_translator = (fun _ -> "");
          dyn_accept_all_conditionals = false
        })
      ]
    ] in
  srv

let init () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let config_cgi = { Netcgi1_compat.Netcgi_env.default_config with
          Netcgi1_compat.Netcgi_env.permitted_input_content_types =
            "application/x-www-form-urlencoded" :: (Magic_mime.all_mime_types ());
          permitted_http_methods = ["GET";"HEAD";"POST";"DELETE";"PUT"] } in

  let config : http_reactor_config = object
      method config_timeout_next_request = 15.0
      method config_timeout = 300.0
      method config_reactor_synch = `Write
      method config_cgi = config_cgi
      method config_error_response n = "<html>Error " ^ string_of_int n ^ "</html>"
      method config_log_error _ _ _ _ msg =
        printf "Error log: %s\n" msg
      method config_max_reqline_length = 256
      method config_max_header_length = 32768
      method config_max_trailer_length = 32768
      method config_limit_pipeline_length = 5
      method config_limit_pipeline_size = 250000
      method config_announce_server = `As "LifeDB"
    end in

  let master_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt master_sock Unix.SO_REUSEADDR true;
  Unix.bind master_sock (Unix.ADDR_INET(Unix.inet_addr_any, (Lifedb_config.port ())));
  Unix.listen master_sock 50;

  let http_config () =
    let lifedb = Lifedb_schema.Init.t (Lifedb_config.Dir.lifedb_db ()) in
    let syncdb = Sync_schema.Init.t (Lifedb_config.Dir.sync_db ()) in
    http_config lifedb syncdb in
  while true do
    try
      Gc.compact ();
      let conn_sock, _ = Unix.accept master_sock in
      Unix.set_nonblock conn_sock;
      let _ = Thread.create (process_connection config conn_sock) (http_config ()) in
      (* process_connection config conn_sock http_config; *)
      ()
    with
        Unix.Unix_error(Unix.EINTR,_,_) -> () 
  done

