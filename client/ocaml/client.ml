(*pp $PP *)

open Http_client.Convenience
open Printf

let repeat_until_eof fn =
   try while true do
      fn ()
   done
   with End_of_file -> ()


module Rpc = struct

  module Config = struct
    type json t = <
      lifedb_directory: string;
      config_directory: string;
      plugins_directory: string list;
      log_directory: string;
      cache_directory: string;
      test_mode: bool
    >
  end

  module Plugin = struct
    type json t = <
      name : string;
      cmd : string;
      declares: decl list;
      dir: string
      >
    and decl = <
      pltype : string;
      description : string;
      implements : string;
      ?icon : string option
    >
    and ts = (string, t) Hashtbl.t
    
  end

  module Task = struct
    type json create = <
     name: string;
     cmd: string;
     mode: string;
     silo: string;
     ?cwd: string option;
     ?period: int option;
     ?secret: passwd option;
     ?args: (string, string) Hashtbl.t option
    >
    and passwd = <
      service: string;
      username: string
    >
    and destroy = <
      name: string
    >
    and t = <
     cmd: string;
     mode: string;
     silo: string;
     duration: float;
     ?period: int option;
     ?pid: int option;
     ?secret: passwd option;
     ?args: (string , string) Hashtbl.t option
    >
    and ts = (string,t) Hashtbl.t
  end
end

class client url username password =
  let uri frag = sprintf "http://%s:%s@%s/%s" username password url frag in
  let string_of_chan cin = 
      let buf = Buffer.create 2048 in
      repeat_until_eof (fun () ->
          Buffer.add_string buf (cin#input_line ());
      );
      Buffer.contents buf in
  let process fn frag = 
    try 
       let res = fn (uri frag) in
       match res#response_status with
       |`Ok -> Some (string_of_chan (res#response_body#open_value_rd ()))
       |_ -> None 
    with Http_client.Http_protocol _ -> None

  in
  let get frag = process http_get_message frag in
  let debug = function
     |Some x -> print_endline (sprintf "http success: %s" x)
     |None -> print_endline (sprintf "http fail") in
  let mapopt fn = function |Some x -> Some (fn x) |None -> None in
  let (>>>=) v offn = 
     debug (mapopt (fun t -> Json_io.string_of_json (offn t)) v) in
  let (>>==) v apfn =
     mapopt (fun r -> apfn (Json_io.json_of_string r)) v in
  object(self)
    method ping = get "ping"
    method debug_ping = debug self#ping

    method task n =
     get ("task/"^n) >>== Rpc.Task.t_of_json

    method debug_task n =
      self#task n >>>= Rpc.Task.json_of_t
 
    method tasks =
      get "task/_all" >>== Rpc.Task.ts_of_json

    method debug_tasks = 
      self#tasks >>>= Rpc.Task.json_of_ts

    method plugin n =
      get ("plugin/"^n) >>== Rpc.Plugin.t_of_json

    method debug_plugin n =
      self#plugin n >>>= Rpc.Plugin.json_of_t
 
    method plugins =
      get ("plugin/_all") >>== Rpc.Plugin.ts_of_json

    method debug_plugins =
      self#plugins >>>= Rpc.Plugin.json_of_ts
   

    method config = get "config"
    method debug_config = debug (get "config")
  end
