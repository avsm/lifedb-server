(*pp $PP *)

open Lifedb
open Http_client.Convenience
open Printf

let repeat_until_eof fn =
   try while true do
      fn ()
   done
   with End_of_file -> ()


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
  let post_raw frag body =
     process (fun uri ->
       http_post_raw_message ~callfn:(fun p ->
          let rh = p#request_header `Base in
          rh#update_field "content-type" "application/json";
          p#set_request_header rh)
        uri body
     ) frag in
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
      get "task/_all" >>== Rpc.Task.rs_of_json

    method debug_tasks = 
      self#tasks >>>= Rpc.Task.json_of_rs

    method plugin n =
      get ("plugin/"^n) >>== Rpc.Plugin.t_of_json

    method debug_plugin n =
      self#plugin n >>>= Rpc.Plugin.json_of_t
 
    method plugins =
      get ("plugin/_all") >>== Rpc.Plugin.ts_of_json

    method debug_plugins =
      self#plugins >>>= Rpc.Plugin.json_of_ts
   
    method plugins_scan =
      ignore(post_raw "plugin/_scan" "{}")

    method config = get "config"
    method debug_config = debug (get "config")

    method doc id =
      get (sprintf "doc/%s" id) >>== Rpc.Entry.doc_of_json

    method debug_doc id =
      self#doc id >>>= Rpc.Entry.json_of_doc

    method month_list year month =
      get (sprintf "/date/%.4d/%.2d" year month) >>== Rpc.Query.month_list_of_json
 
    method debug_month_list year month =
      self#month_list year month >>>= Rpc.Query.json_of_month_list

    method day_list year month day =
      get (sprintf "/date/%.4d/%.2d/%.2d" year month day) >>== Rpc.Query.day_list_of_json
    
    method debug_day_list year month day =
      self#day_list year month day >>>= Rpc.Query.json_of_day_list

    method pltypes = 
      get "pltype" >>== Rpc.Plugin.decls_of_json

    method debug_pltypes =
      self#pltypes >>>= Rpc.Plugin.json_of_decls

    method pltype_info pltype =
      get (sprintf "pltype/%s" pltype) >>== Rpc.Plugin.decl_of_json

    method debug_pltype_info pltype =
      self#pltype_info pltype >>>= Rpc.Plugin.json_of_decl

  end
