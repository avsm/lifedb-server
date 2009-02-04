(*pp $PP *)

open Netcgi1_compat.Netcgi_types
open Printf

type json rpc_result_mode = Success | Failure
and rpc_result = {
   result : rpc_result_mode;
   args : string
}
  
exception Invalid_rpc of string

let pp (cgi : Netcgi.cgi_activation) =
    let out = cgi # output # output_string in
    match cgi # argument_value "f" with
    |"" -> raise (Invalid_rpc "func not specified")
    |"login" -> Lifedb_session.dispatch cgi (cgi#argument_value "p")
    |"cache" -> begin
       let p = cgi#argument_value "p" in
       Lifedb_cache.lockfn (fun () ->
           let v = Lifedb_cache.get () in
           Thread.delay (Random.float 1.);
           let v' = p :: v in
           Lifedb_cache.set v';
           out (String.concat "," v');
       );
       out "\n";
    end 
    |_ -> raise (Invalid_rpc "func unknown")
    