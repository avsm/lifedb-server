(*pp $PP *)

open Netcgi1_compat.Netcgi_types
open Printf

type json rpc_result_error = {
    error : string;
    reason : string
}
  
exception Invalid_rpc of string

let return_error (cgi:Netcgi.cgi_activation) (code:Nethttp.http_status) error reason =
    cgi#output#rollback_work();
	cgi#set_header ~status:code
	  ~cache:`No_cache 
	  ~content_type:"application/json; charset=\"iso-8859-1\""
	  ();
	let resp = Json_io.string_of_json (json_of_rpc_result_error 
	    { error=error; reason=reason }) in
    let cont = Netplex_cenv.self_cont() in
    cont#log `Info (sprintf "return_error: %s" resp);
	cgi#output#output_string resp
    
