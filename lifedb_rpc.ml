(*pp $PP *)

open Netcgi1_compat.Netcgi_types
open Printf

type json rpc_params = string list

let text = Netencoding.Html.encode_from_latin1;;

exception Invalid_rpc of string

let pp (cgi : Netcgi.cgi_activation) =
    let out = cgi # output # output_string in
    match cgi # argument_value "f" with
    |"" -> raise (Invalid_rpc "func not specified")
    |"login" -> begin
       let p = cgi#argument_value "p" in
       let params = rpc_params_of_json (Json_io.json_of_string p) in
       out (String.concat ";;; " params) 
    end 
    |_ -> raise (Invalid_rpc "func unknown")
    