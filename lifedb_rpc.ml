open Netcgi1_compat.Netcgi_types
open Printf

let text = Netencoding.Html.encode_from_latin1;;

let pp (cgi : Netcgi.cgi_activation) =
    let out = cgi # output # output_string in
    match cgi # argument_value "func" with
    |"" -> out "must specify func"
    |"login" -> out "login ok!"
    
