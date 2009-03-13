(*pp $PP *)

open Netcgi1_compat.Netcgi_types
open Printf

type json rpc_result_error = {
    error : string;
    reason : string
}
  
exception Invalid_rpc of string

let passphrase = ref ""

let return_error (cgi:Netcgi.cgi_activation) (code:Nethttp.http_status) error reason =
    cgi#output#rollback_work ();
    cgi#set_header ~status:code ~cache:`No_cache 
        ~content_type:"application/json; charset=\"iso-8859-1\"" ();
    let resp = Json_io.string_of_json (json_of_rpc_result_error 
        { error=error; reason=reason }) in
    cgi#output#output_string resp

let return_need_auth (cgi:Netcgi.cgi_activation) =
    cgi#output#rollback_work ();
    Log.logmod "Auth" "Unauthorized request for %s" (cgi#url ());
    cgi#set_header ~status:`Unauthorized ~cache:`No_cache ();
    Nethttp.Header.set_www_authenticate cgi#environment#output_header
        ["basic", ["realm", "LifeDB credentials"]]

let check_passwd username passwd =
    (username = (Lifedb_config.root_user ())) && (passwd = (!passphrase))

let check_auth cgi =
    try begin
        let authty, authinf = Nethttp.Header.get_authorization cgi#environment#input_header in
        let passwd = Netencoding.Base64.decode (List.assoc "credentials" authinf) in
        let r = match Str.bounded_split (Str.regexp_string ":") passwd 2 with
        |[username;passwd] -> check_passwd username passwd
        |_ -> false in
        if not r then return_need_auth cgi;
        r
    end with
        Not_found -> begin
            return_need_auth cgi; false
        end

