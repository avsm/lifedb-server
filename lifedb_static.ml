open Utils

let serve_config (cgi:Netcgi.cgi_activation) = function
    |["config"] ->
        cgi#output#output_string (Lifedb_config.string_of_config ());
    |_ ->
        Lifedb_rpc.return_error cgi `Not_found "Not found" "Unknown config request"
