
type log_request = [ `Debug of string | `Plugin of string * float * int ]
val push : log_request -> unit
val init : unit -> unit
