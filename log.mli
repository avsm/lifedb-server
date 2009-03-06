
type log_request = [ 
 | `Debug of string
 | `Module of string * string
 | `Plugin of string * float * int 
]

val push : log_request -> unit

val init : unit -> unit

val logmod : string -> ('a, unit, string, unit) format4 -> 'a
