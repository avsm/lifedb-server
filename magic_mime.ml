
let initialized = ref false
let magic = Hashtbl.create 1

let read_magic_mime () =
  let fin = open_in "mime.types" in
  let split = Netstring_pcre.split (Pcre.regexp "[ \r\t\n]+") in
  try
    while true do
      let l = input_line fin in
      if l = "" || l.[0] <> '#' then (
        match split l with
          |[mime_type] -> ()
          |[] -> ()
          |mime_type :: exts ->
            let m = String.lowercase mime_type in
            List.iter (fun ext -> Hashtbl.replace magic ext m) exts
      )
    done; ()
  with
  |End_of_file -> close_in fin
  |e -> close_in fin; raise e

let lookup ext =
  if not !initialized then
    read_magic_mime ();
  try
    Hashtbl.find magic (String.lowercase ext)
  with
    |Not_found -> "application/octet-stream"

