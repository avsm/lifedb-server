let try_final fn finalfn =
  begin
  try fn ()
  with e -> (finalfn(); raise e) end;
  finalfn ()

let repeat_until_eof fn =
   try while true do
      fn ()
   done
   with End_of_file -> ()

let with_lock m fn =
    Mutex.lock m;
    try
        let r = fn () in
        Mutex.unlock m;
        r
    with e -> begin
        Mutex.unlock m;
        raise e
   end

let current_datetime () =
    let tm = Unix.gmtime (Unix.gettimeofday ()) in
    Printf.sprintf "%.4d/%.2d/%.2d %.2d:%.2d:%.2d" (1900+tm.Unix.tm_year) tm.Unix.tm_mon
        tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* read next directory in an open dir_handle *)
external read_next_dir : Unix.dir_handle -> string = "unix_read_next_dir"

(* wrapper for realpath(2) *)
external realpath : string -> string = "unix_realpath"

exception Unable_to_make_dirs of (string * string)

let make_dirs dir =
    let rec fn dir accum = 
      match dir with
      |"/"|""|"." -> raise (Unable_to_make_dirs (dir, String.concat "/" accum))
      |_ when try Sys.is_directory dir with Sys_error _ -> false ->
         ignore(List.fold_left (fun a b ->
              let c = Filename.concat a b in
              Unix.handle_unix_error Unix.mkdir c 0o755;
              c) dir accum)
      |_ ->
         fn (Filename.dirname dir) ((Filename.basename dir) :: accum)
   in fn dir []

let get_extension name =
  let rec search_dot i =
    if i < 1 || name.[i] = '/' then invalid_arg "Filename.chop_extension"
    else if name.[i] = '.' then String.sub name (i+1) (String.length name - i - 1)
    else search_dot (i - 1) in
  search_dot (String.length name - 1)

