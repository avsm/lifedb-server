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
    with e ->
        Mutex.unlock m;
        raise e

