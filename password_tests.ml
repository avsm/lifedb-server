(* test suite for the password encryption library *)

open OUnit
open Passwords

let must = function
   |None -> assert_failure "must"
   |Some x -> x

let never = function
   |Some x -> assert_failure "never"
   |None -> ()

(* Basic test to make sure an encrypted password is reversed
   successfully with the same passphrase and salt *)
let test_encrypt_and_decrypt () =
  let tod = Int64.of_float (Unix.gettimeofday () ) in
  let origpass = "verysecret" in
  let encpass = must (encrypt_password tod "wibble" origpass) in
  let decpass = must(decrypt_password tod "wibble" encpass) in
  "password differ" @? (origpass <> encpass);
  "decpass and encpass differ" @? (decpass <> encpass);
  assert_equal origpass decpass

(* ensure a bad passphrase causes an incorrect decrypt *) 
let test_fail_decrypt () =
  let tod = Int64.of_float (Unix.gettimeofday () ) in
  let origpass = "verysecret" in
  let encpass = must(encrypt_password tod "wibble" origpass) in
  never(decrypt_password tod "foobar" encpass)

(* ensure a different time causes incorrect decrypt *)
let test_fail_decrypt_time () =
  let tod = Int64.of_float (Unix.gettimeofday () ) in
  let tod' = Int64.of_float (12345678.0) in
  let origpass = "verysecret" in
  let encpass = must(encrypt_password tod "wibble" origpass) in
  never(decrypt_password tod' "wibble" encpass);
  never(decrypt_password tod' "foobar" encpass);
  never(decrypt_password tod "foobar" encpass)

(* ensure different passwords dont have the same encrypted result *)
let test_successive_encrypt () =
  let tod = Int64.of_float (Unix.gettimeofday ()) in
  let passwds = [ "one"; "two"; "three"; "four"; "five" ] in
  let encpasswds = List.map (encrypt_password tod "wobble") passwds in
  let h = Hashtbl.create 1 in
  List.iter (fun enc ->
      let p = must enc in
      "password collision" @? (not (Hashtbl.mem h p));
      Hashtbl.add h p ();
  ) encpasswds

let suite = "Password encryption test" >:::
    ["test_encrypt_and_decrypt" >:: test_encrypt_and_decrypt;
     "test_fail_decrypt" >:: test_fail_decrypt;
     "test_fail_decrypt_time" >:: test_fail_decrypt_time;
     "test_successive_encrypt" >:: test_successive_encrypt ]

(* Returns true if the result list contains successes only *)
let rec was_successful results =
  match results with
      [] -> true
    | RSuccess _::t
    | RSkip _::t -> was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ -> false

let _ =
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the tests in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");

  while true do  
  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
    exit 1
  done
