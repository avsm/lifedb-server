(*pp $PP *)

open Utils

module Dir = struct
    let lifedb_dir = ref "" 
    let cache_dir = ref ""
    let log_dir = ref ""
    let plugins_dir = ref []
    let config_dir = ref ""

    let lifedb () = !lifedb_dir
    let lifedb_db () = Filename.concat !lifedb_dir "life.db"
    let cache () = !cache_dir
    let log () = !log_dir
    let plugins () = !plugins_dir
    let config () = !config_dir
    let passwd_db () = Filename.concat !config_dir "passwd.db"
end

let test_mode_val = ref false
let test_mode () = !test_mode_val

let config_filename_val = ref ""
let config_filename () = !config_filename_val

let root_user () = "root"

let read_config file =
    let json = Json_io.load_json file in
    let conf = Lifedb.Rpc.Config.t_of_json json in
    let subst = Str.global_substitute (Str.regexp_string "$HOME") (fun _ -> Sys.getenv("HOME")) in
    Dir.lifedb_dir := subst conf#lifedb_directory;
    Dir.plugins_dir := List.map subst conf#plugins_directory;
    Dir.log_dir := subst conf#log_directory;
    Dir.cache_dir := subst conf#cache_directory;
    Dir.config_dir := subst conf#config_directory;
    test_mode_val := conf#test_mode;
    config_filename_val := realpath file

let string_of_config () =
    let json = Lifedb.Rpc.Config.json_of_t (object
       method lifedb_directory = Dir.lifedb ()
       method plugins_directory = Dir.plugins ()
       method log_directory = Dir.log ()
       method cache_directory = Dir.cache ()
       method config_directory = Dir.config ()
       method test_mode = test_mode ()
    end) in
    Json_io.string_of_json json
