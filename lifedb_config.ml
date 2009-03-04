(*pp $PP *)

type json config = <
    lifedb_directory: string;
    plugins_directory: string list;
    log_directory: string;
    cache_directory: string;
    test_mode: bool
>

module Dir = struct
    let lifedb_dir = ref "" 
    let cache_dir = ref ""
    let log_dir = ref ""
    let plugins_dir = ref []
  
    let lifedb () = !lifedb_dir
    let cache () = !cache_dir
    let log () = !log_dir
    let plugins () = !plugins_dir
end

let test_mode_val = ref false
let test_mode () = !test_mode_val

let read_config file =
    let json = Json_io.load_json file in
    let conf = config_of_json json in
    let subst = Str.global_substitute (Str.regexp_string "$HOME") (fun _ -> Sys.getenv("HOME")) in
    Dir.lifedb_dir := subst conf#lifedb_directory;
    Dir.plugins_dir := List.map subst conf#plugins_directory;
    Dir.log_dir := subst conf#log_directory;
    Dir.cache_dir := subst conf#cache_directory;
    test_mode_val := conf#test_mode
