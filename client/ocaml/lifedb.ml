(*pp $PP *)

open Printf

module Rpc = struct

  module Config = struct
    type json t = <
      lifedb_directory: string;
      config_directory: string;
      plugins_directory: string list;
      log_directory: string;
      cache_directory: string;
      test_mode: bool
    >
  end

  module Plugin = struct
    type json t = <
      name : string;
      cmd : string;
      declares: decl list
      >
    and decl = <
      pltype : string;
      description : string;
      implements : string;
      ?icon : string option
    > and r = <
      info: t;
      dir: string
    >
    and ts = (string, r) Hashtbl.t
  end

  module Task = struct
    type json t = <
      plugin: string;
      mode: string;
      silo: string;
      ?period: int option;
      ?secret: passwd option;
      ?args: (string, string) Hashtbl.t option
     >
    and passwd = <
      service: string;
      username: string
    >
    and r = <
      info: t;
      duration: float;  (* time the task has been running, float seconds *)
      ?pid: int option
    >
    and rs = (string,r) Hashtbl.t
  end
end
