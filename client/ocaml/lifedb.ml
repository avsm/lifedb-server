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
      static_directory: string;
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

  module Entry = struct

    type json t = <
      _type: string;
      _timestamp: float;
      ?_uid : string option;
      ?abrecord: string option;
      ?_from: addr option;
      ?_to: addr list option;
      ?first_name: string option;
      ?last_name: string option;
      ?_services: (string, string list) Hashtbl.t option;
      ?subject: string option;
      ?duration: int option;
      ?text: string option;
      ?_att: string list option
    >
    and addr = (string * string) assoc
  end

  module Query = struct
    type json day_list = <
      date: float;
      ids: string list
    >

    type json month_list = <
      year: int;
      month: int;
      days: int array
    >

  end

end
