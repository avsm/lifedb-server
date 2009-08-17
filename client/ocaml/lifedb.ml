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
      port: int
    >
  end

  module User = struct
    type json t = <
      uid: string;
      ip: string;
      port: int;
      key: string
     >
    and ts = <
      results: int;
      rows: t list
    >

    type json sync = <
      guids: string list
    >

    type json filter = <
      name: string;
      body: string;
      zorder: int
    > and filters = <
      results: int;
      rows: filter list
    >
  end

  module Plugin = struct
    type json t = <
      name : string;
      cmd : string;
      declares: decl list
      > 
    and tr = <
      name : string;
      cmd : string;
      declares: decl list;
      dir : string
    >
    and decl = <
      pltype : string;
      description : string;
      implements : string;
      ?icon : string option
    > and rs = <
      results: int;
      rows: tr list
    >
    and decls = decl list
    and passwd_t = <
      password: string;
      comment: string
    > 
    and passwd_r = <
      service: string;
      username: string;
      password: string;
      comment: string
    > and passwd_rs = <
      results: int;
      rows: passwd_r list
    >
  end

  module Task = struct
    type json in_t = <
      plugin: string;
      mode: string;
      silo: string;
      ?period: int option;
      ?secret: passwd option;
      ?args: string list
     >
    and passwd = <
      service: string;
      username: string
    >
    and in_r = <
      name: string;
      plugin: string;
      mode: string;
      silo: string;
      ?period: int option;
      ?secret: passwd option;
      ?args: string list;
      ?pid: int option;
      duration: float  (* time the task has been running, float seconds *)
    >
    and in_rs = <
      results: int;
      rows: in_r list
    >
  end

  module Entry = struct

    type json contact = <
       id: string;
       first_name: string option;
       last_name: string option;
       uid: string
    >
    and addr =
      (string * string) assoc
    and t = <
      _type: string;
      _timestamp: float;
      _uid : string;
      ?abrecord: string option;
      ?_from: addr option;
      ?_to: addr list option;
      ?first_name: string option;
      ?last_name: string option;
      ?_services: (string, string list) Hashtbl.t option;
      ?subject: string option;
      ?duration: int option;
      ?text: string option;
      ?_att: string list option;
      ?_tags: string list option
    >
    and doc = <
      entry: t;
      contacts: (string, (string, contact) Hashtbl.t) Hashtbl.t
    > 
    and ts = <
      results: int;
      rows: t list
    >
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

    type json contact = <
      first_name: string option; 
      last_name: string option;
      uid: string
    > 
    and contacts = <
      results: int;
      rows: contact list
    >

  end
end
