(* autogenerated by sql_orm *)
(** Use the [[Init]] module to open a new database handle.  Each object type has its own module with functions to create, modify, save and destroy objects of that type into the SQLite database
  *)
module Init : sig
  type t
  type transaction_mode = [`Exclusive |`Deferred |`Immediate ]
  (** Database handle which can be used to create and retrieve objects
    *)
  val t :
    ?busyfn:(Sqlite3.db -> unit) -> ?mode:transaction_mode ->
    string -> t
  (** [t db_name] open a Sqlite3 database with filename [db_name] and create any tables if they are missing. @return a database handle which can be used to create and retrieve objects in the database.
   @raise Sql_error if a database error is encountered
    *)
  val db: t -> Sqlite3.db
  (** [db handle] @return the underlying Sqlite3 database handle for the connection, for advanced queries.
    *)
end
module Attachment : sig
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    file_name : string;
    set_file_name : string -> unit;
    uid : string;
    set_uid : string -> unit;
    mime_type : string;
    set_mime_type : string -> unit;
    save: int64; delete: unit
  >

  (** An object which can be stored in the database with the [save] method call, or removed by calling [delete].  Fields can be accessed via the approriate named method and set via the [set_] methods.  Changes are not committed to the database until [save] is invoked.
    *)

  val t :
    ?id:int64 option ->
    file_name:string ->
    uid:string ->
    mime_type:string ->
    Init.t -> t
  (** Can be used to construct a new object.  If [id] is not specified, it will be automatically assigned the first time [save] is called on the object.  The object is not committed to the database until [save] is invoked.  The [save] method will also return the [id] assigned to the object.
   @raise Sql_error if a database error is encountered
    *)

  val get :
    ?id:int64 option ->
    ?file_name:string option ->
    ?uid:string option ->
    ?mime_type:string option ->
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> t list
  (** Used to retrieve objects from the database.  If an argument is specified, it is included in the search criteria (all fields are ANDed together).
   @raise Sql_error if a database error is encountered
    *)

  val get_by_file_name :
    file_name:string -> 
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    t list

  val get_by_uid :
    uid:string -> 
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    t list

end
module Contact : sig
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    file_name : string;
    set_file_name : string -> unit;
    uid : string;
    set_uid : string -> unit;
    first_name : string option;
    set_first_name : string option -> unit;
    last_name : string option;
    set_last_name : string option -> unit;
    mtime : float;
    set_mtime : float -> unit;
    save: int64; delete: unit
  >

  (** An object which can be stored in the database with the [save] method call, or removed by calling [delete].  Fields can be accessed via the approriate named method and set via the [set_] methods.  Changes are not committed to the database until [save] is invoked.
    *)

  val t :
    ?id:int64 option ->
    file_name:string ->
    uid:string ->
    ?first_name:string option ->
    ?last_name:string option ->
    mtime:float ->
    Init.t -> t
  (** Can be used to construct a new object.  If [id] is not specified, it will be automatically assigned the first time [save] is called on the object.  The object is not committed to the database until [save] is invoked.  The [save] method will also return the [id] assigned to the object.
   @raise Sql_error if a database error is encountered
    *)

  val get :
    ?id:int64 option ->
    ?file_name:string option ->
    ?uid:string option ->
    ?first_name:string option ->
    ?last_name:string option ->
    ?mtime:float option ->
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> t list
  (** Used to retrieve objects from the database.  If an argument is specified, it is included in the search criteria (all fields are ANDed together).
   @raise Sql_error if a database error is encountered
    *)

  val get_by_uid :
    uid:string -> 
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    t list

end
module Mtype : sig
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    name : string;
    set_name : string -> unit;
    label : string;
    set_label : string -> unit;
    icon : string option;
    set_icon : string option -> unit;
    implements : string;
    set_implements : string -> unit;
    save: int64; delete: unit
  >

  (** An object which can be stored in the database with the [save] method call, or removed by calling [delete].  Fields can be accessed via the approriate named method and set via the [set_] methods.  Changes are not committed to the database until [save] is invoked.
    *)

  val t :
    ?id:int64 option ->
    name:string ->
    label:string ->
    ?icon:string option ->
    implements:string ->
    Init.t -> t
  (** Can be used to construct a new object.  If [id] is not specified, it will be automatically assigned the first time [save] is called on the object.  The object is not committed to the database until [save] is invoked.  The [save] method will also return the [id] assigned to the object.
   @raise Sql_error if a database error is encountered
    *)

  val get :
    ?id:int64 option ->
    ?name:string option ->
    ?label:string option ->
    ?icon:string option ->
    ?implements:string option ->
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> t list
  (** Used to retrieve objects from the database.  If an argument is specified, it is included in the search criteria (all fields are ANDed together).
   @raise Sql_error if a database error is encountered
    *)

  val get_by_name :
    name:string -> 
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    t list

end
module Service : sig
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    name : string;
    set_name : string -> unit;
    uid : string;
    set_uid : string -> unit;
    contact : Contact.t option;
    set_contact : Contact.t option -> unit;
    save: int64; delete: unit
  >

  (** An object which can be stored in the database with the [save] method call, or removed by calling [delete].  Fields can be accessed via the approriate named method and set via the [set_] methods.  Changes are not committed to the database until [save] is invoked.
    *)

  val t :
    ?id:int64 option ->
    name:string ->
    uid:string ->
    ?contact:Contact.t option ->
    Init.t -> t
  (** Can be used to construct a new object.  If [id] is not specified, it will be automatically assigned the first time [save] is called on the object.  The object is not committed to the database until [save] is invoked.  The [save] method will also return the [id] assigned to the object.
   @raise Sql_error if a database error is encountered
    *)

  val get :
    ?id:int64 option ->
    ?name:string option ->
    ?uid:string option ->
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> t list
  (** Used to retrieve objects from the database.  If an argument is specified, it is included in the search criteria (all fields are ANDed together).
   @raise Sql_error if a database error is encountered
    *)

end
module Tag : sig
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    name : string;
    set_name : string -> unit;
    save: int64; delete: unit
  >

  (** An object which can be stored in the database with the [save] method call, or removed by calling [delete].  Fields can be accessed via the approriate named method and set via the [set_] methods.  Changes are not committed to the database until [save] is invoked.
    *)

  val t :
    ?id:int64 option ->
    name:string ->
    Init.t -> t
  (** Can be used to construct a new object.  If [id] is not specified, it will be automatically assigned the first time [save] is called on the object.  The object is not committed to the database until [save] is invoked.  The [save] method will also return the [id] assigned to the object.
   @raise Sql_error if a database error is encountered
    *)

  val get :
    ?id:int64 option ->
    ?name:string option ->
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> t list
  (** Used to retrieve objects from the database.  If an argument is specified, it is included in the search criteria (all fields are ANDed together).
   @raise Sql_error if a database error is encountered
    *)

end
module Entry : sig
  type t = <
    id : int64 option;
    set_id : int64 option -> unit;
    uid : string;
    set_uid : string -> unit;
    file_name : string;
    set_file_name : string -> unit;
    created : float;
    set_created : float -> unit;
    mtype : Mtype.t;
    set_mtype : Mtype.t -> unit;
    from : Service.t;
    set_from : Service.t -> unit;
    recipients : Service.t list;
    set_recipients : Service.t list -> unit;
    atts : Attachment.t list;
    set_atts : Attachment.t list -> unit;
    tags : Tag.t list;
    set_tags : Tag.t list -> unit;
    delivered : int64;
    set_delivered : int64 -> unit;
    save: int64; delete: unit
  >

  (** An object which can be stored in the database with the [save] method call, or removed by calling [delete].  Fields can be accessed via the approriate named method and set via the [set_] methods.  Changes are not committed to the database until [save] is invoked.
    *)

  val t :
    ?id:int64 option ->
    uid:string ->
    file_name:string ->
    created:float ->
    mtype:Mtype.t ->
    from:Service.t ->
    recipients:Service.t list ->
    atts:Attachment.t list ->
    tags:Tag.t list ->
    delivered:int64 ->
    Init.t -> t
  (** Can be used to construct a new object.  If [id] is not specified, it will be automatically assigned the first time [save] is called on the object.  The object is not committed to the database until [save] is invoked.  The [save] method will also return the [id] assigned to the object.
   @raise Sql_error if a database error is encountered
    *)

  val get :
    ?id:int64 option ->
    ?uid:string option ->
    ?file_name:string option ->
    ?created:float option ->
    ?delivered:int64 option ->
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> t list
  (** Used to retrieve objects from the database.  If an argument is specified, it is included in the search criteria (all fields are ANDed together).
   @raise Sql_error if a database error is encountered
    *)

  val get_uid :
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    string list

  val get_file_name :
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    string list

  val get_created :
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    float list

  val get_from_recipients :
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    (Service.t * Service.t list) list

  val get_by_uid :
    uid:string -> 
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    t list

  val get_by_file_name :
    file_name:string -> 
    ?custom_where:string * Sqlite3.Data.t list -> Init.t -> 
    t list

end
