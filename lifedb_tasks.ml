(*pp $PP *)
(* Task scheduling *)

open Printf
open Utils

exception Task_error of string
exception Internal_task_error of string

let m = Mutex.create ()

type json rpc_task_create = <
   name: string;
   cmd: string;
   mode: string;
   ?period: int option
>

type json rpc_task_destroy = <
   name: string
>

type json rpc_task = <
   cmd: string;
   mode: string;
   ?period: int option;
   ?pid: int option
>
and rpc_task_list = (string,rpc_task) Hashtbl.t

type task_mode = 
   |Periodic of int
   |Single 
   |Constant

type task_state = {
   cmd: string;
   mode: task_mode; 
   running: Fork_helper.task;
}

let task_list = Hashtbl.create 1
let task_table_limit = 10

let json_of_task t =
   let mode,period = match t.mode with
   |Periodic p -> "periodic", (Some p)
   |Single -> "single", None
   |Constant -> "constant", None in
   let pid = Fork_helper.pid_of_task t.running in
   object
       method cmd=t.cmd
       method mode=mode
       method period=period
       method pid=pid
   end
 
let string_of_task t =
    let running = Fork_helper.string_of_task t.running in
    let mode = match t.mode with 
    |Single -> "single"
    |Periodic p -> sprintf "periodic (every %d sec)" p
    |Constant -> "constant" in
    sprintf "(%s) : %s (%s)" mode t.cmd running

let log_task_table () = 
    Netplex_cenv.log `Info "logging task table...";
    Hashtbl.iter (fun name state ->
        Netplex_cenv.logf `Info "%s: %s" name (string_of_task state)
    ) task_list

let find_task name =
    try
       Some (Hashtbl.find task_list name)
    with
       Not_found -> None

let create_task params =
    if Hashtbl.length task_list >= task_table_limit then
       raise (Task_error "too many tasks already registered");
    let mode = match String.lowercase params#mode, params#period with
    |"periodic", (Some p) -> Periodic p
    |"single",_ -> Single
    |"constant",_ -> Constant
    |_,_ -> raise (Task_error "unknown task mode") in
    let outfn = print_endline in
    let errfn = print_endline in
    let task_status = Fork_helper.create params#cmd outfn errfn in
    let task = { cmd=params#cmd; mode=mode; running=task_status } in
    Hashtbl.add task_list params#name task;
    Netplex_cenv.logf `Debug "Added task: %s" (string_of_task task)

let destroy_task name =
    match find_task name with
    |Some task -> begin
        let final_status = Fork_helper.destroy task.running in
        Hashtbl.remove task_list name;
        Netplex_cenv.logf `Info "Task %s destroyed: %s" name 
            (Fork_helper.string_of_status final_status);
    end
    |None -> raise (Task_error "task not found")

let dispatch cgi = function
   |`Create p -> begin
       let params = rpc_task_create_of_json (Json_io.json_of_string p) in
       with_lock m (fun () ->
           match find_task params#name with
           |Some state ->
               Lifedb_rpc.return_error cgi `Bad_request "Task already exists" "Use a different id"
           |None -> begin
               try
                 create_task params
               with
               |Task_error err ->
                  Lifedb_rpc.return_error cgi `Bad_request "Task error" err
           end
       )
   end
   |`List -> begin
       with_lock m (fun () ->
           let resp = Hashtbl.create 1 in
           Hashtbl.iter (fun name state -> Hashtbl.add resp name (json_of_task state)) task_list;
           cgi#output#output_string (Json_io.string_of_json (json_of_rpc_task_list resp))
       )
   end
   |`Destroy p -> begin
       let params = rpc_task_destroy_of_json (Json_io.json_of_string p) in
       with_lock m (fun () ->
           try
               destroy_task params#name
           with |Task_error err ->
               Lifedb_rpc.return_error cgi `Bad_request "Task error" err
       )
   end

let singleton () =
    let hooks = object
        inherit Netplex_kit.empty_processor_hooks() as super

        val mutable signal_stop = true

        method post_start_hook c =
            super#post_start_hook c;
            ignore(Thread.create (fun () ->
                while signal_stop do
                    c#log `Info (sprintf "Checking tasks to call");
                    Thread.delay 10.;
                done;
                c#log `Debug "Terminating task thread"
            ) ())
            
        method receive_admin_message c msg args =
            c#log `Info (sprintf "received admin msg %s [%s]" msg (String.concat "," (Array.to_list args)));
            match msg,args with
            |"tasks",[|"dump"|] ->
                c#log `Debug "Dumping tasks table";
                with_lock m log_task_table
            |_ -> ()
            
        method shutdown () =
            signal_stop <- false;
            super#shutdown ()
    end in

    object (self)
        method name = "tasks"
        method create_processor _ _ _ =
            object (self)
            inherit Netplex_kit.processor_base hooks
            method process ~when_done _ _ _ = when_done () (* should not ever be called *)
            method supported_ptypes = [ `Multi_threading ]
        end
    end
