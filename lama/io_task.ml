(*

  Lama library -- Lama arbitrary module assortement

  Copyright (C) 2006,2007  M E Leypold, Software-Dienstleistungen und Beratung

  ---------------------------------------------------------------------------
  
  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  version 2 as published by the Free Software Foundation (no later
  version).
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
  02110-1301 USA.

  ---------------------------------------------------------------------------
  
  ...                                                   

*)






(* IO Events take 2

   Design criteria:

   - processes should feel more like threads

   - explicit scheduling / unscheduling of "blocked" processes/actions
     instead of implicit in thread result (only Yield/Terminate is in
     thread result).
*)


(* let dbg_ch = open_out "DBG.log";; *)

type condition = {

  fd     : Unix.file_descr  ;
  action : process          ;
}

and system = {

  input_conditions    : (Unix.file_descr,condition) Hashtbl.t;
  output_conditions   : (Unix.file_descr,condition) Hashtbl.t;
  queue               : process Queue.t ;
}
and process_continuation = Continue_Task of process | Terminate_Task
and process              = unit -> process_continuation
;;

let new_system ?(in_table_size=5) ?(out_table_size=5) () =
  {
    input_conditions  = Hashtbl.create in_table_size;
    output_conditions = Hashtbl.create out_table_size;
    queue             = Queue.create ();
  }
;;

let input_fd_list s =
  Hashtbl.fold (fun key r l -> r.fd :: l) s.input_conditions  []
;;

let output_fd_list s =
  Hashtbl.fold (fun key r l -> r.fd :: l) s.output_conditions []
;;

let schedule_process s p  = Queue.add p s.queue ;;
let add_input        s r  = Hashtbl.add s.input_conditions     r.fd r ;;
let add_output       s r  = Hashtbl.add s.output_conditions    r.fd r ;;
let remove_input     s fd = Hashtbl.remove s.input_conditions  fd  ;;
let remove_output    s fd = Hashtbl.remove s.output_conditions fd  ;;

let conditions_pending s  = Hashtbl.length s.input_conditions > 0 || Hashtbl.length s.output_conditions > 0;;
let processes_runnable  s = Queue.length s.queue > 0;;

let wakeup s t l   (* wakeup all in a category *)
    = 
  List.iter (fun fd -> schedule_process s ((Hashtbl.find t fd).action) ) l
;;

let wakeup_all s input_fds output_fds =
  wakeup s s.input_conditions  input_fds;
  wakeup s s.output_conditions output_fds
;;

let rec select ?(noblock=false) input_fds output_fds =
  let rec select'() =
    try
      let (inp, out, _) = Unix.select input_fds output_fds [] (if noblock then 0.0 else -1.0) 
      in   inp,out    
    with Unix.Unix_error(Unix.EINTR,_,_) -> select' ()
  in
    select' ()
;;


let poll ?(noblock=true) s =

  match select ~noblock:noblock (input_fd_list s) (output_fd_list s)
  with
      [], []      when noblock -> ()
    | l_in, l_out              -> wakeup_all s l_in l_out
;;


let wait s = poll  ~noblock:false s;;

let run_queue s =

  let run' p =

    match p () with
        Terminate_Task  -> ()
      | Continue_Task q -> Queue.add q s.queue
	  
  in

  for i = 1 to Queue.length s.queue do
    run' (Queue.take s.queue)
  done
;;


let run s =

  let work () =
    while processes_runnable s do
      run_queue s;
      poll s;
    done
  in
    work ();
    while conditions_pending s do
      wait s;
      work ();
    done;
;;


(* ----------------------------------------------------------------------------------------

   I/O helper functions 
   --------------------
*)


type 'a data =  Data of 'a | Data_not_ready | Data_end;; (* actual i/o will use string data *)

let read_to_str b fd =

  let rec read () =

    try
      let  max   = (String.length b)     in
      let  count = Unix.read fd b 0 max  in
        if count = 0 then
          Data_end
        else
          Data (if count = max then b else String.sub b 0 count)
    with
        Unix.Unix_error (Unix.EAGAIN, _, _) -> Data_not_ready
      | Unix.Unix_error (Unix.EINTR, _, _)  -> read ()
      | x -> raise x
  in
    read ()
;;



let read_line ch =
  try 
    Data (input_line ch)
  with 
      End_of_file    -> Data_end
    | Sys_blocked_io -> Data_not_ready
;;

type write_result = Complete | Bytes_left of int;;

let write_from_str fd buf count = 
  
  let rec loop count =
    try 

      let len   = (String.length buf) in
      let count = len - (Unix.write fd buf (len - count) count) 
      in
	if   count=0 
	then Complete
	else loop count                                         (* tail rec? *)

    with Unix.Unix_error(Unix.EAGAIN,_,_) -> Bytes_left count
      
  in
    loop count
;;


(* ------------------------------------------------------------------------------------------- 

   SIGPIPE handling / supression
   -----------------------------
*)


Sys.set_signal Sys.sigpipe Sys.Signal_ignore;;



(* ------------------------------------------------------------------------------------------- 

   Task creation procedures
   ------------------------   
*)


type ('acc,'data) data_sink_operation    = 'acc -> 'data -> 'acc
type 'acc         data_sink_finalization = 'acc -> unit

type ('acc,'data) data_source_get        = 'acc -> ('data * 'acc) option
type 'acc         data_source_cleanup    = 'acc option -> unit


module Create = struct

  let copy_raw_to_sink
      s 
      (operation : ('acc, string) data_sink_operation)  ?(finally : 'acc data_sink_finalization = fun x -> ()) 
      (initial:'acc)
      fd 
      
      =

    Unix.set_nonblock fd;
    
    let buffer = String.make 512 ' '
    in
      
    let rec schedule acc =  add_input s { fd = fd; action = (copy' acc) }

    and finalize' acc () = (finally acc)
      
    and copy' acc () : process_continuation = 
      
      remove_input s fd;
      
      match (read_to_str buffer fd) with
	  
          Data d         -> (Continue_Task (copy' (operation acc d)))
	| Data_end       -> finalize' acc () ; Terminate_Task
	| Data_not_ready -> schedule acc     ; Terminate_Task
	    
    in
      schedule initial


  let copy_to_stdout s fd = 
    copy_raw_to_sink s (fun acc s -> print_string s; acc) () fd
  

  let copy_lines_to_sink

      s 
      (operation : ('acc,'data) data_sink_operation)  (finally : 'acc data_sink_finalization) 
      (initial:'acc)
      ch

      =  
    
    let fd = Unix.descr_of_in_channel ch
      
    in
      
      Unix.set_nonblock fd;
	
      let rec schedule acc =  add_input s { fd = fd; action = (copy' acc) }
	
      and finalize' acc () = (finally acc)
	
      and copy' acc () : process_continuation = 
	
	remove_input s fd;
	
	match (read_line ch) with
	    
            Data d         -> (Continue_Task (copy' (operation acc d)))
	  | Data_end       -> finalize' acc () ; Terminate_Task
	  | Data_not_ready -> schedule acc      ; Terminate_Task
	      
      in
	schedule initial

  let copy_lines_to_stdout s ch = 
    copy_lines_to_sink s (fun acc s -> print_string s; print_newline (); acc) (fun () -> ()) () ch

  let read_textfile s (var:string list ref) ch = 
    copy_lines_to_sink s 
      (fun acc s -> s::acc) 
      (fun acc   -> var := (List.rev acc)) 
      [] ch


  let feed_from_source 
      s 
      (get:('state,string) data_source_get) 
      ?(cleanup : 'acc data_sink_finalization = fun x -> ())  
      initial 
      ?(auto_close=true) ?(ignore_broken_pipe=false) 
      fd =

    Unix.set_nonblock fd;
    
    let rec schedule state buf count =  add_output s { fd = fd; action = (copy' state buf count) }   
      
    and finalize rest_state =
      cleanup rest_state; if auto_close then Unix.close fd  
	
    and next_chunk state = 
      
      match get state with
	  None            -> finalize None
	| Some (s, state) -> schedule state s (String.length s)
	    
    and copy' state buf count () : process_continuation = 
      
      remove_output s fd;
      
      try 
	
	match write_from_str fd buf count 
	with 
	    Complete     -> next_chunk state     ; Terminate_Task
	  | Bytes_left n -> schedule state buf count; Terminate_Task
	      
      with Unix.Unix_error (Unix.EPIPE, _, _) as x 
	  -> if ignore_broken_pipe then (finalize (Some state); Terminate_Task) else raise x
    in
      next_chunk initial


  let feed_list s l 
      ?(auto_close=true) 
      ?(ignore_broken_pipe=false) 
      ?(cleanup : 'acc data_sink_finalization = fun x -> ())   fd 

      =

    feed_from_source ~auto_close:auto_close ~ignore_broken_pipe:ignore_broken_pipe ~cleanup:cleanup s 
      (fun l -> 
       
	 match l with 
	     []   , _     -> None
	   | l    , true  -> Some ("\n", (l,false))   (* flag indicates pending EOL *)
	   | h::t , false -> Some (h, (t,true))
      )
      (l,false)
      fd
end
