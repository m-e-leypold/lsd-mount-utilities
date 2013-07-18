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





let tr = Trace.for_module "shell_procedure";;

tr#event "INIT";;

type redirection =
    {
      redirect   : Subprocess.redirection ;
      task_setup : Io_task.system -> Subprocess.t -> unit
    }

module Redirect = struct

  let inherit_from_parent = 
    {
      redirect   =  Subprocess.Redirect.not;
      task_setup =  (fun s p -> ());    
    }

  let to_null =
    {
      redirect   =  Subprocess.Redirect.to_file "/dev/null";
      task_setup =  (fun s p -> ());    
    }

  let to_file f =
    {
      redirect   =  Subprocess.Redirect.to_file f;
      task_setup =  (fun s p -> ());    
    }

  let from_null =
    {
      redirect   =  Subprocess.Redirect.from_file "/dev/null";
      task_setup =  (fun s p -> ());    
    } 

  let from_file f =
    {
      redirect   =  Subprocess.Redirect.from_file f;
      task_setup =  (fun s p -> ());    
    } 

  let from_list l =
    {
      redirect   =  Subprocess.Redirect.from_new_pipe;
      task_setup =  (fun s p -> 
		       let fd = Subprocess.reposses_stdin p in
			 Io_task.Create.feed_list s l fd)   
    } 


  let to_list l_ref =
    {
      redirect   =  Subprocess.Redirect.to_new_pipe;
      task_setup =  (fun s p -> 
		       let ch = Subprocess.reposses_stdout_channel p in
			 Io_task.Create.read_textfile s l_ref ch)
    }

  let when_trace = if Trace.is_active () then inherit_from_parent else to_null

    (* XXX should better be to_parent_error => because we'd also like to 
       change stdout in certain circumstance ... -- usage() should really go to error
    *)


end

module Privileges = struct
  let drop     = Subprocess.Privileges.drop
  let keep     = Subprocess.Privileges.keep
  let escalate = Subprocess.Privileges.escalate
end;;




let run 
    
    ?(stdin=Redirect.from_null)  ?(stdout=Redirect.to_null) ?(stderr=Redirect.inherit_from_parent) 
    ?(privileges=Privileges.drop)
    command args

    =

  (tr#string_list "run" (command::args));

  let  p =  ( Subprocess.create 
		~stdin:stdin.redirect ~stdout:stdout.redirect ~stderr:stderr.redirect 
		~privileges:privileges
		command args  )
  in
  let  s = Io_task.new_system ()  in
    stdin.task_setup  s p ;
    stdout.task_setup s p ;
    stderr.task_setup s p ;
    Io_task.run s;
    Subprocess.wait p
;;


exception Error   of int * string * string list;;
exception Signal  of int * string * string list;;
exception Stopped of int * string * string list;;


let get_output 

    ?(stdin=Redirect.from_null)  ?(stderr=Redirect.inherit_from_parent) 
    ?(privileges=Privileges.drop)
    command args

    =

  let l = ref [] in

    match ( run 
	      ~stdin:stdin ~stderr:stderr ~stdout:(Redirect.to_list l) 
	      ~privileges:privileges 
	      command args )
    with

	Unix.WEXITED   0 ->  !l
      | Unix.WEXITED   n ->  raise (Error   (n,command,args))
      | Unix.WSIGNALED n ->  raise (Signal  (n,command,args))
      | Unix.WSTOPPED  n ->  raise (Stopped (n,command,args))	  
;;


let get_status 
    
    ?(stdin=Redirect.from_null) ?(stdout=Redirect.to_null) ?(stderr=Redirect.inherit_from_parent) 
    ?(privileges=Privileges.drop)   
    command args

    =

  match ( run 
	    ~stdin:stdin ~stderr:stderr ~stdout:stdout 
	    ~privileges:privileges 
	    command args )
  with
      
      Unix.WEXITED   0 ->  true
    | Unix.WEXITED   n ->  false
    | Unix.WSIGNALED n ->  raise (Signal  (n,command,args))
    | Unix.WSTOPPED  n ->  raise (Stopped (n,command,args))	  
;;



let execute
    
    ?(stdin=Redirect.from_null)  ?(stdout=Redirect.to_null) ?(stderr=Redirect.inherit_from_parent) 
    ?(privileges=Privileges.drop)
    command args

    =

  match ( run 
	    ~stdin:stdin ~stderr:stderr ~stdout:stdout 
	    ~privileges:privileges 
	    command args )
  with
      
      Unix.WEXITED   0 ->  ()
    | Unix.WEXITED   n ->  raise (Error   (n,command,args))
    | Unix.WSIGNALED n ->  raise (Signal  (n,command,args))
    | Unix.WSTOPPED  n ->  raise (Stopped (n,command,args))	  
;;
