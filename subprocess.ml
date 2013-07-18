(*

  lsd-mount-utilities -- Utilities to mount encrypted filesystems from /etc/fstab

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
  

  subprocess.ml -- facilities to bind external utilities as ocaml procedures.

*)


exception Failed of string  (* executable path *) ;;
exception Error  of string  (* executable path *) ;;


let run program_path argv =

  let pid = 
    Unix.create_process 
      program_path ( Array.of_list ( program_path :: argv  ))
      Unix.stdin Unix.stdout Unix.stderr

  in 
    match Unix.waitpid [] pid with
	_ , Unix.WEXITED 0 -> ()
      |	_ , Unix.WEXITED _ -> raise (Failed program_path)
      | _                      -> raise (Error  program_path)

and run_catch_stdout program_path argv =

  let  (pipe_read, pipe_write) = Unix.pipe ()          in
  let  return_ch = Unix.in_channel_of_descr pipe_read  in

  let pid = 

    Unix.set_close_on_exec pipe_read;

    Unix.create_process 
      program_path ( Array.of_list ( program_path :: argv  ))
      Unix.stdin pipe_write Unix.stderr
  in 

    Unix.close pipe_write;  (* ensure EOF at reading end *)

    let rec loop lines_so_far =
      
      match 	
	( try 
	    Some (input_line return_ch)
	  with
	      End_of_file ->  None 
	)
      with
	  Some line -> loop (line::lines_so_far)
	| None      -> lines_so_far

    in List.rev (loop [])
;;



(* reading from pipe hangs if child is not reaped (no sigpipe AFAIS)
   check: is this usual unix behaviour? 
   whatever: polling is the savest strategy here.

   a general subprocess / coprocess andling facility would perhaps not
   be wrong (and this should run quasi concurrently, but I'd have to
   check interaction with channel i/o etc.

   a problem remains if the subprocess just dies and leaves us with an
   incomplete line: So this here is not a general approach but a
   useful one.

   for a general approach we would have to recover the original unix
   syscall API flexibility and perhaps write our own buffered input
   library (or not, that depends what we find in channel and how
   signal handling can be integrated.
*)
