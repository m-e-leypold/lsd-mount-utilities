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





open Unix;;

exception Usage_error of string;;
exception Internal_error of string;;

type status       =  Running | Stopped of int | Terminated of process_status | Released;;
                     
type signal_queue =  
    {
      input  : file_descr;
      output : file_descr;
    }
;;


type io_connection =
    
    From_pipe_fd  of file_descr  (* fd given is the input end of the pipe *)
  | To_pipe_fd    of file_descr 
  | From_pipe_ch  of out_channel
  | To_pipe_ch    of in_channel
  | Named_file of string
  | Anon_file of file_descr
  | Unknown

(* maybe add: Remote_Server of ip * port, Remote_Client ... even Socket of ..., also Temp_File of ... *)
;;


type t =
    {
      command      : string;
      args         : string list;

      pid            : int;
      mutable status : status;

      mutable stdin  : io_connection;
      mutable stdout : io_connection;
      mutable stderr : io_connection;

      mutable terminated : signal_queue option;
      mutable stopped    : signal_queue option;  (* XXX not handled yet *)
    }
;;


(* ----------------------------------------------------------------------------------- *)

let children : (int, t) Hashtbl.t = Hashtbl.create 10 ;;


let wait_pid ?(nohang=false) pid  =

  let flags = if nohang then [WNOHANG] else []
  in  
  let (pid,status) = 
    
    let rec wait' () = try (waitpid flags pid) with Unix_error (EINTR,_,_) -> wait' () | x -> raise x 
    in
      wait' ()

  in
    if pid !=0 then begin

      ( try
          let sp = (Hashtbl.find children pid)
          in
            sp.status <- Terminated status;
            ( match sp.terminated with
                  None    -> ()
                | Some q  -> ignore (write q.input "X" 0 1)
            );
            Hashtbl.remove children pid
        with Not_found -> ());
    end
    ;            
    pid
;;

let rec reap_children () =

  begin
    try

      let rec reap_more () =

        let pid = wait_pid ~nohang:true (-1)         (* XXX should also handle stopping / restarting *)
        in
          if pid !=0 then begin
            reap_more ()
          end
      in
        reap_more ()

    with Unix_error(ECHILD,_,_) -> ()
  end
;;


Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun x -> reap_children ())) 

  (* XXX consider saving and chaining original handler *)

;;


let sigchld_block_count = ref 0;;
exception Nesting_error;;

let block_sigchld () =
  ignore (sigprocmask SIG_BLOCK   [Sys.sigchld]);
  sigchld_block_count := !sigchld_block_count+1
;;

let unblock_sigchld () =
  if !sigchld_block_count = 0 then raise Nesting_error;
  sigchld_block_count := !sigchld_block_count-1;
  if !sigchld_block_count = 0 then ignore (sigprocmask SIG_UNBLOCK [Sys.sigchld])
;;


let with_blocked_sigchld proc =
  block_sigchld ();
  try  ( let r = proc () in unblock_sigchld (); r )
  with x -> unblock_sigchld (); raise x 
;;

(* ---------------------------------------------------------------------- *)

let out_channel_from_connection c =
  begin
    match c with
	From_pipe_fd  fd | Anon_file fd -> From_pipe_ch (Unix.out_channel_of_descr fd)
      | From_pipe_ch  ch                -> c
      | _                               -> raise (Usage_error "Can't convert io_connection to channel")
  end   
;;

let in_channel_from_connection c =
  begin
    match c with
	To_pipe_fd  fd | Anon_file fd -> To_pipe_ch (Unix.in_channel_of_descr fd)
      | To_pipe_ch  ch                -> c
      | _                             -> raise (Usage_error "Can't convert io_connection to channel")
  end   
;;

let out_channel_of = 
  function From_pipe_ch ch -> ch | _ -> raise (Usage_error "Can't extract channel from io_connection");;

let in_channel_of = 
  function To_pipe_ch ch -> ch | _ -> raise (Usage_error "Can't extract channel from io_connection");;

let get_stdin_channel p = 
  p.stdin <- out_channel_from_connection p.stdin ; out_channel_of p.stdin
;;

let get_stdout_channel p = 
  p.stdout <- in_channel_from_connection p.stdout ; in_channel_of p.stdout
;;

let get_stderr_channel p = 
  p.stdout <- in_channel_from_connection p.stderr ; in_channel_of p.stderr
;;


let get_stdin p = 
  match p.stdin with From_pipe_fd fd -> fd | _ -> raise (Usage_error "stdin is not a file descriptor")
;;

let get_stdout p = 
  match p.stdout with To_pipe_fd fd -> fd | _ -> raise (Usage_error "stdout is not a file descriptor")
;;

let get_stderr p = 
  match p.stderr with To_pipe_fd fd -> fd | _ -> raise (Usage_error "stderr is not a file descriptor")
;;


let reposses_stdin p = 
  let fd = get_stdin p in p.stdin <- Unknown; fd
;;

let reposses_stdout p = 
  let fd = get_stdout p in p.stdout <- Unknown; fd
;;

let reposses_stderr p = 
  let fd = get_stderr p in p.stderr <- Unknown; fd
;;

let reposses_stdin_channel p = 
  let ch = get_stdin_channel p in p.stdin <- Unknown; ch
;;

let reposses_stdout_channel p = 
  let ch = get_stdout_channel p in p.stdout <- Unknown; ch
;;

let reposses_stderr_channel p = 
  let ch = get_stdout_channel p in p.stdout <- Unknown; ch
;;


let fd_from_connection c =

  match c with
      From_pipe_fd  fd | Anon_file fd | To_pipe_fd fd  ->  fd
    | From_pipe_ch  ch ->  (Unix.descr_of_out_channel  ch)
    | To_pipe_ch ch    ->  (Unix.descr_of_in_channel ch)
    | _                ->  raise (Usage_error "Can't convert io_connection to descriptor")
;;

let get_fd_stdin  p = fd_from_connection p.stdin  ;;
let get_fd_stdout p = fd_from_connection p.stdout ;;
let get_fd_stderr p = fd_from_connection p.stderr ;;


let release_io_connection c =
  begin
    match c with
	From_pipe_fd  fd -> Unix.close fd
      | To_pipe_fd    fd -> Unix.close fd
      | From_pipe_ch  ch -> close_out  ch
      | To_pipe_ch    ch -> close_in   ch
      | Anon_file     fd -> Unix.close fd
      | _                -> () 
  end   
;;


let release_signal_queue q =
  let release' () =
    match q with None -> () | Some q' -> (Unix.close q'.input); (Unix.close q'.output)
  in
    with_blocked_sigchld release'
;;



let release p =

  let release' () =

    p.status <- Released;
    
    release_io_connection p.stdin;  p.stdin  <- Unknown;
    release_io_connection p.stdout; p.stdout <- Unknown;
    release_io_connection p.stderr; p.stderr <- Unknown;

    release_signal_queue p.terminated; p.terminated <- None;
    release_signal_queue p.stopped;    p.stopped    <- None;
      
  in
    with_blocked_sigchld release'
;;

let process_release = release;;



let release_pipes p =

  let release' () =
    let pipe_cleaned c =
      
      match c with
	  From_pipe_fd  fd -> Unix.close fd; Unknown
	| To_pipe_fd    fd -> Unix.close fd; Unknown
	| From_pipe_ch  ch -> close_out  ch; Unknown
	| To_pipe_ch    ch -> close_in   ch; Unknown
	| _                -> c

    in
      
      p.stdin  <- pipe_cleaned p.stdin;
      p.stdout <- pipe_cleaned p.stdout;
      p.stderr <- pipe_cleaned p.stderr;
  in 
    with_blocked_sigchld release'
;;

let process_release_pipes = release_pipes;;

exception No_sig_setup;;

let rec soft_wait sp =

  if !sigchld_block_count != 0 then raise Nesting_error;

  ( match sp.terminated with
        None     -> raise  No_sig_setup
      | Some q  ->

          let rec select' () = 
	    try ignore (Unix.select [q.output] [] [] (-1.0)) with Unix_error(EINTR,_,_) -> select' () | x -> raise x
          in
            select' () 
  )
;;


let rec hard_wait sp =

  let hard_wait' () =
    match sp.status with
      (Terminated _) | Released -> ()
    | _                         -> ignore (wait_pid sp.pid)
  in 
    with_blocked_sigchld hard_wait'
;;


let wait ?(release=true) sp = 
  if release then (process_release_pipes sp);
  begin
    match sp.terminated with
        None    -> hard_wait sp
      | Some q  -> soft_wait sp
  end;

  let status =
    ( match sp.status with
	  Terminated st -> st
	| _            -> raise (Internal_error "Subprocess.wait: process not terminated (WTF?)")
    )
  in 
    if release then (process_release sp);

    status   
;;

(* ---------------------------------------------------------------------- *)


type redirection_info =
    {
      connection : io_connection;
      setup      : unit -> unit;
      cleanup    : unit -> unit;      
    }
;;

type redirection = Unix.file_descr -> redirection_info;;

module Redirect = struct

  let not target_fd = { connection = Unknown; setup = (fun () -> ()); cleanup = (fun () -> ()) }

  let from_file ?(flags=[O_RDONLY]) s target_fd =
    let fd = Unix.openfile s flags 0o600
    in       
      { connection = Named_file s;
	setup      = (fun () -> Unix.dup2 fd target_fd; Unix.close fd ); 
	cleanup    = (fun () -> Unix.close fd )
      }


  let to_file ?(mode=0o600) ?(flags=[O_CREAT;O_WRONLY]) s target_fd =
    let fd = Unix.openfile s flags mode
    in       
      { connection = Named_file s; 
	setup      = (fun () -> Unix.dup2 fd target_fd; Unix.close fd ); 
	cleanup    = (fun () -> Unix.close fd )
      }

  let to_fd fd target_fd =

    { connection = Anon_file fd; 
      setup      = (fun () -> Unix.dup2 fd target_fd; Unix.close fd ); 
      cleanup    = (fun () -> ())
    }


  let to_new_pipe target_fd =

    let pipe_out, pipe_in = Unix.pipe ()
    in 

      { connection = To_pipe_fd pipe_out; 
	setup      = (fun () -> Unix.dup2 pipe_in target_fd; Unix.close pipe_in; Unix.close pipe_out ); 
	cleanup    = (fun () -> Unix.close pipe_in )
      }

  let from_new_pipe target_fd =

    let pipe_out, pipe_in = Unix.pipe ()
    in 

      { connection = From_pipe_fd pipe_in; 
	setup      = (fun () -> Unix.dup2 pipe_out target_fd; Unix.close pipe_out; Unix.close pipe_in ); 
	cleanup    = (fun () -> Unix.close pipe_out )
      }
end
;;



module Privileges = struct

  let drop_priv get gete set () =
    
    let  p = get ()
    and ep = get ()
    in
      if p != ep then set p


  let escalate_priv get gete set () =
    
    let  p = get ()
    and ep = get ()
    in
      if p != ep then set ep

  let with_gid f = f getgid getegid setgid
  let with_uid f = f getuid geteuid setuid
  
  let drop_group     = with_gid drop_priv 
  let drop_user      = with_uid drop_priv 
  let escalate_group = with_gid escalate_priv 
  let escalate_user  = with_uid escalate_priv 

  let drop     () = drop_user (); drop_group ()
  let keep     () = ()
  let escalate () = escalate_user (); escalate_group ()
end
;;


(* --------------------------------------------------------------------------------- *)

let spawn0 ?setup ?cleanup command args  =

  let pid = fork()
  in
    if pid = 0
    then
      begin
	( match setup with None -> () | Some p -> p() );
	execv command  ( Array.of_list ( command :: args ));
      end
    else
      ( match cleanup with None -> () | Some p -> p() );
      pid
;;


let spawn 

    ?( setup   = Privileges.drop )
    ?( cleanup = fun () -> () )

    ?( stdin_conn  = Unknown )  (* maybe drop this from here <-> spawn vs spawn 0 *)
    ?( stdout_conn = Unknown ) 
    ?( stderr_conn = Unknown ) 

    ?( termination_ctrl = None )
    ?( stop_ctrl        = None )

    command 
    args 
    
    =

  let spawn' () =

    let pid = spawn0 ~setup:setup ~cleanup:cleanup command args 
    in
    let process =
      
      { 
	pid     = pid ;
	command = command;
	args    = args;
	
        stdin   = stdin_conn;
        stdout  = stdout_conn;
        stderr  = stderr_conn;
	
        terminated = termination_ctrl;
        stopped    = stop_ctrl;

        status     = Running;
      }
    in
      Hashtbl.add children process.pid process;
      process
  in
    with_blocked_sigchld spawn'
;;


let create 

    ?(more_setup   = [])
    ?(more_cleanup = [])

    ?(privileges   = Privileges.drop)

    ?(stdin  = Redirect.not)
    ?(stdout = Redirect.not)
    ?(stderr = Redirect.not)

    ?( convert_terminated = false ) 
    ?( convert_stopped    = false )

    ?( release = false )

    command args =

  let s_in  = stdin  Unix.stdin 
  and s_out = stdout Unix.stdout
  and s_err = stderr Unix.stderr

  and optional_signal_conversion flag = 
    if flag then 
      let 
	  pipe_out, pipe_in = Unix.pipe ()
      in
	(Some { input=pipe_in; output=pipe_out }) 
    else 
      None

  in

  let p = 

    spawn 
      
      ~setup: ( fun () -> 
		  privileges  ();
		  s_in.setup  ();
		  s_out.setup ();
		  s_err.setup ();
		  List.iter(fun f -> f ()) more_setup;
	      )

      ~stdin_conn:  (s_in.connection)
      ~stdout_conn: (s_out.connection)
      ~stderr_conn: (s_err.connection)

      ~cleanup: ( fun () -> 
		    s_in.cleanup  ();
		    s_out.cleanup ();
		    s_err.cleanup ();
		    List.iter(fun f -> f ()) more_cleanup;
		)

      ~termination_ctrl: (optional_signal_conversion convert_terminated)
      ~stop_ctrl:        (optional_signal_conversion convert_stopped)

      command args
  in
    if release then process_release p;
    p
;;

