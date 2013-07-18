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




(** A module to convienently handle creation and control of child processes. 
    This module provides ...
*)

type t  (** 
	    Proxy ...
	*)

type status =   (** abcbabcabc *)
    Running
  | Stopped of int
  | Terminated of Unix.process_status
  | Released    (**
		    aadad
		*)


exception Usage_error of string

  (**
     [Usage_error] is raised with a descriptive error message if ...      
  *)


  


val with_blocked_sigchld : (unit -> 'a) -> 'a
exception Nesting_error


val get_stdin_channel  : t -> out_channel
val get_stdout_channel : t -> in_channel
val get_stderr_channel : t -> in_channel

val get_stdin  : t -> Unix.file_descr
val get_stdout : t -> Unix.file_descr
val get_stderr : t -> Unix.file_descr

(** {2 XXX: sadasd sdklaskd sdlkasjld asasd } 

    sasöd asöldaöld
*)

val reposses_stdin  : t -> Unix.file_descr
val reposses_stdout : t -> Unix.file_descr
val reposses_stderr : t -> Unix.file_descr

val reposses_stdin_channel  : t -> out_channel
val reposses_stdout_channel : t -> in_channel
val reposses_stderr_channel : t -> in_channel

val get_fd_stdin  : t -> Unix.file_descr
val get_fd_stdout : t -> Unix.file_descr
val get_fd_stderr : t -> Unix.file_descr

val release_pipes         : t -> unit
val release               : t -> unit


exception No_sig_setup

val wait : ?release:bool -> t -> Unix.process_status

type redirection

module Redirect :
  sig
    val not       : redirection
    val from_file :
      ?flags:Unix.open_flag list -> string -> redirection
    val to_file :
      ?mode:Unix.file_perm ->
      ?flags:Unix.open_flag list -> string  -> redirection
    val to_fd : Unix.file_descr -> redirection
    val to_new_pipe   : redirection
    val from_new_pipe : redirection
  end


module Privileges :
  sig
    val drop_group : unit -> unit
    val drop_user : unit -> unit
    val escalate_group : unit -> unit
    val escalate_user : unit -> unit
    val drop : unit -> unit
    val keep : unit -> unit
    val escalate : unit -> unit
  end

val create :
  ?more_setup:(unit -> unit) list ->
  ?more_cleanup:(unit -> unit) list ->
  ?privileges:(unit -> unit) ->
  ?stdin: redirection -> ?stdout:redirection -> ?stderr:redirection ->
  ?convert_terminated:bool ->
  ?convert_stopped:bool -> ?release:bool -> string -> string list -> t
