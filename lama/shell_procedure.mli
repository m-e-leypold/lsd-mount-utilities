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





(* Shell_procedures *)(** easily invoke external programs and get their output.
      
   ...
*)

type redirection (** type of a redirection operator *)

val run :
  ?stdin:redirection ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  ?privileges:(unit -> unit) -> string -> string list -> Unix.process_status

val execute :
  ?stdin:redirection ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  ?privileges:(unit -> unit) -> string -> string list -> unit

val get_output :
  ?stdin:redirection ->
  ?stderr:redirection ->
  ?privileges:(unit -> unit) -> string -> string list -> string list


val get_status :
  ?stdin:redirection ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  ?privileges:(unit -> unit) -> string -> string list -> bool


exception Error of int * string * string list
exception Signal of int * string * string list
exception Stopped of int * string * string list


(** Operators to redirect input and output
    ...
                                                                     *)
module Redirect : 
sig  
  val inherit_from_parent : redirection
  val to_null    : redirection
  val when_trace : redirection

  val to_file : string -> redirection
  val from_null : redirection
  val from_file : string -> redirection
  val from_list : string list -> redirection
  val to_list   : string list ref -> redirection

end

(** {4 See manual page Shell_procedure.Redirect(3o) for reference.} *)

(** See manual page Shell_procedure.Redirect(3o) for reference. *)



(** Operators to drop and escalate privileges
    ...
                                                                     *)
module Privileges : 
sig
  val drop     : unit -> unit
  val keep     : unit -> unit
  val escalate : unit -> unit
end

