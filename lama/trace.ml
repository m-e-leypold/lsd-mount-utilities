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





let tracing = (try (Sys.getenv "lsd_trace_lama")="verbose" with Not_found -> false | x -> raise x);;

let is_active () = tracing;;

let print scope_name event_name message =    

  prerr_string "TRACE\t";
  prerr_string scope_name;
  prerr_string ".";
  prerr_string event_name;
  (
    match message with 
	None   -> ()
      | Some s -> 
	  prerr_string  "\t";
	  prerr_string  s
  );
  prerr_newline ();
  flush stderr
;;


class type tracer =
  object
    method event : string -> unit
    method int : string -> int -> unit
    method msg : string -> string option -> unit
    method string : string -> string -> unit
    method string_list : string -> string list -> unit
  end
;;

class null_tracer : tracer = 
object

  method msg    ev  msg = ()
  method event  ev      = ()
  method string var s   = ()
  method int    var i   = () 

  method string_list var (l:string list) = ()
end;;


let quote s = "\""^s^"\"";;


class module_tracer module_name = 
object

  val scope_name = "MODULE:" ^ module_name 

  method msg    ev  msg = print scope_name ev  msg
  method event  ev      = print scope_name ev  None
  method string var s   = print scope_name var (Some (quote s))
  method int    var i   = print scope_name var (Some (string_of_int i))

  method 
    string_list var (l:string list) =

    let rec msg_of_list s l =
      
      match l with 
	  []   -> s ^ "]"
	| h::t -> msg_of_list (s ^ "; " ^ (quote h)) t

    in      
      
      print scope_name var (Some
			      (match l with
				   []   -> "[]"
				 | h::t -> msg_of_list ("[" ^ (quote h)) t))
	      

(* XXX improve fromatting here
   XXX general list tracer / printer 
   XXX some of this is NOT cheap
*)
    
end;;

let for_module name : tracer = 
  if is_active() 
  then (new module_tracer name)
  else (new null_tracer)
;;

    




