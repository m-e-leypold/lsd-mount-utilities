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
  

  mountlib.ml -- bindings to mount, losetup, cryptsetup.

*)



(* ------------------------------------------------------ *)

let path_to_dmname p =
  ".LSD.lcrypt." ^ (Str.global_replace  (Str.regexp "//*") "#" p)
;;


(* ------------------------------------------------------ *)


module Loop = struct
   
  exception Probing_failed   ;;       
  exception Binding_failed   of string  (* device path     *) ;;       
  exception Unbinding_failed of string  (* device path     *) ;;  


  let delete ~loop_device =
    try
      Subprocess.run  "/sbin/losetup" [ "-d" ; loop_device ] 
    with 
	Subprocess.Failed _ -> raise (Unbinding_failed loop_device)
      | x -> raise x
      
      
  let setup ~loop_device ~file =
    try
      Subprocess.run  "/sbin/losetup" [ loop_device ; file ] 
    with 
	Subprocess.Failed _ -> raise (Binding_failed loop_device)
      | x -> raise x
      

  let probe ~file =
    
    ( try

	Subprocess.run "/sbin/modprobe" ["loop"]

      with Subprocess.Failed _ -> () | x -> raise x 
      
    (* Ignore errors with modprobe. Either the loop module is statically in the kernel,
       or it has just been loaded or the loading failed. It will turn out in the next step
       which is the case: We'll see, wether probing succeeds or fails and do not
       distinguish between those cases here. *)
    );
    
    let rec loop n =
      if n > 7 then raise Probing_failed
      else
	begin
	  let path = "/dev/loop" ^ (string_of_int n)
	  in 
	    try
	      setup path file ;
	      path
	    with 
	      Binding_failed _ -> (loop (n+1))
	      | x                   -> raise x
	end
    in
      loop 0


  let clean () =

    let rec loop n =
      if n > 7 then ()
      else
	begin
	  let path = "/dev/loop" ^ (string_of_int n)
	  in 
	    try
	      delete path;
	      loop (n+1)
	    with 
		Unbinding_failed _    -> (loop (n+1))
	      | x                     -> raise x
	end
    in
      loop 0
	
end
;;



(* ------------------------------------------------------ *)

module Mount = struct

  let attach ~fstype ~device ~options ~mount_point =
    Subprocess.run "/bin/mount" [ "-t" ; fstype ; device ; "-o" ; options ; mount_point ]

  let attach_hidden ~fstype ~device ~options ~mount_point =
    Subprocess.run "/bin/mount" [ "-n" ; "-t" ; fstype ; device ; "-o" ; options ; mount_point ]

  let detach ~mount_point =
    Subprocess.run "/bin/umount" [ mount_point ]

  let detach_really ~mount_point =
    Subprocess.run "/bin/umount" [ "-i" ; mount_point ]
      
  let add_entry ~fstype ~device ~options ~mount_point =
    Subprocess.run "/bin/mount" [ "-i" ; "-f"; "-t" ; fstype ; device ; "-o" ; options ; mount_point ]

  module Compat = struct

    let mount           = attach
    let mount_n         = attach_hidden
    let mount_fi        = add_entry
    let umount          = detach
    let umount_n        = detach_really
  end
end;;


exception Mtab_format_error;;

type fs_type      =  string;;
type fs_options   =  string;;

type ('path_t,'fs_t,'opt_t) mount_spec   =  

    { mount_point : 'path_t ; mount_type : 'fs_t ; mount_options : 'opt_t ; device : 'path_t option };;


let parse_mtab_line l =

  let drop_parens s =

    let  l = String.length s  in
      if String.get s 0     != '(' then raise Mtab_format_error;
      if String.get s (l-1) != ')' then raise Mtab_format_error;

      try
	(String.sub s 1 (l-2))
      with _ -> raise Mtab_format_error	
  in
  
    ( match l with 
  
	  dev :: "on" :: dir :: "type" :: fstype :: options :: []  when (String.get dev 0) = '/' -> 
	    { mount_point = dir; mount_type = fstype; mount_options = (drop_parens options); device = Some dev ; } 
	      
	| dev :: "on" :: dir :: "type" :: fstype :: options :: [] ->
	    { mount_point = dir; mount_type = fstype; mount_options = (drop_parens options); device = None ; }
	| _                -> raise Mtab_format_error
    )
;;



type 'option_key option_table = ('option_key, string option) Hashtbl.t ;;

let option_table_add table key value =
  Hashtbl.add table key (Some value)
;;


let option_table_get table key =
  match Hashtbl.find table key with
      None   -> raise Not_found
    | Some x -> x
;;


let option_table_set table key =
  Hashtbl.add table key None
;;


let option_table_to_string table =

  let append_option_string s o     = if String.length s = 0 then o else s ^ "," ^ o 
  and option_to_string key value   = match value with None -> key | Some x -> key ^ "=" ^ x  
  in
  let add_option key value s       = append_option_string s (option_to_string key value)
  in    
    Hashtbl.fold add_option table ""
;;



let parse_mount_options option_string =

  let table = Hashtbl.create 10   in
  let add   = Hashtbl.add    table

  in

  let l = (Str.split (Str.regexp "[,]+") option_string)
  in 
  let rec loop = function 
      
      h::t -> 
	
	( let l = String.length h in 		   
	    try  let i = String.index h '=' in add (String.sub h 0 i) (Some (String.sub h (i+1) (l-i-1)))
	    with Not_found -> add h None
	);
	loop t
	  
    | []   -> table
	
	
  in loop l
;;

let mtab_line_to_entry l =
  parse_mtab_line (Str.split (Str.regexp "[ \t]+") l)
;;

let get_mtab () =
  List.map mtab_line_to_entry (Subprocess.run_catch_stdout "/bin/mount" [])
;;


let get_mtab_entry dir = 
  
  let rec loop l =
    
    match l with
	[]                           -> raise Not_found
      | h::_  when h.mount_point=dir -> h
      | _ ::t                        -> loop t
  in
  let e = loop (get_mtab ())
  in  {e with mount_options = (parse_mount_options e.mount_options)}
;;


(* option_table_maybe_get *)
(* let option_table_get tbl ... -> string option *)
(* option_table_test *)
(* mount_options_get, mount_options_test <- test/get on entry *)





(* ------------------------------------------------------ *)


module Crypto_device = struct

  let create ~encrypted_device ~decrypted_device =
    Subprocess.run "/sbin/cryptsetup" [ "create" ; "-c" ; "aes" ; decrypted_device ; encrypted_device ]

  (* note: failure here might be due to too short container file *)

  let remove ~decrypted_device =
    Subprocess.run "/sbin/cryptsetup" [ "remove" ; decrypted_device ]
end
;;


(* ------------------------------------------------------ *)




