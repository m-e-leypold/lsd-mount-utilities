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

   
exception Loop_probing_failed   ;;       
exception Loop_binding_failed   of string  (* device path     *) ;;       
exception Loop_unbinding_failed of string  (* device path     *) ;;  


let losetup_delete ~loop_device =
  Subprocess.run  "/sbin/losetup" [ "-d" ; loop_device ] 
;; 

let losetup ~loop_device ~file =
  Subprocess.run  "/sbin/losetup" [ loop_device ; file ] 
;;


let loprobe ~file =
  
  Subprocess.run "/sbin/modprobe" ["loop"];

  let rec loop n =
    if n > 7 then raise Loop_probing_failed
    else
      begin
	let path = "/dev/loop" ^ (string_of_int n)
	in 
	  try
	    losetup path file ;
	    path
	  with 
	      Subprocess.Failed _ -> (loop (n+1))
	    | x                   -> raise x
      end
  in
    loop 0
;;



let loclean () =

  let rec loop n =
    if n > 7 then ()
    else
      begin
	let path = "/dev/loop" ^ (string_of_int n)
	in 
	  try
	    losetup_delete path;
	    loop (n+1)
	  with 
	      Subprocess.Failed _   -> (loop (n+1))
	    | x                     -> raise x
      end
  in
    loop 0
;;


let path_to_dmname p =
  ".dmcrypt2." ^ (Str.global_replace  (Str.regexp "//*") "#" p)
;;


let crypto_map_device ~loop_device ~mapper_device =
  Subprocess.run "/sbin/cryptsetup" [ "create" ; "-c" ; "aes" ; mapper_device ; loop_device ]
;;


let crypto_unmap_device ~mapper_device =
  Subprocess.run "/sbin/cryptsetup" [ "remove" ; mapper_device ]
;;

(* note: failure here might be due to too short container file *)

let mtab_add ~fstype ~device ~options ~mount_point =
  Subprocess.run "/bin/mount" [ "-i" ; "-f"; "-t" ; fstype ; device ; "-o" ; options ; mount_point ]
;;

let mtab_add ~fstype ~device ~options ~mount_point =
  Subprocess.run "/bin/mount" [ "-i" ; "-f"; "-t" ; fstype ; device ; "-o" ; options ; mount_point ]
;;

let really_mount ~fstype ~device ~options ~mount_point =
  Subprocess.run "/bin/mount" [ "-n" ; "-t" ; fstype ; device ; "-o" ; options ; mount_point ]
;;

let really_umount ~mount_point =
  Subprocess.run "/bin/umount" [ "-i" ; mount_point ]
;;

(* ------------------------------------------------------ *)


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
