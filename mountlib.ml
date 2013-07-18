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


open Shell_procedure.Privileges;;
open Shell_procedure.Redirect;;

(* ------------------------------------------------------ *)


module Loop = struct
   
  exception Probing_failed   ;;       
  exception Binding_failed   of string  (* device path     *) ;;       
  exception Unbinding_failed of string  (* device path     *) ;;  


  let delete ~loop_device =
    if    not (Shell_procedure.get_status  "/sbin/losetup" [ "-d" ; loop_device ])
    then  raise (Unbinding_failed loop_device)
      
  let setup ~loop_device ~file =
    if    not(Shell_procedure.get_status  "/sbin/losetup" [ loop_device ; file ] )
    then  raise (Binding_failed loop_device)

  let probe ~file =
    
    ( match Shell_procedure.run "/sbin/modprobe" ["loop"]
      with  _ -> ()
      
      (* Ignore errors with modprobe. Either the loop module is
         statically in the kernel, or it has just been loaded or the
         loading failed. It will turn out in the next step which is
         the case: We'll see, wether probing succeeds or fails and do
         not distinguish between those cases here. 

         This is crude and should be changed some time in the future.

      *)
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





module Filesystem = struct

  let mount ~fstype ~device ~options ~mount_point =
    Shell_procedure.execute 
      ~privileges:keep 
      "/bin/mount" [ "-t" ; fstype ; device ; "-o" ; options ; mount_point ]

  let mount_hidden ~fstype ~device ~options ~mount_point =
    Shell_procedure.execute 
      ~privileges:escalate 
      "/bin/mount" [ "-n" ; "-t" ; fstype ; device ; "-o" ; options ; mount_point ]

  let umount ~mount_point =
    Shell_procedure.execute 
      ~privileges:keep 
      "/bin/umount" [ mount_point ]

  let umount_really ~mount_point =
    Shell_procedure.execute 
      ~privileges:keep 
      "/bin/umount" [ "-i" ; mount_point ]

  let mkfs ?blocksize ~device =
    Shell_procedure.execute 
      "/sbin/mkfs"  
      ([ "-t" ; "ext2" ; device ] @ (match blocksize with None -> [] | Some x -> [string_of_int x]))
      
  let fsck ~device =
    Shell_procedure.execute "/sbin/fsck"   [ "-t" ; "ext2" ; device ]


  module Options = struct

    type ('key,'value) table = ('key, 'value option) Hashtbl.t

    let has_key table key = Hashtbl.mem table key

    let rec remove table key = if has_key table key then (Hashtbl.remove table key; remove table key) else ()      
	
    let add table key value =
      Hashtbl.add table key (Some value)
	
    let get table key =
      match Hashtbl.find table key with
	  None   -> raise Not_found
	| Some x -> x
	    
    let set table key = remove table key; Hashtbl.add table key None

    let is_set table key = 
      try
	match Hashtbl.find table key with
	    None   -> true
	  | Some x -> false
      with Not_found -> false | x -> raise x

	
    (* option_table_maybe_get *)
    (* let option_table_get tbl ... -> string option *)
    (* option_table_test *)
    (* mount_options_get, mount_options_test <- test/get on entry *)      
	
    let table_to_string table =
      
      let append_option_string s o     = if String.length s = 0 then o else s ^ "," ^ o 
      and option_to_string key value   = match value with None -> key | Some x -> key ^ "=" ^ x  
      in
      let add_option key value s       = append_option_string s (option_to_string key value)
      in    
	Hashtbl.fold add_option table ""
	  
    let parse option_string =
      
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
  end
	   
  module Mtab = struct

    exception Format_error

    let add_entry ~fstype ~device ~options ~mount_point =
      Shell_procedure.execute "/bin/mount" [ "-i" ; "-f"; "-t" ; fstype ; device ; "-o" ; options ; mount_point ]


    type ('mount_point_t, 'fstype_t, 'options_t) entry = {	
      mount_point    : 'mount_point_t ;
      mount_type     : 'fstype_t      ;
      mount_options  : 'options_t     ;
      device         : 'mount_point_t option;
    }

    let parse_line l =

      let drop_parens s =	
	let  l = String.length s  in
	  if String.get s 0     != '(' then raise Format_error;
	  if String.get s (l-1) != ')' then raise Format_error;	  
	  try
	    (String.sub s 1 (l-2))
	  with _ -> raise Format_error	
      in	
	( match l with 	      
	      dev :: "on" :: dir :: "type" :: fstype :: options :: []  when (String.get dev 0) = '/' -> 
		{ mount_point = dir; mount_type = fstype; mount_options = (drop_parens options); device = Some dev ; } 
	      
	    | dev :: "on" :: dir :: "type" :: fstype :: options :: [] ->
		{ mount_point = dir; mount_type = fstype; mount_options = (drop_parens options); device = None ; }
	    | _                -> raise Format_error
	)

    let line_to_entry l =
      parse_line (Str.split (Str.regexp "[ \t]+") l)

    let get () =
      List.map line_to_entry (Shell_procedure.get_output "/bin/mount" [])

    let get_mtab = get (* for internal use *)

    let get_entry dir =   
      let rec search_in l =    
	match l with
	    []                           -> raise Not_found
	  | h::_  when h.mount_point=dir -> h
	  | _ ::t                        -> search_in t
      in
      let e = search_in (get_mtab ())
      in  {e with mount_options = (Options.parse e.mount_options)}
  end
end;;



(* ------------------------------------------------------ *)


module Mapper  = struct

  let path_to_device_name p =
    "." ^ 
      (Str.global_replace  (Str.regexp "//*") "#" 
	 (Str.global_replace  (Str.regexp "/*$") "" 
	    p))
end
;;


module Crypto = struct

  module Plain = struct
    
    let create ~encrypted_device ~decrypted_device =
      Shell_procedure.execute 
	~stdin:inherit_from_parent
	~stdout:inherit_from_parent
	"/sbin/cryptsetup" [ "create" ; "-c" ; "aes" ; decrypted_device ; encrypted_device ]
	
    (* note: failure here might be due to too short container file *)
	
    let create_verify_pp ~encrypted_device ~decrypted_device =
      Shell_procedure.execute 
	~stdin:inherit_from_parent
	~stdout:inherit_from_parent
	"/sbin/cryptsetup" [ "create" ; "-y"; "-c" ; "aes" ; decrypted_device ; encrypted_device ]
	
    let remove ~decrypted_device =
      Shell_procedure.execute 
	"/sbin/cryptsetup" [ "remove" ; decrypted_device ]
  end
    
  module LUKS = struct
    
    let is_valid ~encrypted_device =
      Shell_procedure.get_status
	~stderr:when_trace
	"/sbin/cryptsetup" ["isLuks" ; encrypted_device] 

    let create ~encrypted_device ~decrypted_device =
      Shell_procedure.execute "/sbin/cryptsetup" [ "luksOpen" ; encrypted_device ; decrypted_device ]
	
    let remove ~decrypted_device =
      Shell_procedure.execute "/sbin/cryptsetup" [ "luksClose" ; decrypted_device ]
  end

  type setuptype = Plain | LUKS

  let create ~encrypted_device ~decrypted_device =
    
    if LUKS.is_valid ~encrypted_device 
    then ((LUKS.create encrypted_device decrypted_device);  LUKS  )
    else ((Plain.create encrypted_device decrypted_device); Plain )

  let remove ~decrypted_device ~setuptype =
    match setuptype with
	LUKS  -> LUKS.remove decrypted_device
      | Plain -> Plain.remove decrypted_device
end
;;
  
