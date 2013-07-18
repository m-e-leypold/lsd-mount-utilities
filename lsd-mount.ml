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
  

  lsd-mount.ml -- Program image for all functions of lsd-mount-utilities.

*)


open Mountlib;;

exception Usage_Error of string;;

let mount () = 

  let  volume_container = Sys.argv.(1)
  and  mount_point      = Sys.argv.(2) 
  and  mount_options    = Sys.argv.(4)  in

  let  uid              = Unix.getuid ()  in
  let  user             = (Unix.getpwuid uid).Unix.pw_name  in 

  let      options = Filesystem.Options.parse mount_options  
  and  loop_device = Loop.probe volume_container

  in 

    Filesystem.Options.remove options "luks";
    
    try 
      Filesystem.Options.add options "loopdev" loop_device;
      Filesystem.Options.add options "fstype"  "ext2";
      
      if uid != 0 then Filesystem.Options.add options "user" user;
      
      let mapper_device = Mapper.path_to_device_name volume_container
      in 
	
	Filesystem.Options.add options "dmdev" mapper_device;
	
	try 
	  begin
	    match Crypto.create loop_device mapper_device 
	    with
		Crypto.LUKS  -> Filesystem.Options.set options "luks"
	      | Crypto.Plain -> ()
	  end;
	  
	  Unix.setuid (Unix.geteuid ());
	  
	  Filesystem.mount_hidden "ext2" ("/dev/mapper/"^mapper_device) mount_options mount_point; 
	  
	  try 
	    Filesystem.Mtab.add_entry "lcrypt" volume_container (Filesystem.Options.table_to_string options) mount_point;
	    
	  with x -> Filesystem.umount_really mount_point ; raise x
	with x -> Crypto.Plain.remove mapper_device ; raise x
    with x -> Loop.delete loop_device ; raise x
;;



let umount () = 

  let mount_point = Sys.argv.(1)                in
  let mtab_entry  = Filesystem.Mtab.get_entry mount_point  in
  let options     = mtab_entry.Filesystem.Mtab.mount_options    in
  let get_option  = Filesystem.Options.get    options    
  and has_option  = Filesystem.Options.is_set options   
  in
    Unix.setuid (Unix.geteuid ()); 
    
    Filesystem.umount_really mount_point;
    let  mapperdev = (get_option "dmdev")
    and  loopdev   = (get_option "loopdev")
    and  is_luks   = (has_option "luks")
    in 
      Crypto.remove 
	mapperdev 
	(if is_luks then Crypto.LUKS else Crypto.Plain);
      Loop.delete loopdev
;;



exception Not_enough_args of string;;
exception Too_many_args of string;;
exception Arg_error of string;;

let fsck () =

  let volume_container, fsck_args =
    let rec process_args args args' =
    
      match args with
	  []     -> raise (Not_enough_args "need at least volume container")
	| [x]    -> (x, args')
	    
	(* specific arguments *)
	    
	| _      -> raise (Too_many_args "Can'r process additional arguments yet")
    in
      process_args (List.tl ( Array.to_list Sys.argv )) []
  in


  let loop_device = Loop.probe volume_container
  in 
    begin  try 
      
      let mapper_device = Mapper.path_to_device_name volume_container
      in 
	
	begin try 
	  Crypto.Plain.create loop_device mapper_device;
	  
	  Filesystem.fsck ("/dev/mapper/" ^ mapper_device) (* XXX abstract this *)
	    
	with x -> Crypto.Plain.remove mapper_device ; raise x
	end;
	
	Crypto.Plain.remove mapper_device
	  
    with x -> Loop.delete loop_device ; raise x
    end;
    Loop.delete loop_device
;;



let mkfs () =
  let volume_container, mkfs_args =
    let rec process_args args args' =
      
      match args with
	  
          []    -> raise (Not_enough_args "need at least volume_container as argument")
	| [x]   -> (x, None   )
	| [x;y] -> (x, Some ( try (int_of_string y) 
			      with Failure _ -> raise (Arg_error "blocksize not numeric") | x -> raise x))
	    
	(* XXX specific argument processing will happen here -- but is not implemented yet! *)
	    
	| _      -> raise ( Too_many_args  "???" )
    in
      process_args (List.tl ( Array.to_list Sys.argv )) []

  in


  let loop_device = Loop.probe volume_container
  in 
    begin  try 
      
      let mapper_device = Mapper.path_to_device_name volume_container
      in 
	
	begin try 
	  Crypto.Plain.create_verify_pp loop_device mapper_device;
	  
	  Unix.sleep 10;
	  
	  Filesystem.mkfs ("/dev/mapper/" ^ mapper_device) (* XXX abstract this *)
	    
	with x -> Crypto.Plain.remove mapper_device ; raise x
	end;
	
	Crypto.Plain.remove mapper_device
	  
    with x -> Loop.delete loop_device ; raise x
    end;
    Loop.delete loop_device
;;

let loclean() =
  Mountlib.Loop.clean  ()
;;


let name = (List.hd (List.rev (Str.split (Str.regexp "/") Sys.argv.(0))))  in

  match name with

      "mount.lcrypt"   -> mount   ()
    | "umount.lcrypt"  -> umount  ()
    | "fsck.lcrypt"    -> fsck    ()
    | "mkfs.lcrypt"    -> mkfs    ()
    | "loclean"        -> loclean ()
	
    | "lsd-mount.bin" | "lsd-mount.suid" ->
	
	raise (Usage_Error ("lsd-mount.* is only the program image: use one of the linked instances"))

    | _          -> raise (Usage_Error ("Name/function not recognized: "^name))
;;
