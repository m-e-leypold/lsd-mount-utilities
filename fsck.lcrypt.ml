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
  

  fsck.lcrypt.ml -- Fsck utility for encrypted file system.

*)


(* For the time being (during development) we have fixed arguments and
   not much argument processing.  The first version will also not have
   LUKS support and not care about issues like that the data owner is
   not trusting root.
*)

open Mountlib;;

exception Not_enough_args of string;;
exception Too_many_args of string;;
exception Arg_error of string;;


let volume_container, fsck_args =
  let rec process_args args args' =
    
    match args with
	[]     -> raise (Not_enough_args "need at least volume container")
      | [x]    -> (x, args')
	  
      (* specific arguments *)

      | _      -> raise (Too_many_args "Can'r process additional arguments yet")
  in
    process_args (List.tl ( Array.to_list Sys.argv )) []
;;


exception No_Error;;

let loop_device = Loop.probe volume_container
in 
  begin  try 
  
    let mapper_device = path_to_dmname volume_container
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

