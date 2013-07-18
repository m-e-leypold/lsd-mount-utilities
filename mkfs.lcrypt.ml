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
  

  mkfs.lcrypt.ml -- Mkfs utility for encrypted file system.

*)


exception Not_enough_args of string;;
exception Too_many_args;;
exception Arg_error of string;;

let volume_container, mkfs_args =
  let rec process_args args args' =

    match args with

        []    -> raise (Not_enough_args "need at least volume_container as argument")
      | [x]   -> (x, None   )
      | [x;y] -> (x, Some ( try (int_of_string y) 
			    with Failure _ -> raise (Arg_error "blocksize not numeric") | x -> raise x))

      (* XXX specific argument processing will happen here -- but is not implemented yet! *)

      | _      -> raise Too_many_args
  in
    process_args (List.tl ( Array.to_list Sys.argv )) []
;;


open Mountlib;;

exception No_Error;;

let loop_device = Loop.probe volume_container
in 
  begin  try 
  
    let mapper_device = path_to_dmname volume_container
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

