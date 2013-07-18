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
  

  mount.lcrypt.ml -- Mount utility for encrypted file system.

*)


open Mountlib;;

let volume_container = Sys.argv.(1);;
let mount_point      = Sys.argv.(2);;
let mount_options    = Sys.argv.(4);;

exception No_Error;;

let uid         = Unix.getuid ();;
let user        = (Unix.getpwuid uid).Unix.pw_name;;

let options     = Filesystem.Options.parse mount_options
and loop_device = Loop.probe volume_container
in 

  Filesystem.Options.remove options "luks";

  try 
    Filesystem.Options.add options "loopdev" loop_device;
    Filesystem.Options.add options "fstype"  "ext2";

    if uid != 0 then Filesystem.Options.add options "user" user;

    let mapper_device = path_to_dmname volume_container
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


