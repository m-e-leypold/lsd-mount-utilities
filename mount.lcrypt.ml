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

let options     = parse_mount_options mount_options
and loop_device = loprobe volume_container
in 
  try 
    option_table_add options "loopdev" loop_device;
    option_table_add options "fstype"  "ext2";

    if uid != 0 then option_table_add options "user" user;

    let mapper_device = path_to_dmname volume_container
    in 

      option_table_add options "dmdev" mapper_device;

      try 
	crypto_map_device loop_device mapper_device ;

	Unix.setuid (Unix.geteuid ());

	really_mount "ext2" ("/dev/mapper/"^mapper_device) mount_options mount_point; 
	  
	try 
	  mtab_add "dmcrypt2" volume_container (option_table_to_string options) mount_point;

	with x -> really_umount mount_point	  
      with x -> crypto_unmap_device mapper_device ; raise x
  with x -> losetup_delete loop_device ; raise x
;;


