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
  

  umount.lcrypt.ml -- Unmount utility for encrypted file system.

*)


open Mountlib;;

exception Abort;;

let mount_point = Sys.argv.(1)                in
let mtab_entry  = get_mtab_entry mount_point  in
let options     = mtab_entry.mount_options    in
let get_option  = option_table_get options    
in
  Unix.setuid (Unix.geteuid ()); 

  really_umount mount_point;
  let  mapperdev = (get_option "dmdev")
  and  loopdev   = (get_option "loopdev")
  in 
    crypto_unmap_device mapperdev;
    losetup_delete      loopdev
;;
