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

