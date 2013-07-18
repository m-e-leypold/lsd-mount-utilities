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
  

  mountlib.mli -- bindings to mount, losetup, cryptsetup.

*)




module Loop: sig

  val delete : loop_device:string -> unit
  val setup  : loop_device:string -> file:string -> unit
  val probe  : file:string -> string
  val clean  : unit -> unit

  exception Probing_failed
  exception Binding_failed   of string  (* device path     *) 
  exception Unbinding_failed of string  (* device path     *)
end


module Filesystem: sig

  val mount             : fstype:string      -> device:string -> options:string -> mount_point:string -> unit
  val mount_hidden      : fstype:string      -> device:string -> options:string -> mount_point:string -> unit
  val umount            : mount_point:string -> unit
  val umount_really     : mount_point:string -> unit
  val mkfs              : ?blocksize:int     -> device:string -> unit
  val fsck              : device:string      -> unit

  module Options: sig
    type ('key, 'value) table = ('key, 'value option) Hashtbl.t
	
    val add    : ('key, 'value option) Hashtbl.t -> 'key -> 'value -> unit
    val get    : ('key, 'value option) Hashtbl.t -> 'key -> 'value

    val set    : ('key, 'value option) Hashtbl.t -> 'key -> unit
    val is_set : ('key, 'value option) Hashtbl.t -> 'key -> bool

    val has_key : ('key, 'value option) Hashtbl.t -> 'key -> bool
    val remove  : ('key, 'value option) Hashtbl.t -> 'key -> unit

    val table_to_string : (string, string option) Hashtbl.t -> string
    val parse           : string -> (string, string option) Hashtbl.t
  end


  module Mtab: sig

    exception Format_error

    val add_entry       : fstype:string      -> device:string -> options:string -> mount_point:string -> unit

    type ('mount_point_t, 'fstype_t, 'options_t) entry = {
      mount_point    : 'mount_point_t ;
      mount_type     : 'fstype_t      ;
      mount_options  : 'options_t     ;
      device         : 'mount_point_t option;
    }

    val get       : unit -> (string, string, string) entry list
    val get_entry :
      string -> (string, string, (string, string option) Hashtbl.t) entry

  end
end

module Mapper: sig 
  val path_to_device_name : string -> string
end



module Crypto: sig

  module Plain: sig
    val create_verify_pp : encrypted_device:string  -> decrypted_device:string -> unit
    val create           : encrypted_device:string  -> decrypted_device:string -> unit
    val remove           : decrypted_device:string  -> unit
  end

  module LUKS: sig
    val is_valid  : encrypted_device:string  -> bool
    val create    : encrypted_device:string  -> decrypted_device:string -> unit
    val remove    : decrypted_device:string  -> unit
  end

  type setuptype = Plain | LUKS

  val create    : encrypted_device:string  -> decrypted_device:string -> setuptype
  val remove    : decrypted_device:string  -> setuptype:setuptype -> unit
end
