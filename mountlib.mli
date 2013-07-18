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

val path_to_dmname : string -> string



module Mount: sig
  val attach            : fstype:string      -> device:string -> options:string -> mount_point:string -> unit
  val attach_hidden     : fstype:string      -> device:string -> options:string -> mount_point:string -> unit
  val add_entry         : fstype:string      -> device:string -> options:string -> mount_point:string -> unit
  val detach            : mount_point:string -> unit
  val detach_really     : mount_point:string -> unit
end



exception Mtab_format_error
type fs_type = string
type fs_options = string
type ('a, 'b, 'c) mount_spec = {
  mount_point : 'a;
  mount_type : 'b;
  mount_options : 'c;
  device : 'a option;
}


val parse_mtab_line : string list -> (string, string, string) mount_spec
type 'a option_table = ('a, string option) Hashtbl.t
val option_table_add : ('a, 'b option) Hashtbl.t -> 'a -> 'b -> unit
val option_table_get : ('a, 'b option) Hashtbl.t -> 'a -> 'b
val option_table_set : ('a, 'b option) Hashtbl.t -> 'a -> unit
val option_table_to_string : (string, string option) Hashtbl.t -> string
val parse_mount_options : string -> (string, string option) Hashtbl.t
val mtab_line_to_entry : string -> (string, string, string) mount_spec
val get_mtab : unit -> (string, string, string) mount_spec list
val get_mtab_entry :
  string -> (string, string, (string, string option) Hashtbl.t) mount_spec


module Crypto_device : sig
  val create : encrypted_device:string  -> decrypted_device:string -> unit
  val remove : decrypted_device:string  -> unit
end
