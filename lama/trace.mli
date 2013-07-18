(*

  Lama library -- Lama arbitrary module assortement

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
  
  ...                                                   

*)


val tracing : bool
val is_active : unit -> bool
val print : string -> string -> string option -> unit
class type tracer =
  object
    method event : string -> unit
    method int : string -> int -> unit
    method msg : string -> string option -> unit
    method string : string -> string -> unit
    method string_list : string -> string list -> unit
  end
class null_tracer : tracer
class module_tracer :
  string ->
  object
    val scope_name : string
    method event : string -> unit
    method int : string -> int -> unit
    method msg : string -> string option -> unit
    method string : string -> string -> unit
    method string_list : string -> string list -> unit
  end
val for_module : string -> tracer
