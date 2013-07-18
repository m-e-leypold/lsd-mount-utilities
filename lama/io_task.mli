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





type condition = { fd : Unix.file_descr; action : process; }
and system = {
  input_conditions : (Unix.file_descr, condition) Hashtbl.t;
  output_conditions : (Unix.file_descr, condition) Hashtbl.t;
  queue : process Queue.t;
}
and process_continuation = Continue_Task of process | Terminate_Task
and process = unit -> process_continuation
val new_system : ?in_table_size:int -> ?out_table_size:int -> unit -> system
val input_fd_list : system -> Unix.file_descr list
val output_fd_list : system -> Unix.file_descr list
val schedule_process : system -> process -> unit
val add_input : system -> condition -> unit
val add_output : system -> condition -> unit
val remove_input : system -> Unix.file_descr -> unit
val remove_output : system -> Unix.file_descr -> unit
val conditions_pending : system -> bool
val processes_runnable : system -> bool
val wakeup : system -> ('a, condition) Hashtbl.t -> 'a list -> unit
val wakeup_all :
  system -> Unix.file_descr list -> Unix.file_descr list -> unit
val select :
  ?noblock:bool ->
  Unix.file_descr list ->
  Unix.file_descr list -> Unix.file_descr list * Unix.file_descr list
val poll : ?noblock:bool -> system -> unit
val wait : system -> unit
val run_queue : system -> unit
val run : system -> unit
type 'a data = Data of 'a | Data_not_ready | Data_end
val read_to_str : string -> Unix.file_descr -> string data
val read_line : in_channel -> string data
type write_result = Complete | Bytes_left of int
val write_from_str : Unix.file_descr -> string -> int -> write_result
type ('a, 'b) data_sink_operation = 'a -> 'b -> 'a
type 'a data_sink_finalization = 'a -> unit
type ('a, 'b) data_source_get = 'a -> ('b * 'a) option
type 'a data_source_cleanup = 'a option -> unit
module Create :
  sig
    val copy_raw_to_sink :
      system ->
      ('a, string) data_sink_operation ->
      ?finally:'a data_sink_finalization -> 'a -> Unix.file_descr -> unit
    val copy_to_stdout : system -> Unix.file_descr -> unit
    val copy_lines_to_sink :
      system ->
      ('a, string) data_sink_operation ->
      'a data_sink_finalization -> 'a -> in_channel -> unit
    val copy_lines_to_stdout : system -> in_channel -> unit
    val read_textfile : system -> string list ref -> in_channel -> unit
    val feed_from_source :
      system ->
      ('a, string) data_source_get ->
      ?cleanup:'a option data_sink_finalization ->
      'a ->
      ?auto_close:bool -> ?ignore_broken_pipe:bool -> Unix.file_descr -> unit
    val feed_list :
      system ->
      string list ->
      ?auto_close:bool ->
      ?ignore_broken_pipe:bool ->
      ?cleanup:(string list * bool) option data_sink_finalization ->
      Unix.file_descr -> unit
  end
