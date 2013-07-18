module Modules :
  sig
    exception Format_error
    type 'a entry = { name : string; size : int; clients : 'a list; }
    val line_to_entry : string -> string entry
    val list : unit -> string entry list
    val get_info : string -> string entry
  end
