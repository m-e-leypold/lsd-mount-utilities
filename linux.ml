
(* XXX add module probing,
   special loop module and soft probing for that ...
*)
   

module Modules = struct

  exception Format_error

  type 'client entry = { name    : string;
			 size    : int;
			 clients : 'client list;
		       }

  let line_to_entry l = 

    match (Str.split (Str.regexp "[ \t]+") l) with

	[name;size;_]         -> { name = name; size = int_of_string size; clients = [] }

      | [name;size;_;clients] -> { name = name; size = int_of_string size; 
				   clients = (Str.split (Str.regexp "[,]+") clients) }

      | _                     -> raise Format_error


  let list () =
    List.map line_to_entry (List.tl (Shell_procedure.get_output "/bin/lsmod" []))

  let get_info name = 
    let rec search_in l =    
      match l with
	  []                      -> raise Not_found
	| h::_  when h.name=name  -> h
	| _ ::t                   -> search_in t
    in
    let e = search_in (list ())
    in  
      e

(* List.filter or s.th? *)

end;;
  
