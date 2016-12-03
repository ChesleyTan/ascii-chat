val history_buffer: string list ref 

(*val message_mapping = ('_a, '_b) Hashtbl.t *)

val add_to_history_buffer: string -> string -> string -> unit

val refresh_history_buffer: string -> string -> string -> unit

val to_string: string
