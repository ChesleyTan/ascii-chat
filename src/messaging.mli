open Package
open Cv

val add_to_history_buffer: string -> package -> unit

val refresh_history_buffer: string -> package -> unit

val chat_history_to_string: unit -> string

val clear_history_buffer: unit -> unit
