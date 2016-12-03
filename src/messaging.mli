open Package
open Cv

val add_to_history_buffer: string -> package -> unit

val refresh_history_buffer: string -> package -> unit

val to_string: unit -> string
