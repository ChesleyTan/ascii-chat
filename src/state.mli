open Package
open Cv

val update_input_buffer: char -> unit

val delete_input_buffer: unit -> unit

val log_message: unit -> unit

val log_image: image -> unit

val refresh_package: string -> package -> unit

val get_num_users: unit -> int

val get_input_buffer_contents: unit -> string

val get_packages: unit -> package list
