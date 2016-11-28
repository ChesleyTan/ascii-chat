open Cv
type package

val get_timestamp: unit -> int

val pack: image -> string -> int -> package

val unpack: package -> (image * string * int)

val serialize: package -> string

val deserialize: string -> package

val compress: string -> string

val decompress: string -> string
