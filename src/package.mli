open Cv
type package

val get_timestamp: unit -> int

val pack: image -> string -> int -> package

val unpack: package -> (image * string * int)

val serialize: package -> string

val deserialize: string -> package

val encrypt: string -> string

val decrypt: string -> string

val get_encryption_key: unit -> string

val set_encryption_key: string -> unit

val generate_encryption_key: string -> unit

val is_encryption_key_set: unit -> bool
