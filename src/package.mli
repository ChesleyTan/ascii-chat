open Cv
type package

val pack: image -> string -> package

val unpack: package -> (image * string)

val serialize: package -> string

val deserialize: string -> package

val compress: string -> string

val decompress: string -> string
