open Cv
type package

val pack: image -> package

val unpack: package -> image

val serialize: package -> string

val deserialize: string -> package
