module type FastStringSig = sig
    type t
    val create: int -> t
    val make: int -> char -> t
    val length: t -> int
    val get: t -> int -> char
    val set: t -> int -> char -> unit
    val to_string: t -> string
    val append: t -> string -> unit
    val append_char: t -> char -> unit
end

module FastString: FastStringSig

type image = { data: FastString.t
             ; colors: string array
             ; width: int
             ; height: int
             ; text_only: bool}

module type CvSig = sig
    val coordinate_to_index: (int * int * int) -> (int * int * int) -> int
    val cleanup: unit -> unit
    val colorize: image -> string
    val get_frame: bool -> image
end

module Cv: CvSig
