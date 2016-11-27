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
