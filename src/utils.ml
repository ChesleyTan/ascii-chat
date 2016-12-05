open Ctypes
open Foreign

let get_address_self = foreign "get_addr_self" (void @-> returning string)
