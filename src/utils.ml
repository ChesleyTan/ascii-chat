open Ctypes
open Foreign

let safe_int_of_string s =
  try
    Some (int_of_string s)
  with
  | Failure _ -> None

(* Returns the ip address and port of this user *)
let get_address_self = foreign "get_addr_self" (void @-> returning string)
