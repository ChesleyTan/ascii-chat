(* [safe_int_of_string] is a wrapper around [int_of_string], returning None when
 * the string cannot be converted to an integer
 *)
val safe_int_of_string : string -> int option

(* Returns the ip address and port of this user *)
val get_address_self : unit -> string
