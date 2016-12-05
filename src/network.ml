open Package
open Unix
open Lwt

let my_inet_address =
  let host_address_list =
    (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list
  in
  host_address_list.((Array.length host_address_list) - 1)

let my_address = string_of_inet_addr my_inet_address

let send package =
  failwith "Unimplemented"

let network_initialize port f =
  failwith "Unimplemented"
