open Package
open Unix
open Utils
open Lwt

let my_address = Utils.get_address_self()

let my_inet_address = Unix.inet_addr_of_string my_address

let send package =
  failwith "Unimplemented"

let network_initialize port f =
  failwith "Unimplemented"
