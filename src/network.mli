open Package

(* the address associated with the computer network interface *)
val my_address : string

(* sends the package to all the connected computers *)
val send : package -> unit

(* initializes the network.
 * [network_initialize port f] starts a server and listens for available packets
 * on [port]. The callback function [f] is called on the received packet.
 * [f user package] should handle the [package] received from the [user]
 *)
val network_initialize : int -> (string -> package -> unit) -> string
                         -> unit Lwt.t
