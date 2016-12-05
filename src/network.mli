open Package

(* the address associated with the computer network interface *)
val my_address : string

(* sends the package to all the connected computers.
 * [send package f] sends the [package] and uses the callback function [f] if
 * the send to the computer failed
 *)
val send : package -> (string -> unit) -> unit

(* initializes the network.
 * [network_initialize port f host] starts a server and connects to [host] if
 * [host] is not an empty string. The server listens for available packets
 * on [port]. The callback functions [f] is called on the received packets.
 * [(fst f) user package] should handle the [package] received from the [user]
 * [(snd f) user] should handle removal of the [user]
 *)
val network_initialize : int -> (string -> package -> unit) * (string -> unit)
                         -> string -> unit Lwt.t
