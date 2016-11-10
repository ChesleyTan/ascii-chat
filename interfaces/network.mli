module type Network = sig

  (* sends data to the given host *)
  val send : string -> string -> unit

  (* receives data and the sender from the network socket *)
  val receive : unit -> (string, string)

end
