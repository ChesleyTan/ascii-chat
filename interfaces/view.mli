open Network
open Package

module type View = sig

  (* displays the next available image and text from all clients *)
  val display : unit -> unit

end
