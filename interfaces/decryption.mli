open Package

module type Decryption = sig

  (* initializes the decryption algorithm *)
  val initialize : unit -> unit

  (* decrypts the string into a package *)
  val decrypt : string -> package

end
