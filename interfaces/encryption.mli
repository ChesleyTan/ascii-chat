open Package

module type Encryption = sig

  (* initializes the encryption algorithm *)
  val initialize : unit -> unit

  (* encrypts the package into a string *)
  val encrypt : package -> string

end
