open Image

module type Package = sig

  (* type representing the package that contains the data for a single time
     frame *)
  type package

  (* creates the package containing the ascii image and the text for the given
     time frame *)
  val pack : ascii_image -> text -> package

  (* unpacks the data inside the package into the ascii image and the text for
     the given time frame *)
  val unpack : package -> (ascii_image, text)

end
