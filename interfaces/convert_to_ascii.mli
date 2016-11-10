open Image

module type ConvertToASCII = sig

  (* takes the given c_image and produces an array of chars representing general
     look of the pixels in the image *)
  val convert : c_image -> char array

end
