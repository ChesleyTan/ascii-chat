open Image

module type ConvertToColor = sig

  (* takes the given c_image and produces an array of colors representing a
     single 0-255 terminal color value for the pixels in the image *)
  val convert : c_image -> color array

end
