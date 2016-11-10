module type Image = sig

  (* type representing the image coming from the c++ layer *)
  type c_image

  (* type representing the terminal based 256 color *)
  type color

  (* type representing the ascii image *)
  type ascii_image = (color array, char array)

end
