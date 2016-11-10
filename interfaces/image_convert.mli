open ConvertToASCII
open ConvertToColor

module type ImageConvert = sig

  (* gets an image from the camera and converts it to the desired ascii image *)
  val camera_image : unit -> (char array, color array)

end
