open Image

module type OpenCV = sig

  val get_camera_image : unit -> c_image

end
