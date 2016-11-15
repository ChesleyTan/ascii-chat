open Ctypes
open Ctypes.CArray
open Foreign

(* Converts a three-dimensional coordinate to an index in the corresponding
 * representative one-dimensional array *)
let coordinate_to_index (height, width, depth) (col, row, dep) =
    let row_offset = row * width * depth in
    let col_offset = col * depth in
    row_offset + col_offset + dep

let string_of_uchar = Unsigned.UChar.to_string

let _ =
    let frame = foreign "read_frame" (void @-> returning (ptr uchar)) in
    let frame_width = foreign "frame_width" (void @-> returning int) in
    let frame_height = foreign "frame_height" (void @-> returning int) in
    let frame_depth = foreign "frame_depth" (void @-> returning int) in
    let frame_ptr = frame () in
    let width = frame_width () in
    let height = frame_height () in
    let depth = frame_depth () in
    let frame_array = from_ptr frame_ptr (width * height * depth) in
    let c2i = coordinate_to_index (height, width, depth) in
    Printf.printf "%s x %s\n" (string_of_int height)
                              (string_of_int width);
    for row = 0 to height - 1 do
        for col = 0 to width - 1 do
            Printf.printf "(%s,%s,%s)\n"
                (string_of_uchar @@ get frame_array @@ c2i (col, row, 0))
                (string_of_uchar @@ get frame_array @@ c2i (col, row, 1))
                (string_of_uchar @@ get frame_array @@ c2i (col, row, 2))
        done
    done
