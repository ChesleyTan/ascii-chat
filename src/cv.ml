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
let ascii_of_uchar n =
    if n < 100 then
        '.'
    else if n >= 100 && n < 200 then
        '*'
    else
        '#'

(* C++ interface bindings *)
let frame = foreign "read_frame" (void @-> returning (ptr uchar))
let frame_width = foreign "frame_width" (void @-> returning int)
let frame_height = foreign "frame_height" (void @-> returning int)
let frame_depth = foreign "frame_depth" (void @-> returning int)
let cleanup = foreign "cleanup" (void @-> returning void)

let _ =
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
            let avg =
                ((get frame_array (c2i (col, row, 0)) |> Unsigned.UChar.to_int) +
                (get frame_array (c2i (col, row, 1)) |> Unsigned.UChar.to_int) +
                (get frame_array (c2i (col, row, 2)) |> Unsigned.UChar.to_int)) / 3
            in Printf.printf "%c" (ascii_of_uchar avg)
            (*
            Printf.printf "(%s,%s,%s);"
                (string_of_uchar @@ get frame_array @@ c2i (col, row, 0))
                (string_of_uchar @@ get frame_array @@ c2i (col, row, 1))
                (string_of_uchar @@ get frame_array @@ c2i (col, row, 2))
            *)
        done;
        Printf.printf "\n"
    done;
    cleanup ()
