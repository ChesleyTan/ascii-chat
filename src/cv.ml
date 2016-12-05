open Ctypes
open Ctypes.CArray
open Foreign

(* Fast version of string using byte buffer *)
module type FastStringSig = sig
    type t
    val create: int -> t
    val make: int -> char -> t
    val length: t -> int
    val get: t -> int -> char
    val set: t -> int -> char -> unit
    val of_string: string -> t
    val to_string: t -> string
    val append: t -> string -> unit
    val append_char: t -> char -> unit
end

module FastString: FastStringSig = struct
    type t = {data: Bytes.t; length: int ref; size: int ref}
    let create n =
        { data = Bytes.create n
        ; length = ref 0
        ; size = ref n
        }
    let make n c =
        { data = Bytes.make n c
        ; length = ref 0
        ; size = ref n
        }
    let length {data; length; size} = !length
    let get {data; length; size} n = Bytes.get data n
    let set {data; length; size} n c = Bytes.set data n c
    let to_string {data; length; size} = Bytes.sub_string data 0 !length
    let of_string s =
        let bytes_string = Bytes.of_string s in
        let strlen = Bytes.length bytes_string in
        { data = bytes_string
        ; length = ref strlen
        ; size = ref strlen
        }
    let append {data; length; size} x =
        let x_len = String.length x in
        let new_len = !length + x_len in
        if new_len > !size then
            failwith "FastString index out of bounds!"
        else
            Bytes.blit_string x 0 data !length x_len;
            length := new_len
    let append_char {data; length; size} x =
        Bytes.set data !length x;
        incr length
end

type image = { data: FastString.t
             ; colors: string array
             ; width: int
             ; height: int
             ; text_only: bool}

module type CvSig = sig
    val coordinate_to_index: (int * int * int) -> (int * int * int) -> int
    val cleanup: unit -> unit
    val colorize: bool -> image -> string array array
    val get_frame: bool -> int -> int -> image
end

module Cv: CvSig = struct
    (* Converts a three-dimensional coordinate to an index in the corresponding
    * representative one-dimensional array *)
    let coordinate_to_index (height, width, depth) (col, row, dep) =
        let row_offset = row * width * depth in
        let col_offset = col * depth in
        row_offset + col_offset + dep

    let string_of_uchar = Unsigned.UChar.to_string
    let string_of_char = String.make 1
    let ascii_of_uchar n =
        if n < 50 then
            '`'
        else if n >= 50 && n < 100 then
            '.'
        else if n >= 100 && n < 200 then
            '*'
        else
            '#'

    let ansi_reset = "\x1B[0m"

    (* C++ interface bindings *)
    let frame = foreign "read_frame" (int @-> int @-> returning (ptr uchar))
    let frame_width = foreign "frame_width" (void @-> returning int)
    let frame_height = foreign "frame_height" (void @-> returning int)
    let frame_depth = foreign "frame_depth" (void @-> returning int)
    let cleanup = foreign "cleanup" (void @-> returning void)

    (* Credits for color mapping: https://gist.github.com/MicahElliott/719710 *)
    let color_mapping = Hashtbl.create 256
    let _ =
        Hashtbl.add color_mapping "000000" "000";
        Hashtbl.add color_mapping "800000" "001";
        Hashtbl.add color_mapping "008000" "002";
        Hashtbl.add color_mapping "808000" "003";
        Hashtbl.add color_mapping "000080" "004";
        Hashtbl.add color_mapping "800080" "005";
        Hashtbl.add color_mapping "008080" "006";
        Hashtbl.add color_mapping "C0C0C0" "007";
        Hashtbl.add color_mapping "808080" "008";
        Hashtbl.add color_mapping "FF0000" "009";
        Hashtbl.add color_mapping "00FF00" "010";
        Hashtbl.add color_mapping "FFFF00" "011";
        Hashtbl.add color_mapping "0000FF" "012";
        Hashtbl.add color_mapping "FF00FF" "013";
        Hashtbl.add color_mapping "00FFFF" "014";
        Hashtbl.add color_mapping "FFFFFF" "015";
        Hashtbl.add color_mapping "000000" "016";
        Hashtbl.add color_mapping "00005F" "017";
        Hashtbl.add color_mapping "000087" "018";
        Hashtbl.add color_mapping "0000AF" "019";
        Hashtbl.add color_mapping "0000D7" "020";
        Hashtbl.add color_mapping "0000FF" "021";
        Hashtbl.add color_mapping "005F00" "022";
        Hashtbl.add color_mapping "005F5F" "023";
        Hashtbl.add color_mapping "005F87" "024";
        Hashtbl.add color_mapping "005FAF" "025";
        Hashtbl.add color_mapping "005FD7" "026";
        Hashtbl.add color_mapping "005FFF" "027";
        Hashtbl.add color_mapping "008700" "028";
        Hashtbl.add color_mapping "00875F" "029";
        Hashtbl.add color_mapping "008787" "030";
        Hashtbl.add color_mapping "0087AF" "031";
        Hashtbl.add color_mapping "0087D7" "032";
        Hashtbl.add color_mapping "0087FF" "033";
        Hashtbl.add color_mapping "00AF00" "034";
        Hashtbl.add color_mapping "00AF5F" "035";
        Hashtbl.add color_mapping "00AF87" "036";
        Hashtbl.add color_mapping "00AFAF" "037";
        Hashtbl.add color_mapping "00AFD7" "038";
        Hashtbl.add color_mapping "00AFFF" "039";
        Hashtbl.add color_mapping "00D700" "040";
        Hashtbl.add color_mapping "00D75F" "041";
        Hashtbl.add color_mapping "00D787" "042";
        Hashtbl.add color_mapping "00D7AF" "043";
        Hashtbl.add color_mapping "00D7D7" "044";
        Hashtbl.add color_mapping "00D7FF" "045";
        Hashtbl.add color_mapping "00FF00" "046";
        Hashtbl.add color_mapping "00FF5F" "047";
        Hashtbl.add color_mapping "00FF87" "048";
        Hashtbl.add color_mapping "00FFAF" "049";
        Hashtbl.add color_mapping "00FFD7" "050";
        Hashtbl.add color_mapping "00FFFF" "051";
        Hashtbl.add color_mapping "5F0000" "052";
        Hashtbl.add color_mapping "5F005F" "053";
        Hashtbl.add color_mapping "5F0087" "054";
        Hashtbl.add color_mapping "5F00AF" "055";
        Hashtbl.add color_mapping "5F00D7" "056";
        Hashtbl.add color_mapping "5F00FF" "057";
        Hashtbl.add color_mapping "5F5F00" "058";
        Hashtbl.add color_mapping "5F5F5F" "059";
        Hashtbl.add color_mapping "5F5F87" "060";
        Hashtbl.add color_mapping "5F5FAF" "061";
        Hashtbl.add color_mapping "5F5FD7" "062";
        Hashtbl.add color_mapping "5F5FFF" "063";
        Hashtbl.add color_mapping "5F8700" "064";
        Hashtbl.add color_mapping "5F875F" "065";
        Hashtbl.add color_mapping "5F8787" "066";
        Hashtbl.add color_mapping "5F87AF" "067";
        Hashtbl.add color_mapping "5F87D7" "068";
        Hashtbl.add color_mapping "5F87FF" "069";
        Hashtbl.add color_mapping "5FAF00" "070";
        Hashtbl.add color_mapping "5FAF5F" "071";
        Hashtbl.add color_mapping "5FAF87" "072";
        Hashtbl.add color_mapping "5FAFAF" "073";
        Hashtbl.add color_mapping "5FAFD7" "074";
        Hashtbl.add color_mapping "5FAFFF" "075";
        Hashtbl.add color_mapping "5FD700" "076";
        Hashtbl.add color_mapping "5FD75F" "077";
        Hashtbl.add color_mapping "5FD787" "078";
        Hashtbl.add color_mapping "5FD7AF" "079";
        Hashtbl.add color_mapping "5FD7D7" "080";
        Hashtbl.add color_mapping "5FD7FF" "081";
        Hashtbl.add color_mapping "5FFF00" "082";
        Hashtbl.add color_mapping "5FFF5F" "083";
        Hashtbl.add color_mapping "5FFF87" "084";
        Hashtbl.add color_mapping "5FFFAF" "085";
        Hashtbl.add color_mapping "5FFFD7" "086";
        Hashtbl.add color_mapping "5FFFFF" "087";
        Hashtbl.add color_mapping "870000" "088";
        Hashtbl.add color_mapping "87005F" "089";
        Hashtbl.add color_mapping "870087" "090";
        Hashtbl.add color_mapping "8700AF" "091";
        Hashtbl.add color_mapping "8700D7" "092";
        Hashtbl.add color_mapping "8700FF" "093";
        Hashtbl.add color_mapping "875F00" "094";
        Hashtbl.add color_mapping "875F5F" "095";
        Hashtbl.add color_mapping "875F87" "096";
        Hashtbl.add color_mapping "875FAF" "097";
        Hashtbl.add color_mapping "875FD7" "098";
        Hashtbl.add color_mapping "875FFF" "099";
        Hashtbl.add color_mapping "878700" "100";
        Hashtbl.add color_mapping "87875F" "101";
        Hashtbl.add color_mapping "878787" "102";
        Hashtbl.add color_mapping "8787AF" "103";
        Hashtbl.add color_mapping "8787D7" "104";
        Hashtbl.add color_mapping "8787FF" "105";
        Hashtbl.add color_mapping "87AF00" "106";
        Hashtbl.add color_mapping "87AF5F" "107";
        Hashtbl.add color_mapping "87AF87" "108";
        Hashtbl.add color_mapping "87AFAF" "109";
        Hashtbl.add color_mapping "87AFD7" "110";
        Hashtbl.add color_mapping "87AFFF" "111";
        Hashtbl.add color_mapping "87D700" "112";
        Hashtbl.add color_mapping "87D75F" "113";
        Hashtbl.add color_mapping "87D787" "114";
        Hashtbl.add color_mapping "87D7AF" "115";
        Hashtbl.add color_mapping "87D7D7" "116";
        Hashtbl.add color_mapping "87D7FF" "117";
        Hashtbl.add color_mapping "87FF00" "118";
        Hashtbl.add color_mapping "87FF5F" "119";
        Hashtbl.add color_mapping "87FF87" "120";
        Hashtbl.add color_mapping "87FFAF" "121";
        Hashtbl.add color_mapping "87FFD7" "122";
        Hashtbl.add color_mapping "87FFFF" "123";
        Hashtbl.add color_mapping "AF0000" "124";
        Hashtbl.add color_mapping "AF005F" "125";
        Hashtbl.add color_mapping "AF0087" "126";
        Hashtbl.add color_mapping "AF00AF" "127";
        Hashtbl.add color_mapping "AF00D7" "128";
        Hashtbl.add color_mapping "AF00FF" "129";
        Hashtbl.add color_mapping "AF5F00" "130";
        Hashtbl.add color_mapping "AF5F5F" "131";
        Hashtbl.add color_mapping "AF5F87" "132";
        Hashtbl.add color_mapping "AF5FAF" "133";
        Hashtbl.add color_mapping "AF5FD7" "134";
        Hashtbl.add color_mapping "AF5FFF" "135";
        Hashtbl.add color_mapping "AF8700" "136";
        Hashtbl.add color_mapping "AF875F" "137";
        Hashtbl.add color_mapping "AF8787" "138";
        Hashtbl.add color_mapping "AF87AF" "139";
        Hashtbl.add color_mapping "AF87D7" "140";
        Hashtbl.add color_mapping "AF87FF" "141";
        Hashtbl.add color_mapping "AFAF00" "142";
        Hashtbl.add color_mapping "AFAF5F" "143";
        Hashtbl.add color_mapping "AFAF87" "144";
        Hashtbl.add color_mapping "AFAFAF" "145";
        Hashtbl.add color_mapping "AFAFD7" "146";
        Hashtbl.add color_mapping "AFAFFF" "147";
        Hashtbl.add color_mapping "AFD700" "148";
        Hashtbl.add color_mapping "AFD75F" "149";
        Hashtbl.add color_mapping "AFD787" "150";
        Hashtbl.add color_mapping "AFD7AF" "151";
        Hashtbl.add color_mapping "AFD7D7" "152";
        Hashtbl.add color_mapping "AFD7FF" "153";
        Hashtbl.add color_mapping "AFFF00" "154";
        Hashtbl.add color_mapping "AFFF5F" "155";
        Hashtbl.add color_mapping "AFFF87" "156";
        Hashtbl.add color_mapping "AFFFAF" "157";
        Hashtbl.add color_mapping "AFFFD7" "158";
        Hashtbl.add color_mapping "AFFFFF" "159";
        Hashtbl.add color_mapping "D70000" "160";
        Hashtbl.add color_mapping "D7005F" "161";
        Hashtbl.add color_mapping "D70087" "162";
        Hashtbl.add color_mapping "D700AF" "163";
        Hashtbl.add color_mapping "D700D7" "164";
        Hashtbl.add color_mapping "D700FF" "165";
        Hashtbl.add color_mapping "D75F00" "166";
        Hashtbl.add color_mapping "D75F5F" "167";
        Hashtbl.add color_mapping "D75F87" "168";
        Hashtbl.add color_mapping "D75FAF" "169";
        Hashtbl.add color_mapping "D75FD7" "170";
        Hashtbl.add color_mapping "D75FFF" "171";
        Hashtbl.add color_mapping "D78700" "172";
        Hashtbl.add color_mapping "D7875F" "173";
        Hashtbl.add color_mapping "D78787" "174";
        Hashtbl.add color_mapping "D787AF" "175";
        Hashtbl.add color_mapping "D787D7" "176";
        Hashtbl.add color_mapping "D787FF" "177";
        Hashtbl.add color_mapping "D7AF00" "178";
        Hashtbl.add color_mapping "D7AF5F" "179";
        Hashtbl.add color_mapping "D7AF87" "180";
        Hashtbl.add color_mapping "D7AFAF" "181";
        Hashtbl.add color_mapping "D7AFD7" "182";
        Hashtbl.add color_mapping "D7AFFF" "183";
        Hashtbl.add color_mapping "D7D700" "184";
        Hashtbl.add color_mapping "D7D75F" "185";
        Hashtbl.add color_mapping "D7D787" "186";
        Hashtbl.add color_mapping "D7D7AF" "187";
        Hashtbl.add color_mapping "D7D7D7" "188";
        Hashtbl.add color_mapping "D7D7FF" "189";
        Hashtbl.add color_mapping "D7FF00" "190";
        Hashtbl.add color_mapping "D7FF5F" "191";
        Hashtbl.add color_mapping "D7FF87" "192";
        Hashtbl.add color_mapping "D7FFAF" "193";
        Hashtbl.add color_mapping "D7FFD7" "194";
        Hashtbl.add color_mapping "D7FFFF" "195";
        Hashtbl.add color_mapping "FF0000" "196";
        Hashtbl.add color_mapping "FF005F" "197";
        Hashtbl.add color_mapping "FF0087" "198";
        Hashtbl.add color_mapping "FF00AF" "199";
        Hashtbl.add color_mapping "FF00D7" "200";
        Hashtbl.add color_mapping "FF00FF" "201";
        Hashtbl.add color_mapping "FF5F00" "202";
        Hashtbl.add color_mapping "FF5F5F" "203";
        Hashtbl.add color_mapping "FF5F87" "204";
        Hashtbl.add color_mapping "FF5FAF" "205";
        Hashtbl.add color_mapping "FF5FD7" "206";
        Hashtbl.add color_mapping "FF5FFF" "207";
        Hashtbl.add color_mapping "FF8700" "208";
        Hashtbl.add color_mapping "FF875F" "209";
        Hashtbl.add color_mapping "FF8787" "210";
        Hashtbl.add color_mapping "FF87AF" "211";
        Hashtbl.add color_mapping "FF87D7" "212";
        Hashtbl.add color_mapping "FF87FF" "213";
        Hashtbl.add color_mapping "FFAF00" "214";
        Hashtbl.add color_mapping "FFAF5F" "215";
        Hashtbl.add color_mapping "FFAF87" "216";
        Hashtbl.add color_mapping "FFAFAF" "217";
        Hashtbl.add color_mapping "FFAFD7" "218";
        Hashtbl.add color_mapping "FFAFFF" "219";
        Hashtbl.add color_mapping "FFD700" "220";
        Hashtbl.add color_mapping "FFD75F" "221";
        Hashtbl.add color_mapping "FFD787" "222";
        Hashtbl.add color_mapping "FFD7AF" "223";
        Hashtbl.add color_mapping "FFD7D7" "224";
        Hashtbl.add color_mapping "FFD7FF" "225";
        Hashtbl.add color_mapping "FFFF00" "226";
        Hashtbl.add color_mapping "FFFF5F" "227";
        Hashtbl.add color_mapping "FFFF87" "228";
        Hashtbl.add color_mapping "FFFFAF" "229";
        Hashtbl.add color_mapping "FFFFD7" "230";
        Hashtbl.add color_mapping "FFFFFF" "231";
        Hashtbl.add color_mapping "080808" "232";
        Hashtbl.add color_mapping "121212" "233";
        Hashtbl.add color_mapping "1C1C1C" "234";
        Hashtbl.add color_mapping "262626" "235";
        Hashtbl.add color_mapping "303030" "236";
        Hashtbl.add color_mapping "3A3A3A" "237";
        Hashtbl.add color_mapping "444444" "238";
        Hashtbl.add color_mapping "4E4E4E" "239";
        Hashtbl.add color_mapping "585858" "240";
        Hashtbl.add color_mapping "626262" "241";
        Hashtbl.add color_mapping "6C6C6C" "242";
        Hashtbl.add color_mapping "767676" "243";
        Hashtbl.add color_mapping "808080" "244";
        Hashtbl.add color_mapping "8A8A8A" "245";
        Hashtbl.add color_mapping "949494" "246";
        Hashtbl.add color_mapping "9E9E9E" "247";
        Hashtbl.add color_mapping "A8A8A8" "248";
        Hashtbl.add color_mapping "B2B2B2" "249";
        Hashtbl.add color_mapping "BCBCBC" "250";
        Hashtbl.add color_mapping "C6C6C6" "251";
        Hashtbl.add color_mapping "D0D0D0" "252";
        Hashtbl.add color_mapping "DADADA" "253";
        Hashtbl.add color_mapping "E4E4E4" "254";
        Hashtbl.add color_mapping "EEEEEE" "255"

    (* Converts a 0-255 color value to an approximate hex code *)
    let to_color c =
        if c < 60 then
            "00"
        else if c < 115 then
            "5F"
        else if c < 155 then
            "87"
        else if c < 195 then
            "AF"
        else if c < 235 then
            "D7"
        else
            "FF"

    (* Converts a 0-255 grayscale value to an approximate hex code *)
    let to_grayscale c =
        if c < 8 then
            "08"
        else if c < 18 then
            "12"
        else if c < 28 then
            "1C"
        else if c < 38 then
            "26"
        else if c < 48 then
            "30"
        else if c < 58 then
            "3A"
        else if c < 68 then
            "44"
        else if c < 78 then
            "4E"
        else if c < 88 then
            "58"
        else if c < 98 then
            "62"
        else if c < 108 then
            "6C"
        else if c < 118 then
            "76"
        else if c < 128 then
            "80"
        else if c < 138 then
            "8A"
        else if c < 148 then
            "94"
        else if c < 158 then
            "9E"
        else if c < 168 then
            "A8"
        else if c < 178 then
            "B2"
        else if c < 188 then
            "BC"
        else if c < 198 then
            "C6"
        else if c < 208 then
            "D0"
        else if c < 218 then
            "DA"
        else if c < 228 then
            "E4"
        else if c < 238 then
            "EE"
        else
            "FF"

    let is_grayscale r g b =
        let avg = (r + g + b) / 3 in
        abs (r - avg) < 20 && abs (g - avg) < 20 && abs (b - avg) < 20

    (* Gets an approximated hex value for an (r,g,b) color *)
    let get_hex r g b =
        if is_grayscale r g b then
            let avg = (r + g + b) / 3 in
            let hexcode = (to_grayscale avg) in
            hexcode ^ hexcode ^ hexcode
        else
            (to_color r) ^ (to_color g) ^ (to_color b)

    (* Converts a hex code to its xterm-256 representation *)
    let get_256 c =
        try
            Hashtbl.find color_mapping c
        with
            | Not_found -> failwith ("Could not find color: " ^ c)

    (* ANSI escape sequence for 256-color *)
    let get_ansi c text_only =
        if text_only then
            ""
        else
            "\x1B[38;5;" ^ c ^ "m" ^
            "\x1B[48;5;" ^ c ^ "m"

    let get_frame text_only desired_width desired_height =
        let frame_ptr = frame desired_width desired_height in
        let width = frame_width () in
        let height = frame_height () in
        let depth = frame_depth () in
        let frame_array = from_ptr frame_ptr (width * height * depth) in
        let c2i = coordinate_to_index (height, width, depth) in
        let get_int col row dep =
            get frame_array (c2i (col, row, dep)) |> Unsigned.UChar.to_int in
        let size = height * width in
        let buf = FastString.create size in
        let colors = Array.make size "" in
        let idx = ref 0 in
        for row = 0 to height - 1 do
            for col = 0 to width - 1 do
                let b = get_int col row 0 in
                let g = get_int col row 1 in
                let r = get_int col row 2 in
                let avg = (r + g + b) / 3 in
                FastString.append_char buf (ascii_of_uchar avg);
                if not text_only then
                    Array.set colors !idx (get_hex r g b |> get_256)
                else
                    ();
                incr idx
            done;
        done;
        { data = buf
        ; colors = colors
        ; width = width
        ; height = height
        ; text_only = text_only}

    (* Colorizes pixels using the given array of colors for each pixel. The
     * length of pixels and colors are given by [size] *)
    let colorize force_text_only {data; colors; width; height; text_only} =
        let grid = Array.make_matrix height width "" in
        let c2i r c = r * width + c in
        let last_row = height - 1
        and last_col = width - 1 in
        (* Avoid duplicate color control sequences *)
        let last_color = ref "" in
        for row = 0 to last_row do
            for col = 0 to last_col do
                let idx = c2i row col in
                let color = Array.get colors idx
                and pixel = FastString.get data idx in
                if color = !last_color then
                    grid.(row).(col) <- (string_of_char pixel)
                else
                    begin
                        last_color := color;
                        grid.(row).(col) <-
                            (get_ansi color (text_only || force_text_only)) ^
                            (string_of_char pixel);
                    end
            done;
            grid.(row).(last_col) <- grid.(row).(last_col) ^ ansi_reset;
            last_color := ""
        done;
        grid
end
