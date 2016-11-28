open Cv

type package = {image: image}

let pack image = {image}

let unpack {image} = image

let serialize {image} =
    let open Yojson.Basic in
    let {data; colors; width; height; text_only} = image in
    let data_string = FastString.to_string data
    and colors_json_list = Array.(map (fun x -> `String x) colors |> to_list) in
    `Assoc [ ("data", `String data_string)
           ; ("colors", `List colors_json_list)
           ; ("width", `Int width)
           ; ("height", `Int height)
           ; ("text_only", `Bool text_only)
           ] |> to_string

let deserialize json_string =
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string json_string in
    let data = json |> member "data" |> to_string |> FastString.of_string
    and colors = json |> member "colors" |>
                 to_list |> Array.of_list |> Array.map to_string
    and width = json |> member "width" |> to_int
    and height = json |> member "height" |> to_int
    and text_only = json |> member "text_only" |> to_bool
    in { image = { data
                 ; colors
                 ; width
                 ; height
                 ; text_only
                 }
       }

