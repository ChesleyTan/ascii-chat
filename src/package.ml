open Cv

type package = { image: image
               ; text: string
               ; image_timestamp: int (* Time since epoch in milliseconds *)
               ; text_timestamp: int (* Time since epoch in milliseconds *)
               }

(* AES encryption key *)
let encryption_key = ref ""

(* Returns a timestamp in milliseconds since epoch *)
let get_timestamp () = Unix.gettimeofday () *. 1000. |> int_of_float

let pack image text image_timestamp text_timestamp =
    {image; text; image_timestamp; text_timestamp}

let unpack {image; text; image_timestamp; text_timestamp} =
    (image, text, image_timestamp, text_timestamp)

(* Serializes a data package into a JSON string *)
let serialize {image; text; image_timestamp; text_timestamp} =
    let open Yojson.Basic in
    let {data; colors; width; height; text_only} = image in
    let data_string = FastString.to_string data
    and colors_json_list = Array.(map (fun x -> `String x) colors |> to_list) in
    `Assoc [ ("image",
              `Assoc [ ("data", `String data_string)
                  ; ("colors", `List colors_json_list)
                  ; ("width", `Int width)
                  ; ("height", `Int height)
                  ; ("text_only", `Bool text_only)
                  ])
           ; ("text", `String text)
           ; ("image_timestamp", `Int image_timestamp)
           ; ("text_timestamp", `Int text_timestamp)
           ] |> to_string

(* Deserializes a JSON string to a data package *)
let deserialize json_string =
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string json_string in
    let image_json = json |> member "image"
    and text_json = json |> member "text"
    and image_timestamp_json = json |> member "image_timestamp"
    and text_timestamp_json = json |> member "text_timestamp" in
    let data = image_json |> member "data" |> to_string |> FastString.of_string
    and colors = image_json |> member "colors" |>
                 to_list |> Array.of_list |> Array.map to_string
    and width = image_json |> member "width" |> to_int
    and height = image_json |> member "height" |> to_int
    and text_only = image_json |> member "text_only" |> to_bool
    and text = text_json |> to_string
    and image_timestamp = image_timestamp_json |> to_int
    and text_timestamp = text_timestamp_json |> to_int
    in { image = { data
                 ; colors
                 ; width
                 ; height
                 ; text_only
                 }
       ; text = text
       ; image_timestamp = image_timestamp
       ; text_timestamp = text_timestamp
       }

(* Returns the encryption key *)
let get_encryption_key () = !encryption_key

(* Sets the encryption key *)
let set_encryption_key key = encryption_key := key

(* Encrypts a plaintext using AES encryption and stored encryption key *)
let encrypt json_string =
    let compressor = Cryptokit.Zlib.compress ()
    and aes = new Cryptokit.Block.aes_encrypt (get_encryption_key ()) in
    let transform = new Cryptokit.Block.cipher_padded_encrypt
    Cryptokit.Padding.length aes |> Cryptokit.compose compressor in
    transform#put_string json_string;
    transform#finish;
    transform#get_string

(* Decrypts a plaintext using AES encryption and stored encryption key *)
let decrypt ciphertext =
    let uncompressor = Cryptokit.Zlib.uncompress ()
    and aes = new Cryptokit.Block.aes_decrypt (get_encryption_key ()) in
    let transform =
        Cryptokit.compose
        (new Cryptokit.Block.cipher_padded_decrypt Cryptokit.Padding.length aes)
        uncompressor in
    transform#put_string ciphertext;
    transform#finish;
    transform#get_string

(* Generates an encryption key for the AES block cipher using SHA256 *)
let generate_encryption_key key =
    let hash = Cryptokit.Hash.sha256 () in
    hash#add_string key;
    encryption_key := hash#result

(* Returns whether the user has set the encryption key *)
let is_encryption_key_set () = !encryption_key <> ""
