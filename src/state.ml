open Messaging
open Package
open Cv

(* Identity of the current user *)
let current_user = ref ""

(* Text buffer is stored as a string *)
let input_buffer = ref ""

(* Update text buffer when a keypress occurs *)
let update_input_buffer c =
    input_buffer := !input_buffer ^ (String.make 1 c)

(* Delete last character from input_buffer *)
let delete_input_buffer () =
    let buffer_length = String.length !input_buffer in
    if buffer_length > 0 then
        input_buffer := String.sub !input_buffer 0 (buffer_length - 1)
    else ()

(* Maps user identities to the last package received for that user *)
let package_mapping = Hashtbl.create 1

let init_state curr_user =
    let dummy_image = { data = (FastString.of_string "")
                      ; colors = [|""|]
                      ; width = 0
                      ; height = 0
                      ; text_only  = true
                      } in
    current_user := curr_user;
    pack dummy_image "" 0 |> Hashtbl.add package_mapping curr_user

(* Updates user package with text message from the input box buffer, and posts
 * contents of text buffer to the history buffer in the messaging module for
 * logging *)
let log_message () =
    (* Update package in package mapping and post the updated package to
    the messaging module *)
    if Hashtbl.mem package_mapping !current_user then
        let (image, _, _) =
            Hashtbl.find package_mapping !current_user |> unpack in
        let new_package = pack image !input_buffer (get_timestamp ()) in
        Hashtbl.replace package_mapping !current_user new_package;
        refresh_history_buffer !current_user new_package;
        input_buffer := ""
    else ()

(* Updates user package the new image received *)
let log_image new_image =
    if Hashtbl.mem package_mapping !current_user then
        let (_, text, _) =
            Hashtbl.find package_mapping !current_user |> unpack in
        let new_package = pack new_image text (get_timestamp ()) in
        Hashtbl.replace package_mapping !current_user new_package;
    else ()

(* Refreshes package received *)
let refresh_package user package =
    (* Add package to mapping if package is associated with a new user, and
    check if the timestamp of the package is newer than the timestamp of the
    received package, and update the package in the mapping with the newer
    package *)
    let (_, _, new_timestamp) = unpack package in
    if Hashtbl.mem package_mapping user then
        let (_, _, timestamp) =
            Hashtbl.find package_mapping user |> unpack in
        if new_timestamp > timestamp then
            Hashtbl.replace package_mapping user package
        else ()
    else
        Hashtbl.add package_mapping user package

let get_num_users () = Hashtbl.length package_mapping

let get_input_buffer_contents () = !input_buffer

let get_input_buffer_length () = String.length !input_buffer

let get_packages () =
    Hashtbl.fold (fun k v acc -> v::acc) package_mapping []

