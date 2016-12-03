open Messaging
open Package
open View

(* Text buffer is stored as a string *)
let text_buffer = ref ""

(* Update text buffer when a keypress occurs *)
let update_text_buffer c =
    text_buffer := !text_buffer ^ (String.make 1 c)

(* Delete last character from text_buffer *)
let delete_text_buffer () = 
    let buffer_length = String.length !text_buffer in
    if buffer_length > 0 then
        text_buffer := String.sub !text_buffer 0 (buffer_length - 1)
    else ()

(* TODO: Determine size hashtable should be initialized at.
 * Maps user identities to the last package received for that user *)
let package_mapping = Hashtbl.create 1

(* Posts contents of text buffer to the history buffer in the 
 * messaging module for logging *)
let log_message () =
    (* TODO update package in package mapping and post the updated package to
    the messaging module *)
    (* TODO the state module should hold the identity of the current user *)
    text_buffer := "";
    failwith "unimplemented"

(* Refreshes package received *)
let refresh_package user package = 
    (* TODO Add package to mapping if package is associated with a new user, and
    check if the timestamp of the package is newer than the timestamp of the
    received package, and update the package in the mapping with the newer
    package *)
    failwith "unimplemented"

(* Determines which layout to use based on the number of connections, assigns 
 * a pane number to each user, and uses the messaging module to render the 
 * conversation history*)
let render () =
    (* TODO render images *)
    if Hashtbl.length package_mapping = 1 then
        outline One
    else if Hashtbl.length package_mapping = 2 then
        outline Two
    else if Hashtbl.length package_mapping = 3 then
        outline Three
    else
        outline Four
