open Package
open Cv

(* History buffer stored as a list *)
let history_buffer: string list ref = ref []

(* Hashtable for keeping track of timestamp of last messages for each user *)
let message_mapping = Hashtbl.create 1

(* Take user identity (ip address + port), a message, and a timestamp as input,
 * and populate an internal buffer that represents the accumulated chat
 * history, which should only be updated when a new message is received*)
let add_to_history_buffer user package  =
    let (_, text, _, _) = unpack package in
    history_buffer :=
        !history_buffer @
            [ user
            ^ ": "
            ^ text
            ]

(* Refreshes the history buffer *)
let refresh_history_buffer user package =
    let (_, _, _, text_timestamp) = unpack package in
        if Hashtbl.mem message_mapping user
        then
            if Hashtbl.find message_mapping user < text_timestamp then
                begin
                    Hashtbl.replace message_mapping user text_timestamp;
                    add_to_history_buffer user package
                end
            else ()
        else
            begin
                Hashtbl.add message_mapping user text_timestamp;
                add_to_history_buffer user package
            end

(*Calling to_string should return a string of the chat history*)
let chat_history_to_string () = String.concat "\n" !history_buffer
