(*Empty history buffer stored as a list*)
let history_buffer = ref [""]

(*Hashtable for keeping track of last messages for each user*)
let message_mapping = Hashtbl.create 1

(*Take user identity (ip address + port), a message, and a timestamp as input, and populate an internal buffer that represents the accumulated chat
 * history, which should only be updated when a new message is received*)
let add_to_buffer user message timestamp = 
    history_buffer := !history_buffer @ [user] @ [message] @ [timestamp]; ()

(*Refreshes the history buffer*)
let refresh_buffer user message timestamp = 
    if Hashtbl.mem message_mapping user && Hashtbl.find
        message_mapping user = message^timestamp
    then ()
    else let _ = Hashtbl.replace message_mapping user (message^timestamp) in 
        add_to_buffer user message timestamp

(*Calling to_string should return a string of the chat history*)
let to_string = List.fold_left (fun accum str -> accum ^ str) "" !history_buffer  
