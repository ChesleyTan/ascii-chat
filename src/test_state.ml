open OUnit2
open Cv
open Package
open State
open Messaging

(* Testing Variables *)
let user1 = "98.139.180.149:10"
let user2 = "98.139.180.150:20"
let user3 = "98.139.180.151:30"

(* Testing helper functions*)
let assert_int = assert_equal ~printer: string_of_int
let assert_char = assert_equal ~printer: (fun c -> String.make 1 c)
let assert_string = assert_equal ~printer: (fun x -> x)

let tests = [
(* Test updating/deleting text buffer and buffer length *)
    "update1"  >:: (fun _ -> assert_string "O" 
        (State.update_input_buffer 'O';
        State.get_input_buffer_contents ()));
    "length1"  >:: (fun _ -> assert_int 1 
        (State.get_input_buffer_length ()));
    "update2"  >:: (fun _ -> assert_string "O " 
        (State.update_input_buffer ' ';
        State.get_input_buffer_contents ()));
    "length2"  >:: (fun _ -> assert_int 2
        (State.get_input_buffer_length ()));
    "update3"  >:: (fun _ -> assert_string "O F" 
        (State.update_input_buffer ' '; State.delete_input_buffer (); 
        State.update_input_buffer 'F';
        State.get_input_buffer_contents ()));
    "length3"  >:: (fun _ -> assert_int 3
        (State.get_input_buffer_length ()));
    "update4"  >:: (fun _ -> assert_string "O F\na"
        (State.update_input_buffer '\n'; State.update_input_buffer 'a';
        State.get_input_buffer_contents ()));
    "length4"  >:: (fun _ -> assert_int 5
        (State.get_input_buffer_length ()));

(* Test messaging logging *)
    "log1"  >:: (fun _ -> assert_string ""
        (Messaging.clear_history_buffer ();
        Messaging.chat_history_to_string ()));
    "log2"  >:: (fun _ -> assert_string ""
        (State.init_state user1; Package.generate_encryption_key "key";
        State.log_message (); State.get_input_buffer_contents ()));
    "log3"  >:: (fun _ -> assert_string "98.139.180.149:10: O F\na"
        (Messaging.chat_history_to_string ()));

(*Test state helper functions*)
    "get_num_users1"  >:: (fun _ -> assert_int 2
        (State.init_state user2; State.get_num_users ()));
    "get_num_users/delete_user2"  >:: (fun _ -> assert_int 0
        (State.delete_user user1; State.delete_user user2;
        State.get_num_users ()));
    "get_num_users/delete_user3"  >:: (fun _ -> assert_int 2
        (State.init_state user2; State.init_state user3;
        State.get_num_users ()))
]
