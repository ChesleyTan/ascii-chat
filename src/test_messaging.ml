open OUnit2
open Cv
open Package
open Messaging

(* Testing variables *)
let user1 = "98.139.180.149:10"
let user2 = "98.139.180.150:20"
let user3 = "98.139.180.151:30"

let image1 = Cv.get_frame true 100 100
let image2 = Cv.get_frame false 50 200

let text1 = " "
let text2 = "O Fortuna"
let text3 = "velut luna"
let text4 = "statu variabilis"
let text5 = "semper crescis"
let text6 = "aut decrescis;"
let text7 = "vita detestabilis"
let text8 = "nunc obdurat"
let text9 = "et tunc curat"

let package1 = Package.pack image1 text1 10 10
let package2 = Package.pack image2 text2 11 11
let package3 = Package.pack image1 text1 9 9
let package4 = Package.pack image2 text3 12 12
let package5 = Package.pack image1 text4 15 15
let package6 = Package.pack image2 text5 8 8
let package7 = Package.pack image1 text6 13 13
let package8 = Package.pack image2 text7 17 17
let package9 = Package.pack image1 text8 14 14
let package10 = Package.pack image2 text9 18 18

(* Testing helper functions*)
let assert_int = assert_equal ~printer: string_of_int
let assert_char = assert_equal ~printer: (fun c -> String.make 1 c)
let assert_string = assert_equal ~printer: (fun x -> x)

(* Messaging tests
 * Key: np = a package never received before from the user
 *      op = a package that has been received from the user before
 *      nt = a package that as a new timestamp 
 *      ot = a package that has an old timestamp
 *)
let tests = [
    "np/nt/user1/package1/test1"  >:: (fun _ -> assert_string "98.139.180.149:10:  " 
        (Messaging.refresh_history_buffer user1 package1;
        Messaging.chat_history_to_string ()));
    "op/ot/user1/package1/test2"  >:: (fun _ -> assert_string "98.139.180.149:10:  " 
        (Messaging.refresh_history_buffer user1 package1;
        Messaging.chat_history_to_string ()));
    "np/nt/user1/package2/test3"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna" 
        (Messaging.refresh_history_buffer user1 package2;
        Messaging.chat_history_to_string ()));
    "op/ot/user1/package2/test4"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna" 
        (Messaging.refresh_history_buffer user1 package2;
        Messaging.chat_history_to_string ()));
    "np/ot/user1/package3/test5"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna" 
        (Messaging.refresh_history_buffer user1 package3;
        Messaging.chat_history_to_string ()));
    "op/ot/user1/package3/test6"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna" 
        (Messaging.refresh_history_buffer user1 package3;
        Messaging.chat_history_to_string ()));
    "np/nt/user1/package4/test7"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna" 
        (Messaging.refresh_history_buffer user1 package4;
        Messaging.chat_history_to_string ()));
    "np/nt/user3/package5/test8"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis" 
        (Messaging.refresh_history_buffer user3 package5;
        Messaging.chat_history_to_string ()));
    "np/nt/user2/package6/test9"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis" 
        (Messaging.refresh_history_buffer user2 package6;
        Messaging.chat_history_to_string ()));
    "np/nt/user1/package7/test10"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;" 
        (Messaging.refresh_history_buffer user1 package7;
        Messaging.chat_history_to_string ()));
    "np/ot/user3/package7/test11"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;" 
        (Messaging.refresh_history_buffer user3 package7;
        Messaging.chat_history_to_string ()));
    "op/ot/user1/package7/test12"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;" 
        (Messaging.refresh_history_buffer user1 package7;
        Messaging.chat_history_to_string ()));
    "np/nt/user1/package8/test13"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;\n98.139.180.149:10: vita detestabilis" 
        (Messaging.refresh_history_buffer user1 package8;
        Messaging.chat_history_to_string ()));
    "np/ot/user2/package9/test14"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;\n98.139.180.149:10: vita detestabilis" 
        (Messaging.refresh_history_buffer user3 package9;
        Messaging.chat_history_to_string ()));
    "np/nt/user3/package9/test15"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;\n98.139.180.149:10: vita detestabilis\n98.139.180.150:20: nunc obdurat" 
        (Messaging.refresh_history_buffer user2 package9;
        Messaging.chat_history_to_string ()));
    "op/ot/user3/package9/test16"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;\n98.139.180.149:10: vita detestabilis\n98.139.180.150:20: nunc obdurat" 
        (Messaging.refresh_history_buffer user2 package9;
        Messaging.chat_history_to_string ()));
    "np/nt/user3/package9/test17"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;\n98.139.180.149:10: vita detestabilis\n98.139.180.150:20: nunc obdurat\n98.139.180.149:10: et tunc curat" 
        (Messaging.refresh_history_buffer user1 package10;
        Messaging.chat_history_to_string ()));
    "op/ot/user3/package9/test18"  >:: (fun _ -> assert_string "98.139.180.149:10:  \n98.139.180.149:10: O Fortuna\n98.139.180.149:10: velut luna\n98.139.180.151:30: statu variabilis\n98.139.180.150:20: semper crescis\n98.139.180.149:10: aut decrescis;\n98.139.180.149:10: vita detestabilis\n98.139.180.150:20: nunc obdurat\n98.139.180.149:10: et tunc curat" 
        (Messaging.refresh_history_buffer user1 package10;
        Messaging.chat_history_to_string ()));
]
