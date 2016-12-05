open Lwt
open Cv
open Package
open State
open Messaging

(* The number of panes in a window *)
type window =  One | Two | Three | Four

let max_cols = 165
let max_rows = 45

let grid = Array.make_matrix max_rows max_cols ""

let ansi_reset = "\x1B[0m"

(* Prints a given string in unbuffered mode *)
let print_unbuf s =
    Printf.printf "%s%!" s

(* Clears the terminal *)
let clear_screen () =
    print_unbuf "\x1B[2J"

(* Restores the cursor to the top-left of the terminal *)
let restore_cursor () =
    print_unbuf "\x1B[;H"

(* Draws the pane outlines given the layout *)
let outline window =
    match window with
        | One ->
            for r = 0 to (max_rows - 1) do
                for c = 0 to (max_cols - 1) do
                    begin
                        if r = 0 || r = (max_rows - 1) then
                            if c = 0 || c = (max_cols - 1) then
                                grid.(r).(c) <- "+"
                            else
                                grid.(r).(c) <- "-"
                        else if c = 0 || c = (max_cols - 1) then
                            if r <> 0 && r <> (max_rows - 1) then
                                grid.(r).(c) <- "|"
                            else ()
                        else if c = 106 then
                            grid.(r).(c) <- "|"
                        else if r = 41 then
                            if c > 106 then
                                grid.(r).(c) <- "_"
                            else
                                grid.(r).(c) <- " "
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done
        | Two ->
            for r = 0 to (max_rows - 1) do
                for c = 0 to (max_cols - 1) do
                    begin
                        if r = 0 || r = (max_rows - 1) then
                            if c = 0 || c = (max_cols - 1) then
                                grid.(r).(c) <- "+"
                            else
                                grid.(r).(c) <- "-"
                        else if c = 0 || c = (max_cols - 1) then
                            if r <> 0 && r <> (max_rows - 1) then
                                grid.(r).(c) <- "|"
                            else ()
                        else if c = 81 || c = 82 then
                            if r < 33 then
                                grid.(r).(c) <- "|"
                            else if r = 33 then
                                grid.(r).(c) <- "+"
                            else if r = 42 then
                                grid.(r).(c) <- "_"
                            else
                                grid.(r).(c) <- " "
                        else if r = 33 then
                            grid.(r).(c) <- "-"
                        else if r = 42 then
                            grid.(r).(c) <- "_"
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done
        | Three ->
            for r = 0 to (max_rows - 1) do
                for c = 0 to (max_cols - 1) do
                    begin
                        if r = 0 || r = (max_rows - 1) then
                            if c = 0 || c = (max_cols - 1) then
                                grid.(r).(c) <- "+"
                            else
                                grid.(r).(c) <- "-"
                        else if c = 0 || c = (max_cols - 1) then
                            if r <> 0 && r <> (max_rows - 1) then
                                grid.(r).(c) <- "|"
                            else ()
                        else if c = 54 || c = 55 || c = 109 || c = 110 then
                            if r < 22 then
                                grid.(r).(c) <- "|"
                            else if r = 22 then
                                grid.(r).(c) <- "+"
                            else if r = 42 then
                                grid.(r).(c) <- "_"
                            else
                                grid.(r).(c) <- " "
                        else if r = 22 then
                            grid.(r).(c) <- "-"
                        else if r = 42 then
                            grid.(r).(c) <- "_"
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done
        | Four ->
            for r = 0 to (max_rows - 1) do
                for c = 0 to (max_cols - 1) do
                    begin
                        if r = 0 || r = (max_rows - 1) then
                            if c = 0 || c = (max_cols - 1) then
                                grid.(r).(c) <- "+"
                            else
                                grid.(r).(c) <- "-"
                        else if c = 0 || c = (max_cols - 1) then
                            if r <> 0 && r <> (max_rows - 1) then
                                grid.(r).(c) <- "|"
                            else ()
                        else if c = 54 || c = 55 || c = 109 || c = 110 then
                            if r <> 22 then
                                grid.(r).(c) <- "|"
                            else
                                grid.(r).(c) <- "+"
                        else if r = 22 then
                            if c < 109 then
                                grid.(r).(c) <- "-"
                            else
                                grid.(r).(c) <- " "
                        else if r = 41 then
                            if c > 110 then
                                grid.(r).(c) <- "_"
                            else
                                grid.(r).(c) <- " "
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done

(* Copies a string array array into the grid at the given start position, and
 * having the given maximum dimensions *)
let copy_to_grid (start_row, start_col) (cols, rows) g =
    let g_rows = min ((Array.length g) - 1) (rows - 1) in
    if g_rows < 0 then
        ()
    else
        let g_cols = min ((Array.length g.(0)) - 1) (cols - 1) in
        if start_row + g_rows >= max_rows || start_col + g_cols >= max_cols then
            failwith "copy_to_grid overflow!"
        else
            for r = 0 to g_rows do
                for c = 0 to g_cols do
                    begin
                        grid.(start_row + r).(start_col + c) <- g.(r).(c);
                        if c = g_cols then
                            grid.(start_row + r).(start_col + c) <-
                            grid.(start_row + r).(start_col + c) ^ ansi_reset
                        else ();
                    end
                done
            done

(* Copies a string into the grid at the given start position, and having the
 * given maximum dimensions *)
let print_to_grid (start_row, start_col) (max_width, max_height) s =
    (* NOTE: We are assuming that the provided string has no format characters
     * other than newlines *)
    if start_row + max_height >= max_rows
    || start_col + max_width >= max_cols then
        failwith "print_to_grid overflow!"
    else
        let r = ref start_row
        and c = ref start_col in
        for i = 0 to (String.length s - 1) do
            let sub = String.sub s i 1 in
            if sub <> "\n" then
                begin
                    grid.(!r).(!c) <- sub;
                    incr c;
                end
            else ();
            if !c - start_col >= max_width || sub = "\n" then
                begin
                    for j = !c to start_col + max_width - 1 do
                        grid.(!r).(j) <- " "
                    done;
                    c := start_col;
                    incr r
                end
            else ();
            if !r - start_row >= max_height then
                r := start_row
            else ();
        done

(* Prints the grid out *)
let print_grid () =
    Array.map (fun xs -> Array.to_list xs |> String.concat "") grid
    |> Array.to_list |> String.concat "\n"
    |> print_unbuf

(* Returns the expected image dimensions for a given window layout *)
let image_dimensions window = match window with
    | One -> (105, 43)
    | Two -> (80, 32)
    | Three -> (53, 21)
    | Four -> (53, 21)

(* Returns the expected text dimensions for a given window layout *)
let text_dimensions window = match window with
    | One -> (57, 40)
    | Two -> (163, 8)
    | Three -> (163, 19)
    | Four -> (53, 40)

(* Returns the expected input box dimensions for a given window layout *)
let input_dimensions window = match window with
    | One -> (57, 2)
    | Two -> (163, 1)
    | Three -> (163, 1)
    | Four -> (53, 2)

(* Returns the top-left coordinate for the given pane in the given window layout
 * The (n+1)th pane in the `n` layout is the chat box, and the (n+2)th pane is
 * the input box. All other panes are for user images.
 *)
let pane_start_coord pane window = match (pane, window) with
    | 1, One -> (1, 1)
    | 2, One -> (1, 107)
    | 3, One -> (42, 107)
    | 1, Two -> (1, 1)
    | 2, Two -> (1, 83)
    | 3, Two -> (34, 1)
    | 4, Two -> (43, 1)
    | 1, Three -> (1, 1)
    | 2, Three -> (1, 56)
    | 3, Three -> (1, 111)
    | 4, Three -> (23, 1)
    | 5, Three -> (43, 1)
    | 1, Four -> (1, 1)
    | 2, Four -> (1, 56)
    | 3, Four -> (23, 1)
    | 4, Four -> (23, 56)
    | 5, Four -> (1, 111)
    | 6, Four -> (42, 111)
    | _ -> failwith "Invalid pane number for window layout!"

(* Returns the window layout associated with the given number of users *)
let layout_for_num_users n =
    match n with
        | 1 -> One
        | 2 -> Two
        | 3 -> Three
        | 4 -> Four
        | _ -> failwith "More than 4 users is unsupported"

let last_layout = ref One

(* Determines which layout to use based on the number of connections, assigns
 * a pane number to each user, and uses the messaging module to render the
 * conversation history *)
let render text_only =
    let num_users = get_num_users () in
    let layout = layout_for_num_users num_users
    and packages = get_packages ()
    and chat_history = chat_history_to_string ()
    and input_buffer = get_input_buffer_contents () in
    let render_image idx package =
        try
            let (image, _, _, _) = unpack package in
            image |>
            Cv.colorize text_only |>
            copy_to_grid (pane_start_coord idx layout) (image_dimensions layout)
        with
            | _ -> () in
    if layout <> !last_layout then
        begin
            clear_screen ();
            last_layout := layout
        end
    else ();
    outline layout;
    print_to_grid (pane_start_coord (num_users + 1) layout)
        (text_dimensions layout) chat_history;
    print_to_grid (pane_start_coord (num_users + 2) layout)
        (input_dimensions layout) input_buffer;
    List.iteri (fun idx package -> render_image (idx + 1) package) packages;
    print_grid ()

