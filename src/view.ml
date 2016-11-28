open Lwt
open LTerm_widget
open LTerm_text

(*
let hbox = new hbox in
let frame = new frame in
let text = new
*)

(*The number of panes in a window*)
type window =  One | Two | Three | Four

(*let draw_one term = *)

let outline window term =
    let term_size = LTerm.size term in
    let term_width = LTerm_geom.cols term_size in
    let term_height = LTerm_geom.rows term_size in
    let term_mat = LTerm_draw.make_matrix term_size in
    let term_context = LTerm_draw.context term_mat term_size in
        match window with
            | One -> LTerm_draw.draw_string term_context 0 0 "hello world"
            | Two -> LTerm_draw.draw_string term_context 0 0 "hello world"
            | Three ->  LTerm_draw.draw_string term_context 0 0 "hello world"
            | Four -> LTerm_draw.draw_string term_context 0 0 "hello world"

let redraw window term = match window with
    | One -> outline window term
    | Two -> outline window term
    | Three -> outline window term
    | Four -> outline window term

