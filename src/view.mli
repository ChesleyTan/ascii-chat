open Lwt
open LTerm_widget

type window =  One | Two | Three | Four

val redraw: window -> LTerm.t -> unit

