open Ctypes
open Foreign
let foo = foreign "foo" (void @-> returning void)
let _ = foo ()
