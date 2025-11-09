open Base;;

let () = print_endline "Hello, World!";;

let arr = Array.make_matrix 32 32 "W";;
let arr2 = Array.make_matrix 32 32  "P";;
let arr3 = Array.make_matrix ~dimx: 32  ~dimy: 32  "Z";;

let row0 = arr3.(0);;
let row1 = arr3.(1);;

let _ = Array.set arr3.(0) 0 "Q";;
let _ = Array.set arr3.(31) 31 "LAST";;
(* cannot see answer as it is elided *)

(* Array.iter *)

let () =
  Stdio.print_endline "Hello from Base!";   (* same as print_endline *)
  Stdio.printf "Pi ≈ %.2f\n" 3.14159;
  Stdio.eprintf "Warning: %s\n" "something went wrong";;

let a = 3 ;;



