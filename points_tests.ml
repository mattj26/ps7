(*Tests for the point class*)
open Points;;
open CS51;;

let p1 = new point 1. 4.
let p2 = new point (-(2.)) 5.

let pp s =
  print_endline s

let ver funs vals =
  assert (List.fold_left2 (fun b f v -> (f = v) && b) true funs vals)

let nf x =
  float_of_int (-x)

let test_x () =
  ver [p1#x; p2#x] [1.; float_of_int (-2)];
  pp "Test x passed."

let test_y () =
  ver [p1#y; p2#y] [4. ; 5.];
  pp "Test y passed."

let test_pos () =
  ver [p1#pos; p2#pos] [(1., 4.); -(2.), 5.];
  pp "Test pos passed."

let test_move () =
  p1#move (new point 2. 1.);
  p2#move (new point (nf 3) 0.);
  ver [p1#x; p1#y; p2#x; p2#y] [3.; 5.; -(5.); 5.];
  pp "Test move passed"

let _ =
  test_x ();
  test_y ();
  test_pos ();
  test_move ()


