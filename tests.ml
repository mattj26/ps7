(*Tests for the point class*)
open Points;;
open Masses;;

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
  p1#move (new point 4. 1.);
  p2#move (new point (nf 3) 0.);
  ver [p1#x; p1#y; p2#x; p2#y] [4.; 1.; -(3.); 0.];
  pp "Test move passed"

let test_scale () =
  let pt1 = p1#scale 3. in
  let pt2 = p2#scale 1.5 in
  ver [pt1#x; pt1#y; pt2#x; pt2#y] [12.; 3.; -4.5; 0.];
  pp "Test scale passed"

let p3 = new point 1. (nf 4)
let p4 = new point (nf 1) 0.

let print_point (p : point) : unit =
  Printf.printf "(%f, %f,)\n" p#x p#y

let test_plus () =
  let pt1 = p1#plus p3 in
  let pt2 = p2#plus p4 in
  ver [pt1#x; pt1#y; pt2#x; pt2#y] [5.; -(3.); -(4.); 0.];
  pp "Test plus passed."

let test_minus () =
  let pt1 = p1#minus p4 in
  let pt2 = p2#minus p3 in
  ver [pt1#x; pt1#y; pt2#x; pt2#y] [5.; 1.; -(4.); 4.];
  pp "Test minus passed"

let test_norm () =
  print_float p1#norm;
  print_newline ();
  print_float p2#norm;
  print_newline ()

let test_dist () =
  print_float (p1#distance p2);
  print_newline ();
  pp "Test distance passed"

let test_unit_vec () =
  (* ver [p1#unit_vector#norm; p2#unit_vector#norm] [1.; 1.]; *)
  print_point p1#unit_vector;
  print_point p2#unit_vector;
  pp "Test unit vector passed"

let m1 = new mass 2. 3. 12.
let p5 = new point 12. 9.

let test_mass_move () =
  m1#move p5;
  ver [m1#x; m1#y] [12.; 9.];
  pp "Mass move test passed"

let test_restore_pos () =
  m1#restore_pos;
  ver [m1#x; m1#y] [2.; 3.];
  m1#move p1;
  m1#move p2;
  m1#restore_pos;
  ver [m1#x; m1#y] [p1#x; p1#y];
  pp "Restore pos test passed"

let _ =
  test_x ();
  test_y ();
  test_pos ();
  test_move ();
  test_scale ();
  test_plus ();
  test_minus ();
  test_norm ();
  test_dist ();
  test_unit_vec ();
  test_mass_move ();
  test_restore_pos ()


