(* This is code to generate a force-directed graph which displays
    the starting lineups for the Major League Baseball game on
    4/12/17 between the Mets and the Phillies. *)

open List ;;

open Points ;;
open Masses ;;
open Controls ;;
open Graphobj ;;
open Graphdraw ;;


type graph =
    { labels : string list; (* labels, one for each node *)
      positions : (int * int) list; (* initial x, y positions of reach node *)
      edges : (int * int) list (* edges between nodes given as pairs of node indices *)
    } ;;

(* graph g -- Generates and solves a force-directed graph layout for
   the given graph g. *)

let graph ({labels; positions; edges} : graph) : unit =

  let masses = positions |> map (fun (x, y) ->
                                 new mass (float_of_int x) (float_of_int y) 1.) in

  let scene = masses |> mapi (fun i m ->
                              new square ~label: (nth labels i)
                                  ~col: cGRAY
                                  ~textcol: cBLACK
                                  (m :> point) 20) in

  let edgeobjs = edges |> map (fun (i, j) ->
                               new edge ((nth masses i) :> point)
                                   ~col: cLIGHTGRAY
                                   ((nth masses j) :> point)) in

  let constraints =
    let centerpoint = new point (float_of_int cFRAMESIZE/.2.) (float_of_int cFRAMESIZE/.2.) in
    flatten (flatten (mapi (fun i mi ->
                            mapi (fun j mj ->
                                  if i = j && (i = 0 || i = 1) then
                                    (* pull all nodes toward center *)
                                    [((new positionspring
                                                       ~stiffness: (CS51.const 0.7)
                                                       mi centerpoint) :> control)]
                                  else if mem (i, j) edges then
                                    (* connected nodes should be close *)
                                    [((new bispring ~rest:80.
                                           ~stiffness: (CS51.const (1. /. (float_of_int (length masses))))
                                           mi mj) :> control)]
                                  else if i < j then
                                    (* nodes shouldn't overlap *)
                                    [((new logisticrepel ~rest: 200.
                                           ~stiffness: (CS51.const 1.0)
                                           ~steepness: 0.5
                                           mi mj) :> control)]
                                  else [])
                                 masses)
                           masses))
  in
  x11_solve masses (constraints :> control list) (scene @ edgeobjs) ;;

let pi = 4.0 *. atan 1.0;;

let make_float_list min max =
  List.map (fun x -> float_of_int x) (CS51.range min max)

let circ_points num x0 y0 r =
  let nums = make_float_list 0 (num - 1) in
  let angle = 2. *. pi /. float_of_int num in
  List.map (fun n -> (int_of_float(x0 +. r *. cos (n *. angle)),
    int_of_float (y0 +. r *. sin (n *. angle)))) nums

let print_list lst =
  List.iter (fun (x, y) -> Printf.printf "(%i, %i)\n" x y) lst

let team_starts = [(10,10); (1,1)]

let mets_phils () =
  graph { labels = ["Mets"; "Phillies"; "Conforto"; "Cabrera"; "Cespedes";
                    "Bruce"; "Walker"; "Duda"; "Reyes"; "d'Arnaud"; "Wheeler";
                    "Hernandez"; "Kendrick"; "Herrera"; "Franco"; "Saunders";
                    "Joseph"; "Rupp"; "Galvis"; "Velasquez"];
          positions = team_starts @ (circ_points 9 100. 100. 4.) @ (circ_points 9 10. 10. 6.);

          edges = [(0,2); (0,3); (0,4); (0,5); (0,6); (0,7); (0,8); (0,9); (0,10);
                    (1, 11); (1, 12); (1, 13); (1, 14); (1, 15); (1, 16);
                    (1, 17); (1, 18); (1, 19)]
}

let _ =
  mets_phils ();;






