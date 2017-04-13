(*
                          Graphical objects
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

open Graphics ;;
open Printf ;;

open Points ;;

(*......................................................................
  Constants -- for configuring the graphical output
 *)

(* Linewidth to use for edges and object borders*)
let cLINEWIDTH = 3 ;;

(* Some useful colors. See color type in Graphics module. *)
let cBLACK = 0x000000 ;;
let cDARKGRAY = 0x555555 ;;
let cGRAY = 0x999999 ;;
let cLIGHTGRAY = 0xCCCCCC ;;

(*......................................................................
  Utility functions
 *)

(* draw_text_centered s (x, y) -- Draws the text in string s such that
   its center is at the position x, y *)
let draw_text_centered (s : string) (x, y : int * int) =
  let w, h = text_size s in
  moveto (x - w/2) (y - h/2);
  draw_string s ;;

(* draw_triangle pt dir scale -- Draws a filled triangle pointing at
   pt in the direction given by dir with a size base on dir scaled by
   scale. *)
let draw_triangle (pt : point) (dir : point) (scale : float) : unit =
  let ortho = let x, y = dir#pos in new point y (-.x) in
  let left = (pt#minus (dir#scale scale))#plus (ortho#scale (scale /. 2.0)) in
  let right = (pt#minus (dir#scale scale))#plus (ortho#scale (-. scale /. 2.0)) in
  fill_poly [| pt#round; left#round; right#round |] ;;

(* Minimum and maximum elements in a list *)
let minimum = CS51.reduce min ;;
let maximum = CS51.reduce max ;;

(*......................................................................
  Graphical objects

Graphical objects are objects that can be drawn on the canvas relative
to certain anchor points, which have typically been located based on
their connection to a mass in a mass-spring system.

Drawable objects have a color, a label, and a layer; subclasses may
have additional elements.

The layer indicates the drawing order, with lower-numbered layers
"further away than" (potentially obscured by) higher numbered
layers. By default, nodes are at layer 20 (up front), with edges at
layer 10 behind them, and zone boxes at layer 0 even further back.
 *)

class virtual drawable ?(label : string = "")
                       ?(layer : int = 0)
                       (col : color) =
  object
    val color : color = col
    val label : string = label
    method layer = layer
    method virtual draw : unit
  end

(* Here's an example graphical object -- a circle drawn at a specified
   point. We've provided the code for this one. You'll do the
   others. *)

(* Class circle -- nodes depicted with a small circle
   Arguments: ?label : string    optional label for the node (default: "")
              ?col : color       color to  draw the circle (black)
              ?layer : int       layer index; see above (20)
              ?textcol : color   color to draw the label (red)
              ?linewidth : int   width to draw the circle (cLINEWIDTH)
              m : point          center of the circle
              r : int            radius of the circle
 *)
class circle ?(label : string = "")
             ?(col : color = black)
             ?(layer : int = 20)
             ?(textcol : color = red)
             ?(linewidth : int = cLINEWIDTH)
             (m : point)
             (r : int) =
  object
    inherit drawable ~label ~layer col
    val anchor : point = m
    val radius : int = r
    val textcolor : color = textcol
    val linewidth : int = linewidth

    method draw =
      let (x, y) as p = anchor#round in
      set_line_width linewidth;
      set_color background;
      fill_circle x y radius;
      set_color color;
      draw_circle x y radius;
      set_color textcolor;
      draw_text_centered label p
  end


(*......................................................................
  Your part goes here: Provide the implementations of the rectangle,
  square, and edge classes.
......................................................................*)

(* Class rectangle -- nodes depicted with a small rectangle
   Arguments: ?label : string    optional label for the node (default: "")
              ?col : color       color to  draw the rectangle (black)
              ?layer : int       layer index (20)
              ?textcol : color   color to draw the label (red)
              ?width : int       width to draw the rectangle (cLINEWIDTH)
              m : point          center of the rectangle
              w : int            width of the rectangle
              h : int            height of the rectangle
 *)
 class rectangle ?(label : string = "")
                 ?(col : color = black)
                 ?(layer: int = 20)
                 ?(textcol : color = red)
                 ?(linew : int = cLINEWIDTH)
                 (m : point)
                 (w : int)
                 (h : int) =
  object
    inherit drawable ~label ~layer col
    val anchor : point = m
    val textcolor : color = textcol
    val linewidth : int = linew
    val width : int = w
    val height : int = h

    method draw =
      let x, y = anchor#round in
      let x0, y0 = (x - width /2 - linewidth / 2), (y - height/2 - linewidth / 2) in
      set_line_width linewidth;
      set_color background;
      fill_rect x0 y0 width height;
      set_color col;
      draw_rect x0 y0 width height;
      set_color textcol;
      draw_text_centered label (x, y)
  end


(* Class square -- nodes depicted with a small square
   Arguments: ?label : string    optional label for the node (default: "")
              ?col : color       color to  draw the square (black)
              ?layer : int       layer index (20)
              ?textcol : color   color to draw the label (red)
              ?linewidth : int   width to draw the square (cLINEWIDTH)
              m : point          center of the square
              w : int            width of the square
 *)

class square ?(label : string = "")
                 ?(col : color = black)
                 ?(layer: int = 20)
                 ?(textcol : color = red)
                 ?(linew : int = cLINEWIDTH)
                 (m : point)
                 (l : int)
                 =
  object
    inherit rectangle ~label ~col ~layer ~textcol ~linew m l l
  end



(* Class edge -- an edge between two points
   Arguments: ?label : string    optional label for the edge (default: "")
              ?col : color       color to  draw the square (black)
              ?layer : int       layer index (20)
              ?textcol : color   color to draw the label (red)
              ?width : int       width to draw the square (cLINEWIDTH)
              source : point     source point of the edge
              target : point     target point of the edge
 *)
class edge ?(label : string = "")
           ?(col : color = black)
           ?(layer : int = 20)
           ?(textcol : color = red)
           ?(width : int = cLINEWIDTH)
           (source : point)
           (target : point)
           =
object
  inherit drawable ~label ~layer col
  method draw : unit =
    set_color col;
    set_line_width width;
    let (x1, y1), (x2, y2) = source#round, target#round in
    draw_poly_line (Array.of_list [(x1, y1); (x2, y2)]);
    set_color textcol;
    draw_text_centered label ((x1 + x2) / 2, (y1 + y2) / 2)
  end




(* Class zone -- a zone box that surrounds a set of points
   Arguments: ?label : string      optional label for the edge (default: "")
              ?col : color         color to  draw the square (black)
              ?layer : int         layer index (20)
              ?border : int        amount of whitespace to leave around the
                                   bounding box of the enclosed masses
              ?linewidth : int     linewidth to draw the square (cLINEWIDTH)
              points : point list  a list of points defining the zone to be enclosed
 *)
class zone ?(col : color = cLIGHTGRAY)
           ?(label : string = "")
           ?(border : int = 20)
           ?(layer : int = 0)
           ?(linewidth : int = cLINEWIDTH)
           (points : point list) =
  object
    inherit drawable ~label ~layer col
    method draw : unit =
      set_color col;
      set_line_width linewidth;
      let xmin, ymin, xmax, ymax =
        let open List in
        int_of_float (minimum (map (fun m -> fst m#pos) points)) - border,
        int_of_float (minimum (map (fun m -> snd m#pos) points)) - border,
        int_of_float (maximum (map (fun m -> fst m#pos) points)) + border,
        int_of_float (maximum (map (fun m -> snd m#pos) points)) + border in
      draw_rect xmin ymin (xmax - xmin) (ymax - ymin)
  end



