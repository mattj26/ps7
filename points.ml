(*
                Mutable points with vector arithmetic
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)

class point (x0 : float) (y0 : float) =
object (this)
  val mutable xSave = x0
  val mutable ySave = y0
  method x = xSave
  method y = ySave
  method pos = (x0, y0)
  method round = (int_of_float (xSave +. 0.5), int_of_float (ySave +. 0.5))
  method move (p1 : point) = xSave <- p1#x; ySave <- p1#y

  method scale s = new point (xSave *. s) (ySave *. s)
  method plus (p1 : point) = new point (xSave +. p1#x) (ySave +. p1#y)
  method minus (p1 : point) = new point (xSave -. p1#x) (ySave -. p1#y)
  method private vec_dif (p2 : point) = (xSave -. p2#x) ** 2. +. (ySave -. p2#y) ** 2.
  method norm = sqrt (this#vec_dif (new point 0. 0.))
  method distance p1 = sqrt(this#vec_dif p1)
  method unit_vector = this#scale (1. /. this#norm)

  (******
   ****** Your implementation of the point class goes here. Make sure
   ****** to see points.mli for the signature and documentation of the
   ****** class.
   ******)

end



(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) each part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 0 ;;
