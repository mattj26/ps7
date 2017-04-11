(*
		       Point masses with forces
			  CS51 Problem Set 7
			 -.-. ... ..... .----
 *)

open Points

val cFRAMESIZE : int

(* Class: mass
     Arguments: initx : float
                inity : float      initial x and y positions of the mass
		            m : float          the "physical" mass of the mass for computing
		    		                        force effects

   A mass is a point that has a mass associated with it and can have
   forces act upon it. *)

class mass : float -> float -> float -> object
  (* A mass is located at a point *)
  inherit point

  (* Masses have distinct integer ids *)
  method get_id : int

  (* The mass itself *)
  val mass : float

  (* Moves mass to provided point, storing previous position for
     possible later restoring, clipping to frame boundary while we're
     at it. *)
  method move : point -> unit

  (* Restore position of mass to the single previous stored
     position. Multiple restores are not supported. *)
  method restore_pos : unit

  (* Forces on the mass *)
  (* Sets force on the mass to the vector given by point *)
  method set_force : point -> unit
  (* Resets the force on the mass to 0 *)
  method reset_force : unit
  (* Returns the force on the mass *)
  method get_force : point
  (* Adds to the existing force on the mass a force specified as the
     given point vector *)
  method add_force : point -> unit
  (* Applies the current force to the mass, moving it accordingly, and
     resetting the force to 0 *)
  method apply_force : unit
  (* Scales the current force by the provided multiplicative factor *)
  method scale_force : float -> unit

  (* I/O methods *)
  (* Draws an indicator of the mass location on the canvas *)
  method reveal : unit
  (* Prints a textual description of the object on stdout *)
  method describe : unit
end
