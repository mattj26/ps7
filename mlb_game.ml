open Masses;;
open Points;;

type player =
{
  home : bool;
  name : string;
  hits : int list
}
let mets = new point 1. 1.
let phills = new point 2. 2.
let teamPoints = [mets; phills]

let metsMass = new mass mets#x mets#y 1.
let phillsMass = new mass phills#x phills#y 1.

let center = new point (float_of_int cFRAMESIZE /. 2.) (float_of_int cFRAMESIZE /. 2.)



let mets1 = {home = false; name = "Conforto"; hits = [1; 4]}
