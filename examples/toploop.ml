
#require "archimedes";;

let p = Plot.Array.make "graphics hold" 400. 400.;;
Plot.Array.f p (fun x -> x *. x) (-1.) 1.;;

Plot.Array.close p;;
