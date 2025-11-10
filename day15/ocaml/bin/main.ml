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

(* forget everything about efficiency and focus on correctness - minimize ways this thing can screw up
   int integer small 64 bit value

#######   
#.G...#   G(200)
#...EG#   E(200), G(200)
#.#.#G#   G(200)
#..G#E#   G(200), E(200)
#.....#   
#######

either goblin elf wall or empty slot
goblin / elf has hit health = 200 and power = 3

grid g

occupied g x y = is position x y occupied ?
enemy g p x y  = is there an enemy to player p at x y ?

transition g -> g

for some given grid g , must be a transition to next g state g

   
 *)

let g = [| [| '#' ; '#' ; '#' ; '#' ; '#' ; '#' ; '#'|] ;
           [| '#' ; '.' ; 'G' ; '.' ; '.' ; '.' ; '#'|] ;
           [| '#' ; '.' ; '.' ; '.' ; 'E' ; 'G' ; '#'|] ;
           [| '#' ; '.' ; '#' ; '.' ; '#' ; 'G' ; '#'|] ;
           [| '#' ; '.' ; '.' ; 'G' ; '#' ; 'E' ; '#'|] ;
           [| '#' ; '.' ; '.' ; '.' ; '.' ; '.' ; '#'|] ;
           [| '#' ; '#' ; '#' ; '#' ; '#' ; '#' ; '#'|] |] ;;




                       
  

(* this array access is g.(Y).(X) format
   [|  ... |] declares an array with seperator ; semicolon
   each entry needs to be same type
   gob()
   elf()

   g matrix is (Y=0..7).(X=0..6)
   7 wide
   8 high
   switch X Y
   zero based
   g.(Y:0..7).(X:0..6)

   g.(0).(0);;
   g.(7).(6);; 



g.(0).(0);;

g.(7).(6);; 

 we can change entry into matrix using Y then X index 
Array.set g.(7) 6 'Q' ;;

g ;;
 *)

(* although goblin/elf has hits strength and power , we also give each goblin/elf an id ,
   so we can tell if its the goblin we are interested in
   also a bool if that goblin/elf has moved in this transition

   Goblin hits power id true
   
   true means can move and attack
   false means has already moved/ attacked this round
 *)

type gp = Goblin of int * int * int * bool
        | Elf of int * int * int * bool
        | Wall
        | Cave
        | Val of int ;;

Val 5;;
Wall ;; 

(* let rec fac n = *)
(* let f x = *)

(* convert char grid to proper game grid *)

let g2 = Array.make_matrix ~dimx: 7  ~dimy: 8  Wall;;

(* module Bool

   &&  and
   ||  or

   onboard g x y
   -1 should be (-1)
   onboard g (-1) (-2);; => false
   
 *)

let onboard g x y = let hgt = Array.length g
                    and wid = Array.length g.(0)
                    in x >= 0 && x < wid && y >= 0 && y < hgt ;;


(* show the grid *)

(* let rec show *)

(* convert the grid
let g = [| [| '#' ; '#' ; '#' ; '#' ; '#' ; '#' ; '#'|] ;
           [| '#' ; '.' ; 'G' ; '.' ; '.' ; '.' ; '#'|] ;
           [| '#' ; '.' ; 'G' ; '.' ; 'E' ; 'G' ; '#'|] ;
           [| '#' ; '.' ; 'G' ; '.' ; '.' ; 'G' ; '#'|] ;
           [| '#' ; '.' ; 'G' ; '.' ; 'G' ; 'E' ; '#'|] ;
           [| '#' ; '.' ; 'G' ; '.' ; '.' ; '.' ; '#'|] ;
           [| '#' ; '.' ; 'G' ; '.' ; '.' ; '.' ; '#'|] ;
           [| '#' ; '#' ; '#' ; '#' ; '#' ; '#' ; '#'|] |] ;;
g 7wide 8high           
let g4 = Array.make_matrix ~dimx: 7 ~dimy: 8 Wall ;;
g4.(7).(6) => error because wide is only 0..6 , high only 0..7
Array matrix uses X coord Y coord
whereas visual presentation of g above uses Y coord X coord
highly confusing
           
 *)

module Fifo =
    struct
      type 'a queue = { front: 'a list; rear: 'a list }
      let make front rear =
        match front with
        | [] -> { front = List.rev rear; rear = [] }
        | _  -> { front; rear }
      let empty = { front = []; rear = [] }
      let is_empty = function { front = []; _ } -> true | _ -> false
      let add x q = make q.front (x :: q.rear)
      exception Empty
      let top = function
        | { front = []; _ } -> raise Empty
        | { front = x :: _; _ } -> x
      let pop = function
        | { front = []; _ } -> raise Empty
        | { front = _ :: f; rear = r } -> make f r
    end;;

Fifo.empty;;

Fifo.add 'a' Fifo.empty;;

Fifo.add 'b' (Fifo.add 'a' Fifo.empty);;

Fifo.make [1;2;3] [4;5;6] ;; 

let cnvGrid g = let hgt = Array.length g
                and wid = Array.length g.(0)
                in let g2 = Array.make_matrix ~dimx: wid  ~dimy: hgt Wall
                   in let rec cnvGridRec x y =
                      if y >= hgt then g2
                      else if x >= wid then cnvGridRec 0 (y + 1)
                      else let v = gpCnv g.(y).(x)
                           in Array.set g2.(x) y v; (* really confusing *)
                              cnvGridRec (x + 1) y
                    and gpCnv = function
                      | '#' -> Wall
                      | '.' -> Cave 
                      | 'G' -> Goblin (200, 3, 1, true) 
                      | 'E' -> Elf (200,3,1, true)
                      | _ -> Cave 
                    in cnvGridRec 0 0;;



let g5 = Array.make_matrix ~dimx: 3 ~dimy: 4 'a';;
Array.length g5 ;;
Array.length g5.(0) ;;

let g4 = cnvGrid g;;

(* make a simpler grid type
   indexing from 1 to width
   indexing from 1 to height
   width property
   height property
   conversion routine from flat array of arrays
 *)

module Grid =
  struct
    type 'a grid = { data: 'a Base.Array.t Base.Array.t ; wid: int ; hgt: int }
    let make w h z = { data = Array.make_matrix ~dimx: w ~dimy: h z ; wid = w ; hgt = h }
    let get g x y = g.data.(x - 1).(y - 1)
    let set g x y z = Array.set g.data.(x - 1) (y - 1) z
    let width g = g.wid
    let height g = g.hgt
    let onboard g x y = x >= 1 && x <= g.wid && y >= 1 && y <= g.hgt 
    (* show routine when converted ? *)
    let convert g = let hgt = Array.length g
                    and wid = Array.length g.(0)
                    and id = let n = ref 1
                             in fun () -> n := !n + 1 ; !n 
                    in let g2 = { data = Array.make_matrix ~dimx: wid ~dimy: hgt Wall ; wid = wid ; hgt = hgt }
                       in let rec cnvGridRec x y =
                            (* Stdio.printf "converting %d %d\n" x y ;   *)
                            if y >= hgt then g2
                            else if x >= wid then cnvGridRec 0 (y + 1)
                            else let v = gpCnv g.(y).(x)
                                 in Array.set g2.data.(x) (y) v ; 
                                    cnvGridRec (x + 1) y
                          and gpCnv = function
                            | '#' -> Wall
                            | '.' -> Cave 
                            | 'G' -> Goblin (200, 3, id() , true) 
                            | 'E' -> Elf (200,3,id(), true)
                            | _ -> Cave 
                          in cnvGridRec 0 0;;
    let show g = let hgt = g.hgt
                 and wid = g.wid
                 in let rec showGridRec x y =
                      (* Stdio.printf "showing %d %d\n" x y ;  *)
                      if y >= hgt then ()
                      else if x >= wid then showGridRec 0 (y + 1)
                      else if x = (wid - 1) then (gpShow g.data.(x).(y) ; (* end of line *)
                                                  Stdio.printf "\n";
                                                  showGridRec 0 (y + 1))
                      else (gpShow g.data.(x).(y) ;
                            (* Stdio.printf " "; *)
                            showGridRec (x + 1) y)
                    and gpShow = function
                      | Val n -> Stdio.print_string (Printf.sprintf "{%2d}" n)
                      | Wall -> Stdio.print_string "   #"
                      | Cave -> Stdio.print_string "   ."
                      | Goblin _ -> Stdio.print_string "   G"
                      | Elf _ -> Stdio.print_string "   E"
                    in showGridRec 0 0;;
    let show2 g = let hgt = g.hgt
                 and wid = g.wid
                 in let rec showGridRec x y =
                      (* Stdio.printf "showing %d %d\n" x y ;  *)
                      if y >= hgt then ()
                      else if x >= wid then showGridRec 0 (y + 1)
                      else if x = (wid - 1) then (gpShow g.data.(x).(y) ; (* end of line *)
                                                  Stdio.printf "\n";
                                                  showGridRec 0 (y + 1))
                      else (gpShow g.data.(x).(y) ;
                            (* Stdio.printf " "; *)
                            showGridRec (x + 1) y)
                    and gpShow = function
                      | Val n -> Stdio.print_string (Printf.sprintf "{%3d}" n)
                      | Wall -> Stdio.print_string "   #  "
                      | Cave -> Stdio.print_string "   .  "
                      | Goblin (h,_,_,_) -> Stdio.printf" G%3d " h
                      | Elf (h,_,_,_) -> Stdio.printf " E%3d " h
                    in showGridRec 0 0;;
  end;;


(*
let g5 = Grid.make 2 3 'a';;
Grid.width g5;;
Grid.height g5;;
Grid.set g5 1 1 'A';;
Grid.set g5 2 2 'B';;
Grid.set g5 2 3 'C';;
Grid.get g5 1 1 ;;
Grid.get g5 2 2 ;;
Grid.get g5 2 3 ;;
g5;;
 *)

let g6 = Grid.convert g;;

(*
  traverse grid and find first goblin that has true as active property
  in order to be consistent , return Wall instead of goblin or elf
  option types 
  Some 3 ;;
  None ;; 
 *)
let gridTraverse g f = let h = Grid.height g
                   and w = Grid.width g
                   in let rec trav x y =
                        if y > h then ()
                        else if x > w then trav 1 (y + 1)
                        else let gval = Grid.get g x y
                             in f gval ;
                                trav (x + 1) y
                      in trav 1 1 ;;

let gridTraverseXY g f = let h = Grid.height g
                         and w = Grid.width g
                         in let rec trav x y =
                              if y > h then ()
                              else if x > w then trav 1 (y + 1)
                              else let gval = Grid.get g x y
                                   in f x y gval ;
                                      trav (x + 1) y
                            in trav 1 1 ;;



(* collect active goblins -- side effects localised to routine -- is it still functional ? *)
let allGoblinXY g = let gobs = ref []
                     in let fn x y z =
                          match z with 
                     | (Goblin (h ,p ,id ,a)) -> gobs := ((Goblin (h, p ,id, a)),x,y) :: !gobs
                     | _ -> ()
                   in gridTraverseXY g fn ;
                      !gobs ;;

let allElfXY g = let gobs = ref []
                 in let fn x y z =
                      match z with 
                      | (Elf (h ,p ,id ,a)) -> gobs := ((Elf (h, p ,id, a)),x,y) :: !gobs
                      | _ -> ()
                    in gridTraverseXY g fn ;
                       !gobs ;;

let allGoblins g = let gobs = ref []
                in let fn = function
                     | (Goblin (h ,p ,id ,a)) -> gobs := (Goblin (h, p ,id, a)) :: !gobs
                     | _ -> ()
                   in gridTraverse g fn ;
                      !gobs ;;

let allElfs g = let gobs = ref []
                in let fn = function
                     | (Elf (h,p,id,a)) -> gobs := (Elf (h,p,id,a)) :: !gobs
                     | _ -> ()
                   in gridTraverse g fn ;
                      !gobs ;;
                        


let activeGoblins g = let gobs = ref []
                in let fn = function
                     | Goblin (hits,power,id, true) -> gobs := (Goblin (hits,power,id, true)) :: !gobs
                     | _ -> ()
                   in gridTraverse g fn ;
                      !gobs ;;

let activeElfs g = let gobs = ref []
                in let fn = function
                     | Elf (hits,power,id, true) -> gobs := (Elf (hits,power,id, true)) :: !gobs
                     | _ -> ()
                   in gridTraverse g fn ;
                      !gobs ;;

let inActiveGoblins g = let gobs = ref []
                in let fn = function
                     | Goblin (hits,power,id, false) -> gobs := (Goblin (hits,power,id, false)) :: !gobs
                     | _ -> ()
                   in gridTraverse g fn ;
                      !gobs ;;

let inActiveElfs g = let gobs = ref []
                in let fn = function
                     | Elf (hits,power,id, false) -> gobs := (Elf (hits,power,id, false)) :: !gobs
                     | _ -> ()
                   in gridTraverse g fn ;
                      !gobs ;;

(* number empty squares -- making a new grid in process so do not contaminate original grid ??
   need to run several breadth first searches 

   Grid.onboard g6 (-1) (-2)   is square -1 , -2 on the board ?? no
   grid indexs from 1 .. width , 1 .. height
   Grid .get g x y
   Grid .set g x y z 
   
 *)

(* copy everything *)
let gridCopy g = let wid = Grid.width g
                 and hgt = Grid.height g
                 in let g2 = Grid.make wid hgt (Grid.get g 1 1)
                    in  let f x y z = Grid.set g2 x y z ; ()
                        in gridTraverseXY g f ; 
                           g2 ;;


(* copy all but Val _ replaced with Caves *)
let gridCopyNoVals g = let wid = Grid.width g
                       and hgt = Grid.height g
                       in let g2 = Grid.make wid hgt Wall
                          in  let f x y z =
                                match z with
                                | Val _  -> Grid.set g2 x y (Cave) ; ()
                                | _      -> Grid.set g2 x y z ; ()
                              in gridTraverseXY g f ;
                                 g2 ;;


(* lexicographic order
   only pick up active goblins and elfs
   goblins and elfs that have had a move will not be picked up
 *)
let lexi g = let ord = ref []
             in let f x y z =
                  match z with
                  | Elf (hits,power,id, true) -> ord := ( (Elf (hits,power,id, true)) , x , y)  :: !ord ; ()
                  | Goblin (hits,power,id, true) -> ord := ( (Goblin (hits,power,id, true)) , x , y)  :: !ord ; ()
                  | _      -> ()
                in gridTraverseXY g f ;
                   List.rev !ord ;;



(* try flood fill on target itself , just does nothing since cannot pass through elf or goblin
   but if we put distance 1 around elf/goblin it should then flood fill any empty / reachable areas 
 *)
let flood g2 p q = let g = gridCopyNoVals g2
                   in let rec recur x y n =
                        (* Stdio.printf "flooding square %d %d\n" x y ;                         *)
                        if Grid.onboard g x y then
                          let e = Grid.get g x y
                          in match e with
                             | Val n2 -> if n2 <= n then ()
                                         else
                                           let () = Grid.set g x y (Val n)
                                           in
                                           recur (x - 1) y (n + 1) ;
                                           recur (x + 1) y (n + 1) ;
                                           recur x (y - 1) (n + 1) ;
                                           recur x (y + 1) (n + 1) ;
                                           ()
                             | Cave -> let () = Grid.set g x y (Val n)
                                       in
                                       recur (x - 1) y (n + 1) ;
                                       recur (x + 1) y (n + 1) ;
                                       recur x (y - 1) (n + 1) ;
                                       recur x (y + 1) (n + 1) ;
                                       ()
                             | _ -> ()
                        else ()
                      in recur (p - 1) q 1;
                         recur (p + 1) q 1;
                         recur p (q + 1) 1;
                         recur p (q - 1) 1;
                         g ;;

                                        
(* lexi g6
[(Goblin (200, 3, 2, true), 3, 2); (Elf (200, 3, 3, true), 5, 3);
 (Goblin (200, 3, 4, true), 6, 3); (Goblin (200, 3, 5, true), 6, 4);
 (Goblin (200, 3, 6, true), 4, 5); (Elf (200, 3, 7, true), 6, 5)]

 Goblin ( hits, power , ID , true )
                              ^ still active ??


 lexi -> identify active entities

 {lexi g6}
 goblin / elf
 note if goblin or elf and the position x1 y1
 if goblin -> find all elfs active or not 
                for each elf at position x2 y2
                 compute ...  flood g x2 y2 ...
                 examine around location of x1 y1 which direction yields lowest value if any 
                 check & update global lowest values in each direction
                 
               e <X> e
               d  X  d
               c  b  c
                  a

 move vertically upward meet obstacle <x> is target , we can see two equal paths to reach 
                  
 *)

(* suppose we have an entity E {elf or goblin} and position x1 y1 
let move g e x1 y1 = match e with
  | Goblin (hits,power,id, true) -> moveGoblin g e x1 y1 (allElfs g)
  | Elf (hits,power,id, true) -> moveElf g e x1 y1 (allGoblins g)
  | _ -> ()
  and

    let left = Int.max_value
  and right = Int.max_value
  and down = Int.max_value
  and up = Int.max_value

 *)

(* horizontal or vertically distance by 1 step -- ie touching *)
let inrange x1 y1 x2 y2 =  (x1 = x2 && abs(y1 - y2) = 1) || (y1 = y2 && abs(x1 - x2) = 1);;

(*
  for some grid g 
  move goblin e at position x1 y1
  --- well , determine if any elfs within reach

  recursive case let rec moveGoblin
  other recursive definitions drop let
  also assumed to be recursive also 


  WHY DOES THIS NOT LOAD ?? 
  
  
 *)



(*
let topGoblin g e x1 y1 =
  let left = ref Int.max_value
  and right = ref Int.max_value
  and up = ref Int.max_value
  and down = ref Int.max_value  
  in let rec moveGoblin g e x1 y1 =  
       let rec elfs = allElfXY g
       and reach = ref []
       in let rec recur xs =
            match xs with 
            | [] -> ()
            | (h :: t) ->
               match h with
               | ((Elf(hits,power,id,active)),x2,y2) ->  if (inrange x1 y1 x2 y2) then
                                                           (reach := ((Elf(hits,power,id,active)),x2,y2) :: !reach ;
                                                            recur t)
                                                         else recur t
               | _ -> recur t 
          in recur elfs ;
             (* if any elfs in range - move directly to attackGoblin *)
             match !reach with
             | [] -> moveGoblin2 g e x1 y1 elfs
             | _ -> attackGoblin g e x1 y1 !reach
     and attackGoblin g e x1 y1 r = ()
     and moveGoblin2 g e x1 y1 elfs =
       let rec recur xs =
         match xs with 
         | [] -> ()
         | (h :: t) ->
            match h with
            | ((Elf(hits,power,id,active)),x2,y2) ->  let fg = flood g x2 y2  (* fg flooded grid *)
                                                      in record_left fg (x1 - 1) y1 ;
                                                         record_right fg (x1 + 1) y1 ;
                                                         record_up fg x1 (y1 - 1) ;
                                                         record_down fg x1 (y1 + 1) ;
                                                         recur t
            | _ -> recur t 
       in recur elfs
     and record_left fd x3 y3 =
       if Grid.onboard fd x3 y3 then (let z = Grid.get fd x3 y3 in
                                      match z with
                                      | Val n -> if n < !left then left := n ; ()
                                      | _ -> ())
       else ()
     and record_right fd x3 y3 =
       if Grid.onboard fd x3 y3 then (let z = Grid.get fd x3 y3 in
                                      match z with
                                      | Val n -> if n < !right then right := n ; ()
                                      | _ -> ())
       else ()
     and record_up fd x3 y3 =
       if Grid.onboard fd x3 y3 then (let z = Grid.get fd x3 y3 in
                                      match z with
                                      | Val n -> if n < !up then up := n ; ()
                                      | _ -> ())
       else ()
     and record_down fd x3 y3 =
       if Grid.onboard fd x3 y3 then (let z = Grid.get fd x3 y3 in
                                      match z with
                                      | Val n -> if n < !down then down := n ; ()
                                      | _ -> ())
       else ();;
 *)






(* for each elf position , flood on that position , from x1 y1 determine lowest scores
   then decide best direction to move in
 *)

(* let moveGoblin g x y = *)
  
(*
  are they phyiscally equal to each other
  one returns original grid
  another returns original grid filled with Values Val n depending on where elf is located
  
# phys_equal g6  (goblinFlood g6 1 1);;
- : Base.bool = true
# phys_equal g6  (goblinFlood g6 2 2);;
- : Base.bool = true
# phys_equal g6  (goblinFlood g6 3 2);;
- : Base.bool = true
# phys_equal g6  (goblinFlood g6 5 3);;
- : Base.bool = false
# phys_equal g6  (goblinFlood g6 5 3);;
- : Base.bool = false

 *)


(* have a goblin at x1 y1 - generate me a flood filled diagram given elf at x2 y2 -
   regardless if elf or goblin at that actual address
 *)

(* fg flooded grid *)
let goblinFlood g x y = 
  if Grid.onboard g x y then
    let e = Grid.get g x y
    in match e with
       | (Elf(hits,power,id,active)) ->  let fg = flood g x y 
                                         in fg
       | _ -> g            
  else g ;;

let isElf x =
  match x with
  | (Elf(hits,power,id,active)) -> true
  | _ -> false;;

let isGoblin x =
  match x with
  | (Goblin(hits,power,id,active)) -> true
  | _ -> false;;


(* for a given goblin does not matter where it is , only where elf is and elf flood fills the grid
   similarly for the elf
 *)
let goblinFloods g =
  let obs = ref []
  in let f x y z = match z with
       | (Elf(hits,power,id,active)) ->  let fg = flood g x y in (obs := fg :: !obs ; ())
       | _ -> ()
     in gridTraverseXY g f ;
        !obs;;

let elfFloods g =
  let obs = ref []
  in let f x y z = match z with
       | (Goblin(hits,power,id,active)) ->  let fg = flood g x y in (obs := fg :: !obs ; ())
       | _ -> ()
     in gridTraverseXY g f ;
        !obs;;

goblinFloods g6;;

elfFloods g6;;
(* flat list iterator needed not map - but use it *)

(* assume no elf around the goblin on g6 , goblin at 3 2 - figure out lowest left right up down *)
(* works well we get lowest value from list of floods by elfs  *)
(*

let moveGoblin g e x y =
  let left = ref Int.max_value
  and right = ref Int.max_value
  and up = ref Int.max_value
  and down = ref Int.max_value
  in let record g2 = let ok = Grid.onboard g2 (x - 1) y
                     in match ok with
                        | true -> (let elem = Grid.get g2 (x - 1) y
                                   in match elem with
                                      | Val v -> (if v < !left then (left := v ; ()))
                                      | _ -> ())
                        | _ -> ()         
     in  let ef = elfFloods g      
         in let _ = List.map ef ~f:record in left ;;
 *)


(* remove spurious Some None from a list and just return the actual elements that are there *)
let clean xs =
  let obs = ref []
  in let rec recur xs =
       match xs with
       | [] -> !obs
       | (Some x :: t) -> obs := x :: !obs ; recur t
       | (_ :: t) -> recur t
     in recur xs ;;

let secondCompare x y =
  match (snd x, snd y) with
  | (u,v) -> u < v ;;
  
type direction = Left | Right | Up | Down | Stay ;;

(* cannot just List.tl (List.tl xs) !!! not well typed !!! *)

(* compare v1 v2 used in List.sort returns an signed int !
   v1 < v2  ->  -1
   v1 = v2  ->  0
   v1 > v2  ->  +1
   How WEIRD ! 
 *)

let moveGoblin g e x y =
  let left = ref Int.max_value
  and right = ref Int.max_value
  and up = ref Int.max_value
  and down = ref Int.max_value
  in let leftfn g2 x y = let ok = Grid.onboard g2 (x - 1) y
                         in match ok with
                            | true -> (let elem = Grid.get g2 (x - 1) y
                                       in match elem with
                                          | Val v -> (if v < !left then (left := v ; ()))
                                          | _ -> ())
                            | _ -> ()
     and rightfn g2 x y = let ok = Grid.onboard g2 (x + 1) y
                          in match ok with
                             | true -> (let elem = Grid.get g2 (x + 1) y
                                        in match elem with
                                           | Val v -> (if v < !left then (right := v ; ()))
                                           | _ -> ())
                             | _ -> ()
     and upfn g2 x y = let ok = Grid.onboard g2 x (y - 1)
                       in match ok with
                          | true -> (let elem = Grid.get g2 x (y - 1)
                                     in match elem with
                                        | Val v -> (if v < !left then (up := v ; ()))
                                        | _ -> ())
                          | _ -> ()
     and downfn g2 x y = let ok = Grid.onboard g2 x (y + 1)
                         in match ok with
                            | true -> (let elem = Grid.get g2 x (y + 1)
                                       in match elem with
                                          | Val v -> (if v < !left then (down := v ; ()))
                                          | _ -> ())
                            | _ -> ()
     in let record g2 = leftfn g2 x y ;
                        rightfn g2 x y ;
                        upfn g2 x y ;
                        downfn g2 x y                         
        in  let ef = elfFloods g      
            in let _ = List.map ef ~f:record in
               let res = [(Up,!up) ; (Left , !left) ; (Right, !right) ; (Down,!down) ]
               in let lows = List.filter res ~f:(fun x -> match x with
                                                          | (s , v) -> if v = Int.max_value then false else true)
                  in let cdirs = List.sort lows (fun (_,v1) (_,v2) ->
                                     if v1 < v2 then -1
                                     else if v1= v2 then 0
                                     else 1)
                     in cdirs;;

let cdirs xs =
  match (List.hd xs) with
  | None -> Stay
  | Some (dir, _) -> dir ;;

     



(*
  given a goblin at e at x y on grid g
  find all elfs => ef
  flood fill each elf and record which direction on grid goblin should move to closest elf
  lowest score on generarted grids , leads to four directions having a score or Int.max_value
  filter out max_values
  left with list (possibly empty) of moves to make 
  
 *)


(*
  lexi g6 => [  (Entity , X , Y ) ; .... ] list of entities and position in 3 tuple
  will only pull out entries with TRUE true in active

  if entity is not in range then moveGoblin E X Y 

  entity in range ?
  
 *)

(* elf opponent in range  *)
let elf_oir g x y = 
  let obs = ref []
  in  if Grid.onboard g x y then
        let elem = Grid.get g (x - 1) y
        in match elem with
           | (Goblin(hits,power,id,active)) -> obs := (elem,x-1,y) :: !obs ; ()
           | _ -> ()
      else () ;
      if Grid.onboard g x y then
        let elem = Grid.get g (x + 1) y
        in match elem with
           | (Goblin(hits,power,id,active)) -> obs := (elem,x+1,y) :: !obs ; ()
           | _ -> ()
      else () ;
      if Grid.onboard g x y then
        let elem = Grid.get g x (y - 1)
        in match elem with
           | (Goblin(hits,power,id,active)) -> obs := (elem,x,y-1) :: !obs ; ()
           | _ -> ()
      else () ;
      if Grid.onboard g x y then
        let elem = Grid.get g x (y + 1)
        in match elem with
           | (Goblin(hits,power,id,active)) -> obs := (elem,x,y+1) :: !obs ; ()
           | _ -> ()
      else () ;
      match List.hd !obs with
      | None -> []
      | Some pr -> List.sort !obs (fun (Goblin(h,p,i,a),x,y) (Goblin(h2,p2,i2,a2),x2,y2) ->
                       if h < h2 then -1
                       else if h = h2 then 0
                       else 1) ;;
      
      
(* goblin opponent in range  *)
let goblin_oir g x y = 
  let obs = ref []
  in  if Grid.onboard g x y then
        let elem = Grid.get g (x - 1) y
        in match elem with
           | (Elf(hits,power,id,active)) -> obs := (elem,x-1,y) :: !obs ; ()
           | _ -> ()
      else () ;
      if Grid.onboard g x y then
        let elem = Grid.get g (x + 1) y
        in match elem with
           | (Elf(hits,power,id,active)) -> obs := (elem,x+1,y) :: !obs ; ()
           | _ -> ()
      else () ;
      if Grid.onboard g x y then
        let elem = Grid.get g x (y - 1)
        in match elem with
           | (Elf(hits,power,id,active)) -> obs := (elem,x,y-1) :: !obs ; ()
           | _ -> ()
      else () ;
      if Grid.onboard g x y then
        let elem = Grid.get g x (y + 1)
        in match elem with
           | (Elf(hits,power,id,active)) -> obs := (elem , x,y+1) :: !obs ; ()
           | _ -> ()
      else () ;
      match List.hd !obs with
      | None -> []
      | Some pr -> List.sort !obs (fun (Elf(h,p,i,a),x,y) (Elf(h2,p2,i2,a2),x2,y2) ->
                       if h < h2 then -1
                       else if h = h2 then 0
                       else 1) ;;
      
(*
  oir = opponent in range
  return a list of values , sorted by oppoenent
  oir needs to be fired on either elf or goblin , since wall cave val does not have enemy
 *)
let oir g x y =
  if Grid.onboard g x y then
    let player = Grid.get g x y
    in match player with
       | Elf(hits,power,id,active) -> elf_oir g x y
       | Goblin(hits,power,id,active) -> goblin_oir g x y
       | _ -> []
  else [] ;;


(* an attack - instigator + damaged
   1 copy grid
   2

   last = return new grid with changes applied

 *)
let attack g x y x2 y2 =
  let g2 = gridCopy g
  and alpha = Grid.get g x y
  and beta = Grid.get g x2 y2
  in match (alpha,beta) with
     | ((Goblin(h,p,i,a)), (Elf(h2,p2,i2,a2))) ->
        Grid.set g2 x y (Goblin(h,p,i,false)) ;
        let h3 = h2 - p
        in if h3 < 0 then Grid.set g2 x2 y2 Cave
           else Grid.set g2 x2 y2 (Elf(h2 - p,p2,i2,a2)) ;
           g2
     | ((Elf(h,p,i,a)), (Goblin(h2,p2,i2,a2))) ->
        Grid.set g2 x y (Elf(h,p,i,false)) ;
        let h3 = h2 - p
        in if h3 < 0 then Grid.set g2 x2 y2 Cave
           else Grid.set g2 x2 y2 (Elf(h2 - p,p2,i2,a2));
           g2 ;;
                  
(* as is a reserved word in ocaml , obviously *)

(* roundGoblin
   goblin been identified as active in lexicographic ordering
   asks opponent in range - if so - conduct attack , generate a new grid , kill elf if it died , replace with cave
   otherwise move the goblin closer to 
 *)

let moveLeft g x y =
  let g2 = gridCopy g
  and alpha = Grid.get g x y  
  in match alpha with
     | (Goblin(h,p,i,a)) -> Grid.set g2 (x - 1) y (Goblin(h,p,i,false)) ;
        Grid.set g2 x y Cave ;
        g2
     | (Elf(h,p,i,a)) -> Grid.set g2 (x - 1) y (Elf(h,p,i,false)) ;
        Grid.set g2 x y Cave ;
        g2 ;;
        
let moveRight g x y =
  let g2 = gridCopy g
  and alpha = Grid.get g x y  
  in match alpha with
     | (Goblin(h,p,i,a)) -> Grid.set g2 (x + 1) y (Goblin(h,p,i,false)) ;
        Grid.set g2 x y Cave ;
        g2
     | (Elf(h,p,i,a)) -> Grid.set g2 (x + 1) y (Elf(h,p,i,false)) ;
        Grid.set g2 x y Cave ;
        g2 ;;

let moveUp g x y =
  let g2 = gridCopy g
  and alpha = Grid.get g x y  
  in match alpha with
     | (Goblin(h,p,i,a)) -> Grid.set g2 x (y - 1) (Goblin(h,p,i,false)) ;
        Grid.set g2 x y Cave ;
        g2
     | (Elf(h,p,i,a)) -> Grid.set g2 x (y  - 1) (Elf(h,p,i,false)) ;
        Grid.set g2 x y Cave ;
        g2 ;;

let moveDown g x y =
  let g2 = gridCopy g
  and alpha = Grid.get g x y  
  in match alpha with
     | (Goblin(h,p,i,a)) -> Grid.set g2 x ( y + 1) (Goblin(h,p,i,false)) ;
        Grid.set g2 x y Cave ;
        g2
     | (Elf(h,p,i,a)) -> Grid.set g2 x (y + 1) (Elf(h,p,i,false)) ;
        Grid.set g2 x y Cave ;
        g2 ;;



let rec roundGoblin g e x y recur =
  (let myas = goblin_oir g x y
  in match List.hd myas with 
     | Some ((Elf (h,p,i,a)),x2,y2) -> let g2 = attack g x y x2 y2
                                       in recur g2
     | _ -> let mv = cdirs (moveGoblin g e x y)
            in match mv with
               | Stay -> g 
               | Left -> let g2 = moveLeft g x y  in g2 
               | Right -> let g2 = moveRight g x y in g2
               | Up -> let g2 = moveUp g x y in g2
               | Down -> let g2 = moveDown g x y in g2 : gp Grid.grid) ;;







(* lexi g6 *)

(*
let round g =
  let rec recur g = 
         
  and roundElf e x y =
    let atk = None in
    match atk with
    | Some g2 -> recur g2 (* proceed with attack happened and player who attacked is now active=false *)
    | None -> g
  in   let lex = lexi g
       in match lex with
          | [] -> g  (* no more active players - this g has all inactive players? *)
          |  h :: t  -> match h with
                        | (Goblin (hits,power,id,active),x,y) -> roundGoblin (Goblin (hits,power,id,active)) x y
                        | (Elf (hits,power,id,active),x,y) -> roundElf (Elf (hits,power,id,active)) x y
;;
 *)



(*


                             let opp = elf_oir g x y
                             in match opp with
                                | [] -> (* move + attack ? *) g
                                |  o :: t2  -> (* attack o ;
                                                   set this goblin to false ;
                                                   round changed g *)
                                                   g ;;

                                                   
  
                       let opp = goblin_oir g x y
                       in match opp with
                          | [] -> let dir = moveGoblin g e x y
                                  and newg = ref g
                                  in let _ = match dir with
                                       | Stay -> () 
                                       | Left -> newg := moveLeft g x y ; ()
                                       | Right -> newg := moveRight g x y ; ()
                                       | Up -> newg := moveUp g x y ; ()
                                       | Down -> newg := moveDown g x y ; ()
                                     in let g = !newg
                                        in g
                          |  o :: t2  -> (* attack o ;
                                          set this goblin to false ;
                                          round changed g *)
                             g


let ts = [1;2;3] in
    match ts with
    | [] -> true
    | h :: t -> false ;;

 *)
