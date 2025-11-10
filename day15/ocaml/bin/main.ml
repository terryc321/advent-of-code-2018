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
  
 *)

let rec moveGoblin g e x1 y1 =
  let elfs = allElfXY g
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
  and moveGoblin2 g e x1 y1 elfs = ()
;;








