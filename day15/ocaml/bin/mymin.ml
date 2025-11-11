(*

> example;;
- : piece array array =
[|[|Wall; Wall; Wall; Wall; Wall; Wall; Wall|];
  [|Wall; Cave; Cave; Cave; Cave; Cave; Wall|];
  [|Wall; Goblin (200, 2); Cave; Wall; Cave; Cave; Wall|];
  [|Wall; Cave; Cave; Cave; Goblin (200, 6); Cave; Wall|];
  [|Wall; Cave; Elf (200, 3); Wall; Wall; Cave; Wall|];
  [|Wall; Cave; Goblin (200, 4); Goblin (200, 5); Elf (200, 7); Cave; Wall|];
  [|Wall; Wall; Wall; Wall; Wall; Wall; Wall|]|]
  
> # show_grid example;;
#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######

- : unit = ()
> # lexi example;;
- : (piece * int * int) list =
[(Goblin (200, 2), 2, 1); (Elf (200, 3), 4, 2); (Goblin (200, 4), 5, 2);
 (Goblin (200, 5), 5, 3); (Goblin (200, 6), 3, 4); (Elf (200, 7), 5, 4)]

> # neighbour example 2 1;;
- : (piece * int * int) list =
[(Cave, 1, 1); (Cave, 3, 1); (Wall, 2, 0); (Cave, 2, 2)]
 
 *)



let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents 


(*
  goblin hit points change , power always 3
  likewise elf hit points change , power always 3
  
  goblin hits id
  elf hits id 
   
  where hits = health ie how much more damage can sustain before dies , when hits <= 0 
   
 *)
type piece = Wall
           | Cave
           | Goblin of int * int 
           | Elf of int * int
           | Val of int (* a hack to help flood fill *)

(* ;; make it a wall  *)
let make_grid x y = Array.init_matrix x y (fun x y -> Wall)


(* ;; sets x y to cave ! *)
(* ;; g.(6).(6) <- Cave ;; *)

(* reads a file  *)
let example () =
  let g = make_grid 7 7  
  and counter = ref 1
  in  let id () = counter := !counter + 1 ; !counter
      in let rec process_string_list xs n =
           match xs with
           | [] -> g 
           | ( s :: t ) -> let _ = process_string s n 
                           in process_string_list t (n + 1)
         and process_string s n = String.mapi (fun i c ->
                                      match c with
                                      | '#' -> g.(i).(n) <- Wall ; c
                                      | 'G' -> g.(i).(n) <- (Goblin(200,id())) ; c 
                                      | 'E' -> g.(i).(n) <- (Elf(200,id())) ; c 
                                      | '.' -> g.(i).(n) <- Cave ; c 
                                      | _ -> c) s  
         in process_string_list (read_lines "../../example.txt") 0


(* show the grid wid=7 hgt=7 *)
let show_grid g =
  let rec recur x y =
    if y >= 7 then (print_string "\n" ; ())
    else if x >= 7 then (print_string "\n" ; recur 0 (y + 1))
    else let e = g.(x).(y)
         and next () = recur (x + 1) y 
         in match e with
            | (Elf(hits,id)) -> print_string " E " ; next () 
            | (Goblin(hits,id)) -> print_string " G " ; next ()
            | Wall -> print_string " # " ; next ()
            | Cave -> print_string " . " ; next ()
            | (Val(n)) -> print_string " ";  print_string (string_of_int n) ; print_string " "  ; next ()
  in recur 0 0 



(* lexicographic order , cons-ing needs to reverse last step *)
let lexi g =
  let obs = ref []
  in  let rec recur x y =
        if y >= 7 then List.rev !obs
        else if x >= 7 then recur 0 (y + 1)
        else let e = g.(x).(y)
             in match e with
                | (Elf(hits,id)) -> obs := (e,x,y) :: !obs ; recur (x+1) y
                | (Goblin(hits,id)) -> obs := (e,x,y) :: !obs ; recur (x+1) y
                | _ -> recur (x+1) y
      in recur 0 0 


let onboard x y = x >= 0 && x < 7 && y >= 0 && y < 7

type direction = Up | Down | Left | Right | Stay 


let neighbour_left g x y = (let dx = x - 1 and dy = y in if onboard dx dy then [(g.(dx).(dy),dx,dy,Left)] else [])

let neighbour_right g x y = (let dx = x + 1 and dy = y in if onboard dx dy then [(g.(dx).(dy),dx,dy,Right)] else [])

let neighbour_up g x y = (let dx = x and dy = y - 1 in if onboard dx dy then [(g.(dx).(dy),dx,dy,Up)] else [])

let neighbour_down g x y = (let dx = x and dy = y + 1 in if onboard dx dy then [(g.(dx).(dy),dx,dy,Down)] else [])

let neighbour g x y =  (neighbour_left g x y) @
                        (neighbour_right g x y) @
                          (neighbour_up g x y) @
                            (neighbour_down g x y)

let isElf e =
  match e with
  | (Elf(hits,id)) -> true
  | _ -> false

let isGoblin e =
  match e with
  | (Elf(hits,id)) -> true
  | _ -> false


let show_piece s =
  match s with
  | Cave -> print_string "Cave ;"
  | Wall -> print_string "Wall ;"
  | (Goblin(hits,id)) -> print_string "Goblin{hits=" ; print_string (string_of_int hits) ; print_string "} ;"
  | (Elf(hits,id)) -> print_string "Elf{hits=" ; print_string (string_of_int hits) ; print_string "} ;"
  | (Val(n)) -> print_string "Val{n=" ; print_string (string_of_int n) ; print_string "} ;"


let show_piece_xy s =
  match s with
  | (Cave,x,y) -> print_string "Cave ;"
  | (Wall,x,y) -> print_string "Wall ;"
  | ((Goblin(hits,id)),x,y) -> print_string "Goblin{hits=" ;
                               print_string (string_of_int hits) ;
                               print_string " at " ;
                               print_string (string_of_int x) ;
                               print_string " , " ;
                               print_string (string_of_int y) ;                               
                               print_string "} ;"
  | ((Elf(hits,id)),x,y) -> print_string "Elf{hits=" ;
                            print_string (string_of_int hits) ;
                            print_string " at " ;
                            print_string (string_of_int x) ;
                            print_string " , " ;
                            print_string (string_of_int y) ;                               
                            print_string "} ;"
  | ((Val n),x,y) -> print_string "Val {n=" ;
                            print_string (string_of_int n) ;
                            print_string " at " ;
                            print_string (string_of_int x) ;
                            print_string " , " ;
                            print_string (string_of_int y) ;                               
                            print_string "} ;"


let show_neighbour s =
  match s with
  | (Cave,x,y,dir) -> print_string "Cave ;"
  | (Wall,x,y,dir) -> print_string "Wall ;"
  | ((Goblin(hits,id)),x,y,dir) -> print_string "Goblin{hits=" ; print_string (string_of_int hits) ; print_string "} ;"
  | ((Elf(hits,id)),x,y,dir) -> print_string "Elf{hits=" ; print_string (string_of_int hits) ; print_string "} ;"
  | ((Val(n)),x,y,dir) -> print_string "Val{n=" ; print_string (string_of_int n) ; print_string "} ;"

exception AttackExceptionWall

exception AttackExceptionCave

exception AttackExceptionVal

(* mutates the grid g !?! *)
let attack g x y x2 y2 =
  let t = g.(x2).(y2)
  in match t with
     | Cave -> raise AttackExceptionCave
     | Wall -> raise AttackExceptionWall 
     | (Goblin(hits,id)) -> let h = hits - 3 in
                            if h <= 0 then g.(x2).(y2) <- Cave (* goblin died *)
                            else g.(x2).(y2) <- (Goblin(h,id))  
     | (Elf(hits,id)) -> let h = hits - 3 in
                         if h <= 0 then g.(x2).(y2) <- Cave (* elf died *)
                         else g.(x2).(y2) <- (Elf(h,id))
     | Val _ -> raise AttackExceptionVal 

(* clear flood *)
let flood_clear g =
  let rec recur x y =
    if y >= 7 then ()
    else if x >= 7 then recur 0 (y + 1)
    else let e = g.(x).(y)
         and next () = recur (x + 1) y 
         in match e with
            | (Val(n)) -> g.(x).(y) <- Cave  ; next ()
            | _ -> next () 
  in recur 0 0 




(* onboard - sort with direction *)
let rec flood g x y n =
  if onboard x y then  (* fixed 7 x 7 grid we do not need g to determine size of grid *)
    let elem = g.(x).(y)
    in match elem with
       | Cave -> (g.(x).(y) <- Val n ;
                 flood g (x - 1) y (n + 1);
                 flood g (x + 1) y (n + 1);
                 flood g x (y - 1) (n + 1);
                 flood g x (y + 1) (n + 1))
       | (Val n2) -> if n < n2 then (g.(x).(y) <- Val n ;
                                     flood g (x - 1) y (n + 1);
                                     flood g (x + 1) y (n + 1);
                                     flood g x (y - 1) (n + 1);
                                     flood g x (y + 1) (n + 1))
       | _ -> () 
  


let goblin_move g e x y = 
  let ns = List.filter (fun (e2,x2,y2) -> isElf e2) (lexi g)
  in print_string "\nMoving (";
     show_piece e ;
     print_string "at " ;
     print_string (string_of_int x) ; print_string " , " ;
     print_string (string_of_int y) ;
     print_string ") targets =>" ;
     print_string "{";
     List.iter show_piece_xy ns ;
     print_string "}";
     let left = ref Int32.max_int
     and right = ref Int32.max_int
     and down = ref Int32.max_int
     and up = ref Int32.max_int
     in List.iter (fun (e2,x2,y2) ->
            flood_clear g ; 
            flood g (x2 - 1) y2 1 ;
            flood g (x2 + 1) y2 1 ;
            flood g x2 (y2 - 1) 1 ;
            flood g x2 (y2 + 1) 1 ;
            print_string "\nFlood filled for elf at " ;
            print_string (string_of_int x2) ;
            print_string " , " ;                                     
            print_string (string_of_int y2) ;
            print_string "\n";
            show_grid g ;
            print_string "\n";
            (* weight directions ==== TO DO === *)
            
            (* clear away Val(n) from grid *)
            flood_clear g
          ) ns 



exception MoveOrAttackException1

exception MoveOrAttackException2

exception MoveOrAttackException3


let rec goblin_move_or_attack g e x y =
  let ns = List.filter (fun (e2,x2,y2,dir2) -> isElf e2) (neighbour g x y)
  in
  print_string "\n(";
  show_piece e ;
  print_string "at " ;
  print_string (string_of_int x) ; print_string " , " ;
  print_string (string_of_int y) ;
  print_string ") neighbours =>" ;
  print_string "{";
  List.iter show_neighbour ns ;
  print_string "}";
  match ns with
  | [] -> goblin_move g e x y
  | ( (Elf(hits2,id2),x2,y2,dir2) :: t) -> (match t with
                                           | [] -> (print_string "\n(";
                                                    show_piece e ;
                                                    print_string "at " ;
                                                    print_string (string_of_int x) ; print_string " , " ;
                                                    print_string (string_of_int y) ;
                                                    print_string ") attacks Elf at " ;
                                                    print_string (string_of_int x2) ; print_string " , " ;
                                                    print_string (string_of_int y2) ;
                                                    print_string "\n" ;
                                                    attack g x y x2 y2)
                                           | _ -> raise MoveOrAttackException1)
  | _ -> raise MoveOrAttackException2
                                           



let rec elf_move_or_attack g e x y =
  let ns = List.filter (fun (e2,x2,y2,dir2) -> isGoblin e2) (neighbour g x y)
  in   print_string "\n{";
       List.iter show_neighbour ns ;
       print_string "}"


let rec move_or_attack g e x y =
  match e with
  | (Goblin(hits,id)) -> goblin_move_or_attack g e x y
  | (Elf(hits,id)) -> elf_move_or_attack g e x y
  | _ -> raise MoveOrAttackException3


let rec round g n =
  if n > 50 then ()
  else  let lex = lexi g
        in List.iter (fun (e,x,y) -> let _ = move_or_attack g e x y in () )  lex  ;
           print_string "After Round " ;
           print_string (string_of_int n);
           print_string "\n" ;
           show_grid g ;
           round g (n+1)



let run () =
  let g = example ()
  in  print_string "Initial \n"  ;
      show_grid g ;
      round g 1
  










