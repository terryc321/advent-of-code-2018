(*

minimal working solution  MVP

functional so no side effects
Grid.set g x y z -> should make a new grid !

#######   
#.G...#   G(200)
#...EG#   E(200), G(200)
#.#.#G#   G(200)
#..G#E#   G(200), E(200)
#.....#   
#######

 *)

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents 

type piece = Wall
           | Cave
           | Goblin of int  
           | Elf of int



let input () =
  let obs  = ref []
  in  let rec process_string_list xs n =
        match xs with
        | [] -> !obs 
        | ( s :: t ) -> let _ = process_string s n 
                        in process_string_list t (n + 1)
      and process_string s n = String.mapi (fun i c ->
                                   match c with
                                   | '#' -> obs := (Wall ,i+1,n) :: !obs  ; c
                                   | 'G' -> obs := ((Goblin(200)),i+1,n) :: !obs ;c 
                                   | 'E' -> obs := ((Elf(200)),i+1,n) :: !obs ;c 
                                   | '.' -> obs := (Cave,i+1,n) :: !obs ;c
                                   | _ -> c) s  
      in process_string_list (read_lines "../../example.txt") 1 ;;
          

(* goblin hit points change , power always 3
   likewise elf hit points change , power always 3 
 *)





