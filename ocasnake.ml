type direction = N | E | S | W;;

(* SYSTEM FUNCTIONS *)
let get_menu_dimensions maxxy = 
  (((fst maxxy) - 5), (snd maxxy))

let draw_menu menu_win maxxy = 
  let dims = get_menu_dimensions maxxy in
  let _ = Curses.wresize menu_win (fst dims) (snd dims) in
  let _ = Curses.box menu_win 0 0 in
  Curses.wrefresh menu_win;;

let print_c (x,y) = 
  let _ = Curses.move y x in
  let _ = Curses.addch 65 in
  ()
;;

let remove_c (x,y) =
  let _ = Curses.move y x in
  let _ = Curses.addch 32 in
  ()
;;

(* GAME FUNCTIONS *)
let create_snake = Queue.create;;
let slide_snake s (x,y) direction = 
  let t = ref (0,0) in
  let aux s (x,y) direction = match direction with
  | N -> Queue.push (x-1, y) s; t := (x-1, y)
  | W -> Queue.push (x, y-1) s; t := (x, y-1)
  | S -> Queue.push (x+1, y) s; t := (x+1, y)
  | E -> Queue.push (x, y+1) s; t := (x, y+1)
  in
  let (x1,y1) = Queue.pop s in
  let _ = remove_c (x1,y1) in
  let _ = aux s (x,y) direction in
  let _ = print_c !t in
  !t
;;
let expand_snake s (x,y) direction =
  let t = ref (0,0) in
  let aux s (x,y) direction = match direction with
  | N -> Queue.push (x-1, y) s; t := (x-1, y)
  | W -> Queue.push (x, y-1) s; t := (x, y-1)
  | S -> Queue.push (x+1, y) s; t := (x+1, y)
  | E -> Queue.push (x, y+1) s; t := (x, y+1)
  in
  let _ = aux s (x,y) direction in
  print_c !t;;
let set_snake (y,x) =
  let midx = x / 2 in
  let midy = y / 2 in
  let s = create_snake () in
  let _ = Queue.push (midx, midy) s in
  let _ = print_c (midx,midy) in
  (midx, midy)
;;
  
let getdirection input = match input with
| 258 -> N
| 259 -> S
| 260 -> E
| 261 -> W
| _ -> N
;;


(* MAIN LOOP *)
let rec main_loop win s t = 

  let _ = Curses.refresh in

  (* Getting input *)
  let input = Curses.getch () in
  let t2 = slide_snake s t (getdirection input) in

  (* Pressing q leave the game *)
  if (input == 113) then 
    1
  else
    main_loop win s t2
;;

let initialize_window = 
  let win = Curses.initscr () in
  let _ = Curses.keypad win true in
  let _ = Curses.noecho in
  win;;


let () = 
  (* Window and menu init*)
  let win = initialize_window in
  let maxxy = Curses.getmaxyx win in

  (*Snake init *)
  let s =  create_snake () in
  let t = set_snake maxxy in
  let _ = Queue.push t s in

  (* Main loop *)
  let _ = main_loop win s t in
  
  Curses.endwin ();;