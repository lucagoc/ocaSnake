type direction = N | E | S | W;;

(* Window and menu init*)
let initialize_window = 
  let win = Curses.initscr () in
  let _ = Curses.keypad win true in
  let _ = Curses.noecho in
  let _ = Curses.curs_set 0 in
  win;;

let win = initialize_window;;
let maxxy = Curses.getmaxyx win;;


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
  let _ = Curses.move y (x+1) in
  let _ = Curses.addch 65 in
  ()
;;

let print_a (x,y) = 
  let _ = Curses.move y x in
  let _ = Curses.addch 112 in
  let _ = Curses.move y (x+1) in
  let _ = Curses.addch 112 in
  ()
;;

let remove_c (x,y) =
  let _ = Curses.move y x in
  let _ = Curses.addch 32 in
  let _ = Curses.move y (x+1) in
  let _ = Curses.addch 32 in
  ()
;;

let set_apple (x,y) =
  let resx = (Random.int x)/2*2+1 in
  let resy = (Random.int y)/2*2+1 in
  let _ = print_a (resx, resy) in
  (resx,resy)
;;

(* GAME FUNCTIONS *)
let create_snake = Queue.create;;
let slide_snake s (x,y) direction apple = 
  let t = ref (0,0) in
  let aux s (x,y) direction = match direction with
    | N -> Queue.push (x-2, y) s; t := (x-2, y)
    | W -> Queue.push (x, y-1) s; t := (x, y-1)
    | S -> Queue.push (x+2, y) s; t := (x+2, y)
    | E -> Queue.push (x, y+1) s; t := (x, y+1)
  in
  let _ = aux s (x,y) direction in
  let _ = print_c !t in
  if (apple == !t) then(
    let _ = set_apple maxxy in
    !t)
  else
    (let (x1,y1) = Queue.pop s in
    let _ = remove_c (x1,y1) in
    !t)
;;
let set_snake (y,x) =
  let midx = x / 2 in
  let midy = y / 2 in
  let s = create_snake () in
  let _ = Queue.push (midx, midy) s in
  let _ = print_c (midx,midy) in
  (midx, midy)
;;
  
let getdirection input fdirection  = match input with
| 258 -> E
| 259 -> W
| 260 -> N
| 261 -> S
| _ -> fdirection
;;


(* MAIN LOOP *)
let rec main_loop win s t fdirection apple = 

  let _ = Curses.refresh in

  (* Getting input *)
  let _ = Curses.timeout 200 in
  let input = Curses.getch () in
  let direction = getdirection input fdirection in
  let t2 = slide_snake s t direction apple in

  (* Pressing q leave the game *)
  if (input == 113) then 
    1
  else
    main_loop win s t2 direction apple
;;


let () = 

  (*Snake init *)
  let s =  create_snake () in
  let t = set_snake maxxy in
  let _ = Queue.push t s in
  let apple = set_apple maxxy in

  (* Main loop *)
  let _ = main_loop win s t N apple in
  
  Curses.endwin ();;