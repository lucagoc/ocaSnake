type direction = N | E | S | W;;

(* Window and menu init*)
let initialize_window = 
  let win = Curses.initscr () in
  let _ = Curses.keypad win true in
  let _ = Curses.noecho in
  let _ = Curses.curs_set 0 in
  let _ = Curses.start_color () in
  let _ = Curses.init_pair 0 0 0 in (*Background*)
  let _ = Curses.init_pair 1 0 4 in (*Snake*)
  let _ = Curses.init_pair 2 0 1 in (*Apple*)
  let _ = Curses.init_pair 3 0 2 in (*Score*)
  let _ = Curses.init_pair 4 0 2 in (*Overlay*)
  win
;;

let win = initialize_window;;
let maxxy = Curses.getmaxyx win;;


(* SYSTEM FUNCTIONS *)
let get_menu_dimensions maxxy = 
  (((fst maxxy) - 5), (snd maxxy))
;;

let draw_menu menu_win maxxy = 
  let dims = get_menu_dimensions maxxy in
  let _ = Curses.wresize menu_win (fst dims) (snd dims) in
  let _ = Curses.box menu_win 0 0 in
  Curses.wrefresh menu_win
;;

let print_c (x,y) = 
  let _ = Curses.attr_set 1 1 in
  let _ = Curses.move y x in
  let _ = Curses.addch 32 in
  let _ = Curses.move y (x+1) in
  let _ = Curses.addch 32 in
  let _ = Curses.attr_set 0 0 in
  ()
;;

let print_a (x,y) = 
  let _ = Curses.attr_set 2 2 in
  let _ = Curses.move y x in
  let _ = Curses.addch 32 in
  let _ = Curses.move y (x+1) in
  let _ = Curses.addch 32 in
  let _ = Curses.attr_set 0 0 in
  ()
;;

let remove_c (x,y) =
  let _ = Curses.attr_set 0 0 in
  let _ = Curses.move y x in
  let _ = Curses.addch 32 in
  let _ = Curses.move y (x+1) in
  let _ = Curses.addch 32 in
  let _ = Curses.attr_set 0 0 in
  ()
;;

let issnakescrewed s t = 
  let s2 = Queue.copy s in
  
  (*Routing the queue*)
  let rec aux sa t =
    if(Queue.is_empty sa) then
      false
    else
      let t2 = Queue.pop sa in
      let (x1, y1) = t in
      let (x2, y2) = t2 in
      if (Queue.is_empty sa) then
        false
      else
      if (x1 == x2 && y1 == y2) then
        true
      else
        aux sa t
  in

  aux s2 t
;;

let rec set_apple (x,y) s =
  let _ = Random.self_init  in
  let time = Sys.time () in
  let _ = Random.init ((Random.int 2048) + (int_of_float time)) in
  let resx = ((Random.int y)/2)*2 in
  let resy = ((Random.int x)/2)*2 in

  (*Test to not override the snake, using snakescrewed function*)
  if (issnakescrewed s (resx,resy)) then
    set_apple (x,y) s
  else
    let _ = print_a (resx, resy) in
    (resx, resy)
;;

(* GAME FUNCTIONS *)
let create_snake = Queue.create;;

let rec yourescrewd () =
  let _ = Curses.attr_set 2 2 in
  let (x,y) = maxxy in
  let midx = x / 2 + (x mod 2) in
  let midy = y / 2 in
  let _ = Curses.move midx (midy-16) in
  let _ = Curses.addstr "YOURE SCREWD ! Press q to quit..." in
  let _ = Curses.attr_set 0 0 in
  let _ = Curses.timeout (-1) in
  let input = Curses.getch () in
  if (input == 113) then
    ()
  else
    yourescrewd ()
;;


let slide_snake s (x,y) direction apple score = 
  let t = ref (0,0) in
  let aux s (x,y) direction = match direction with
    | N -> Queue.push (x-2, y) s; t := (x-2, y)
    | W -> Queue.push (x, y-1) s; t := (x, y-1)
    | S -> Queue.push (x+2, y) s; t := (x+2, y)
    | E -> Queue.push (x, y+1) s; t := (x, y+1)
  in
  let _ = aux s (x,y) direction in
  let _ = print_c !t in

  let (xapple, yapple) = apple in
  let (tx, ty) = !t in
  let (maxx, maxy) = maxxy in
  if (issnakescrewed s !t) then
    (yourescrewd ();
    ((-1,-1),(-1,-1))) (*Return stupid values*)
  else
  if (xapple == tx && yapple == ty) then
    (let apple = set_apple maxxy s in
    score := !score + 1;
    (!t,apple))
  else if ((tx < 0) || (ty < 0) || (tx > maxy/2*2-1) || (ty > maxx/2*2)) then
    (yourescrewd ();
    ((-1,-1),(-1,-1))) (*Return stupid values*)
  else
    (let (x1,y1) = Queue.pop s in
    let _ = remove_c (x1,y1) in
    (!t,apple))
;;

let set_snake (y,x) =
  let midx = x / 2 + (x mod 2) in
  let midy = y / 2 in
  let s = create_snake () in
  let _ = Queue.push (midx, midy) s in
  let _ = print_c (midx,midy) in
  (midx, midy)
;;
  
let getdirection input fdirection  = match input with
| 258 -> if not(fdirection == W) then E else fdirection;
| 259 -> if not(fdirection == E) then W else fdirection;
| 260 -> if not(fdirection == S) then N else fdirection;
| 261 -> if not(fdirection == N) then S else fdirection;
| _ -> fdirection
;;

let print_score score =
  let _ = Curses.attr_set 2 2 in
  let _ = Curses.move 0 0 in
  let _ = Curses.addstr "Score " in
  let unities = !score mod 10 in
  let tennies = (!score / 10) mod 10 in
  let hundreds = (!score / 100) mod 10 in
  let _ = Curses.addch (48+(hundreds)) in
  let _ = Curses.addch (48+(tennies)) in
  let _ = Curses.addch (48+(unities)) in
  ()
;;

let speedcalculation score =
  let calculation = ((300 - !score*6)) in
  if (calculation > 0) then
    calculation
  else
    1
;;

(* MAIN LOOP *)
let rec main_loop win s t fdirection apple score = 

  let _ = Curses.refresh in

  (* Getting input *)
  let _ = Curses.timeout (speedcalculation score) in
  let input = Curses.getch () in
  let direction = getdirection input fdirection in
  let (t2,napple) = slide_snake s t direction apple score in

  let _ = print_score score in

  (*Does the snake is screwed ?*)
  if (t2 == (-1,-1)) then 
    1
  (* Pressing q leave the game *)
  else if (input == 113) then 
    1
  (*The game can continue*)
  else
    main_loop win s t2 direction napple score
;;


let () = 

  (*Snake init *)
  let s =  create_snake () in
  let t = set_snake maxxy in
  let _ = Queue.push t s in
  let apple = set_apple maxxy s in
  let score = ref 0 in

  (* Main loop *)
  let _ = main_loop win s t N apple score in
  
  Curses.endwin ()
;;