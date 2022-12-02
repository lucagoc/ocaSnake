
let get_menu_dimensions maxxy = 
  (((fst maxxy) - 5), (snd maxxy))

let draw_menu menu_win maxxy = 
  let dims = get_menu_dimensions maxxy in
  let _ = Curses.wresize menu_win (fst dims) (snd dims) in
  let _ = Curses.box menu_win 0 0 in
  Curses.wrefresh menu_win;;


let rec main_loop win menu = 
  (* Evaluation command *)
  let maxxy = Curses.getmaxyx win in
  let _ = draw_menu menu maxxy in
  let input = Curses.getch () in
  let current_position = Curses.getyx win in
  if (input == 9) 
  then 1
  else 
    let newxy = match input with
      | 258 -> (((fst current_position) + 1), (snd current_position))
      | 259 -> (((fst current_position) - 1), (snd current_position))
      | 260 -> ((fst current_position), ((snd current_position) - 1))
      | 261 -> ((fst current_position), ((snd current_position) + 1))
      | 410 -> current_position  
      | _ -> current_position in
    let _ = Curses.move (fst newxy) (snd newxy) in
    let _ = Curses.refresh in
  main_loop win menu;;

let initialize_window = 
  let win = Curses.initscr () in
  let _ = Curses.keypad win true in
  let _ = Curses.noecho in
  (* let _ = Curses.box win 64 64 in *)
  win;;


let () = 
  let win = initialize_window in
  let maxxy = Curses.getmaxyx win in
  let menu_dims = get_menu_dimensions maxxy in
  let menu = Curses.newwin (fst menu_dims) (snd menu_dims) 0 0  in
  let _ = main_loop win menu in
  Curses.endwin ();;
