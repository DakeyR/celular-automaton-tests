Random.self_init();;
let grid = Array.init 128 (fun x -> Array.init 96 (fun y -> 0));;
(*
y
^
|
|
|
|
|-------> x
(1 growing x, 0 stable,  -1 downing)
*)

let turn_right dir =
    let x = dir.(0) and
        y = dir.(1) in
    if x = 1 then
        begin
            dir.(0) <- 0;
            dir.(1) <- (-1)
        end
    else if x = -1 then
        begin
            dir.(0) <- 0;
            dir.(1) <- 1
        end
    else if y = 1 then
        begin
            dir.(0) <- 1;
            dir.(1) <- 0
        end
    else
        begin
            dir.(0) <- (-1);
            dir.(1) <- 0
        end;;

let turn_left dir = 
    begin
        turn_right dir;
        dir.(0) <- dir.(0) * -1;
        dir.(1) <- dir.(1) * -1;
    end;;

let move ant ant_dir =
    let dx = ant_dir.(0) and
        dy = ant_dir.(1) in
        let nx = ant.(0) + dx and
            ny = ant.(1) + dy in
            begin
                ant.(0) <- (if nx >= 0 then nx mod 128
                           else 128 + nx);
                ant.(1) <- (if ny >= 0 then ny mod 96
                           else 96 + ny);
            end;;

open Graphics;;
open_graph"";;
let show game = 
    Array.iteri (fun x col -> Array.iteri (fun y cell ->
        (if cell = 1 then set_color blue
            else if cell = 2 then set_color green
            else set_color white;
        fill_rect (x * 5)  (y * 5) 5 5)) col) game;;

let show_ant ant = 
    begin
        set_color red;
        fill_rect (ant.(0) * 5) (ant.(1) * 5) 5 5
    end;;
let show_cell x y cell = 
    begin
        if cell = 1 then set_color blue
            else if cell = 2 then set_color green
            else set_color white;
        fill_rect (x * 5)  (y * 5) 5 5
    end;;

let do_step game ant ant_dir = 
    let curr = game.(ant.(0)).(ant.(1)) in
    begin
        if curr = 0
        then turn_right ant_dir
        else if curr = 2 then
        turn_left ant_dir;
        game.(ant.(0)).(ant.(1)) <- (curr + 1) mod 3;
        show_cell ant.(0) ant.(1) curr;
        move ant ant_dir
    end;;

let ant = [|Random.int 128; Random.int 96|];;
let ant_dir = [|1; 0|];;

let game = grid in
    show game;
    show_ant ant;
    while true do
        do_step game ant ant_dir;
        show_ant ant;
        Unix.sleepf 0.01;
    done;;
