Random.self_init();;
let grid = Array.init 64 (fun x -> Array.init 48 (fun y -> Random.int 5));;
(*
let grid = [|
    [|0;0;0;0;0|];
    [|0;0;0;0;0|];
    [|0;3;3;3;0|];
    [|0;0;0;0;0|];
    [|0;0;0;0;0|]
            |];;
*)

let is_alive arr x y alive_limit =
    let width = Array.length arr in 
        let height = Array.length arr.(0) in
            let i = (if x < 0 then width + x else x mod width) and
                j = (if y < 0 then height + y else y mod height) in
            arr.(i).(j) >= alive_limit;;

let get_neighbours arr x y alive_limit = 
    let acc = [|0|] in
        for i = x - 1 to x + 1 do
            for j = y - 1 to y + 1 do
                if (i != x || j != y) && is_alive arr i j alive_limit then
                    acc.(0) <- acc.(0) + 1
            done
        done;
        acc.(0);;
            
open Graphics;;
open_graph"";;
let show game alive_limit = 
    Array.iteri (fun x col -> Array.iteri (fun y cell ->
        (if cell >= alive_limit then set_color black
            else set_color white;
        fill_rect (x * 10)  (y * 10) 10 10); set_color black; draw_rect (x * 10) (y *10) 10 10) col) game;;

let do_step game alive_limit= 
    Printf.printf "next step\n";
    Array.mapi (fun x col -> Array.mapi (fun y cell ->
        let score = get_neighbours game x y alive_limit in
            if cell >= alive_limit then
                (if score < 2 then 0
                else if score > 3 then 0
                else cell)
            else if score = 3 then alive_limit
            else 0) col) game;;

let copy_board game next =
    Array.iteri (fun x col -> Array.iteri (fun y cell ->
        game.(x).(y) <- cell) col) next;;

let game = grid and
    alive_limit = 3 in
    while true do
        show game alive_limit;
        let next = do_step grid alive_limit in
        Printf.printf "end of step\n";
        copy_board game next;
        Unix.sleepf 0.2;
    done;;
