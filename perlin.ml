open Graphics;;

let permutation = [151;160;137;91;90;15;
131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180]
 
let lerp(t, a, b) = a +. t *. (b -. a)
 
let fade t =
  (t *. t *. t) *. (t *. (t *. 6. -. 15.) +. 10.)
 
let grad (hash, x, y, z) =
  let h = hash land 15 in
  let u = if (h < 8) then x else y in
  let v = if (h < 4) then y else (if (h = 12 || h = 14) then x else z) in
  (if (h land 1 = 0) then u else (0. -. u)) +.
    (if (h land 2 = 0) then v else (0. -. v))
 
let perlin_init p = 
  List.rev (List.fold_left (fun i x -> x :: i) (List.rev p) p);;
 
let perlin_noise p x y z =
  let x1 = (int_of_float x) land 255 and
      y1 = (int_of_float y) land 255 and
      z1 = (int_of_float z) land 255 and
      xf = x -. (float (int_of_float x)) and
      yf = y -. (float (int_of_float y)) and
      zf = z -. (float (int_of_float z)) in
  let u = fade xf and
      v = fade yf and
      w = fade zf and
      a = (List.nth p x1) + y1 in
  let aa = (List.nth p a) + z1 and
      ab = (List.nth p (a + 1)) + z1 and
      b = (List.nth p (x1 + 1)) + y1 in
  let ba = (List.nth p b) + z1 and
      bb = (List.nth p (b + 1)) + z1 in
  lerp(w, lerp(v, lerp(u, (grad((List.nth p aa), xf, yf, zf)),
                          (grad((List.nth p ba), xf -. 1., yf , zf))),
                  lerp(u, (grad((List.nth p ab), xf , yf -. 1., zf)),
                          (grad((List.nth p bb), xf -. 1., yf -. 1., zf)))),
          lerp(v, lerp(u, (grad((List.nth p (aa + 1)), xf, yf, zf -. 1.)),
                          (grad((List.nth p (ba + 1)), xf -. 1., yf , zf -. 1.))),
                  lerp(u, (grad((List.nth p (ab + 1)), xf , yf -. 1., zf -.  1.)),
                          (grad((List.nth p (bb + 1)), xf -. 1., yf -.  1., zf -. 1.)))))
 
;;

let color_and_plot x y c = (set_color c; plot x y);;
let get_int c = int_of_float ((float_of_int 255) *. c);;
let compute_color f = 
    let value = (f /. 2.0) +. 0.5 in
        match value with
            _ when value < 0.2 -> rgb 23 63 131
           |_ when value < 0.4 -> rgb 50 117 50
           |_ when value < 0.6 -> rgb 71 149 71
           |_ when value < 0.8 -> rgb 214 218 125
           |_ when value < 0.95 -> rgb 213 207 75 
           |_ -> rgb 255 255 255;;
    (* rgb (get_int value) (get_int value) (get_int value);; *)

open_graph"";;

let myarr = Array.init 640 (fun x -> Array.init 480 (fun y -> perlin_noise  (perlin_init permutation) (float_of_int y *. 0.01) (float_of_int x *. 0.01) (0.2)));;
let colors = Array.map (fun line -> Array.map compute_color line) myarr;;
Array.iteri (fun x line -> Array.iteri (fun y cell -> color_and_plot x y cell) line) colors;; 
let p = perlin_init permutation in
    print_string((Printf.sprintf "%0.17f" (perlin_noise p 3.14 42.0 7.0)) ^ "\n")
;;
read_int ();
