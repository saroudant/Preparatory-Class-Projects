# open "graphics";;
open_graph "";;

(* Initialisons les variables *)

let k1 = 1.28;; let k2 = 33.6;; let k3 = 2400000.;;let k4 = 2400.;;
let k5 = 1.;; let f = 1.;; (*let A = 0.3357*) let A = 0.06;; (*let B = 0.05714*) let B = 0.02;; let fer = 0.002679;; let diffx = 0.002;; let diffy = 0.003;; let diffz = 0.002;;
let size = (min (size_x()) (size_y()))/4 - 10 ;;
let a = make_matrix (size+2) (size+2) 0.;; let b = make_matrix (size+2) (size+2) 0.;; let c = make_matrix (size+2) (size+2) 0.;;
let d = make_matrix (size) (size) 0.;; let e = make_matrix (size) (size) 0.;; let g = make_matrix (size) (size) 0.;;

(* Pour un soucis de clareté, on a repris des notations semblables à celles de l'algorithme Maple; néanmoins, le problème des majuscules fait que a,b et c 
correspondent aux variables locales *)
let vider () = for i=0 to size do 
	for j=0 to size do 
		set_color white;
		fill_circle (i*7) (j*7) 10; done; done;;
let remplacer t v =
	for i = 0 to size - 1 do
		for j = 0 to size - 1 do
			t.(i).(j) <- v.(i).(j);
		done;
	done;;
	
let reac_diff dx dy dz x y z dt =
	for i = 1 to size do
		for j = 1 to size do
			a.(i).(j) <- x.(i-1).(j-1); b.(i).(j) <- y.(i-1).(j-1); c.(i).(j) <- z.(i-1).(j-1);
		done;
	done;
	for i = 0 to size - 1 do
		for j = 0 to size - 1 do
		x.(i).(j) <- a.(i+1).(j+1)+. (k1 *. A *. b.(i+1).(j+1) +. k2 *. A *. a.(i+1).(j+1) -. k3 *. a.(i+1).(j+1)*. b.(i+1).(j+1)-. 2. *. k4 *. a.(i+1).(j+1) *. a.(i+1).(j+1) +.diffx /.(2.*.dx*.dx) *. (a.(i+2).(j+1)+. a.(i+1).(j+2)+. a.(i).(j+1)+.  a.(i+1).(j)+. a.(i).(j) +. a.(i+2).(j) +. a.(i+2).(j+2)+. a.(i).(j+2)-. 4.*.a.(i+1).(j+1)))*.dt;
		y.(i).(j) <- b.(i+1).(j+1)+. ( (-.1.)*.k1 *. A *. b.(i+1).(j+1) -. k3*. a.(i+1).(j+1) *. b.(i+1).(j+1) +. (1./.2.)*. k5 *. f *. B*. c.(i+1).(j+1) +. diffy /.(2.*.dy*.dy)*.( b.(i+2).(j+1)+. b.(i+1).(j+2)+. b.(i).(j+1)+.  b.(i+1).(j)+. b.(i).(j)+. b.(i+2).(j) +. b.(i+2).(j+2)+. b.(i).(j+2) -.4.*.b.(i+1).(j+1)))*.dt;
		z.(i).(j) <- c.(i+1).(j+1)+. (2.*. k2 *.A *. a.(i+1).(j+1) -. k5*.B*. c.(i+1).(j+1) +. diffz /. (2.*.dz*.dz) *. (c.(i+2).(j+1)+. c.(i+1).(j+2)+. c.(i).(j+1)+.  c.(i+1).(j)+. c.(i).(j) +. c.(i).(j+2) +. c.(i+2).(j)+. c.(i).(j+2) -.4.*.c.(i+1).(j+1)))*.dt;
		
		if x.(i).(j) < 0. then x.(i).(j) <- 0.;
		if y.(i).(j) < 0. then y.(i).(j) <- 0.;
		if z.(i).(j) < 0. then z.(i).(j) <- 0.;
		
	done; 
	done;
(*remplacer x d;
	remplacer y e;
	remplacer z g*);;
	

let dessin i j z = 
	if (z.(i).(j) >= fer/.2.) then begin set_color blue; fill_circle (i*4) (j*4) 2; end 
		else if (z.(i).(j) >= fer /.4.) then begin set_color blue; fill_circle (i*4) (j*4) 2; end
		(*else if	(z.(i).(j) >= fer /. 500.) then begin set_color green; fill_circle (i*10) (j*10) 6; end
		else if (z.(i).(j) > 0.) then begin set_color black; fill_circle (i*4) (j*4) 3; end*)
		else  begin set_color red; fill_circle (i*4) (j*4) 1; end ;;

let bz_color z = 
	for i = 0 to size - 1 do
		for j = 0 to size - 1 do
			dessin i j z;
		done;
	done;;
	
(* tire aléatoirement de manière à donner un résultat vrai une fois sur k*)	
let tirage_aleatoire k = 
	let u = random__int (k) in (u=0);;
	
let reac_bz dx dy dz eps ordre dt = 
	let x = make_matrix (size) (size) 0.  and y = make_matrix (size) (size) 0. and z = make_matrix (size) (size) 0. in z.(size/2).(size/2)<- eps; bz_color z;
	let v = ref 2 and v2 = ref 2. and j = ref 0 and k = ref 0 in
	for i = 0 to ordre - 1 do
		reac_diff dx dy dz x y z dt;
		(*if tirage_aleatoire !v then begin
			j:= random__int size; k:= random__int size; z.(!j).(!k) <- z.(!j).(!k) +. eps;
			end;
		v:= (9* !v)/2;*)
		if i mod 100 = 0 then begin
			vider(); bz_color z;end;
	done;
	(z.(size/2).(size/2),x.(size/2).(size/2), y.(size/2).(size/2));;
		


(*reac_bz 1. 1. 1. (0.001) 1500 0.1;;*)
(*reac_bz 5. 5. 5. (0.000005) 4000 10.;;*)
reac_bz (50.*.sqrt(diffx)) (50.*.sqrt(diffy)) (50.*.sqrt(diffz)) (0.00005) 1000 0.5;;
