# open "graphics";;
open_graph "";;

let tirer () =
	let a = random__int(3) in
	match a with
	0 -> 40
	|1 -> -40
	|2 -> 0;;
	

let marche_aléatoire p =
	let x = ref (size_x()/2) and y = ref (size_y()/2) and n = p in
	moveto !x !y;
	for i = 1 to n do
		if !x <= 0 then begin
			x:= 40;
			if !y = 0 then y:= 1
			else if !y >= size_y() then y := size_y() - 40
			else if !y <= 0 then y:= 40
			else y:= !y + tirer();
			end
		else if !x >= size_x() then begin
		x:= !x-1;
			if !y = 0 then y:= 1
			else if !y >= size_y() then y := size_y() - 40
			else if !y <= 0 then y:= 40
			else y:= !y + tirer();
			end
		else begin
		x:= tirer() + !x;
			if !y = 0 then y:= 40
			else if !y >= size_y() then y := size_y() - 40
			else if !y <= 0 then y:= 40
			else y:= !y + tirer();
			end;
		lineto !x !y;
	done;;
	
	marche_aléatoire 200;;
	
	let tirer()=
		let alpha = random__float (4*atan
	let marche_aleatoire_realiste p = 
		let x = ref (size_x()/2) and y = ref (size_y()/2) and n = p in
		moveto !x !y;
		if !x <