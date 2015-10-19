(* Fonction tirant un paquet au hasard *)
(* cf fichier world pour explication *)

let tirage_paquet () =
	let t = make_vect (6*52) 0 in
	for k = 0 to 6*52 - 1 do t.(k) <- k mod 13; done;
	
	let v = make_vect (6*52) 0 and a = ref 0 in
	for k = 0 to 6*52 - 1 do
		a := random__int (6*52-k);
		v.(k) <- t.(!a);
		for i = !a to 6*52-2-k do 
			t.(i) <- t.(i+1);
		done;
	done;
	v;;


(*Fonction tirant une carte au hasard à partir d'un deck donné.
C'est une fonction bidon, juste question de simplifier l'écriture après *)

let tirer_carte t =
	let a = ref t.(0) in
	for i = 0 to 6*52-2 do
		t.(i)<-t.(i+1);
	done;
	t.(6*52-1) <- !a;
	t;;

let test_hilo () =
	let n = ref 0 and t = tirage_paquet() and k = ref 0 in
	for i = 0 to 51 do
		if t.(i) <= 4 then decr k
			else if t.(i) >= 7 then incr k;
	done;
	!k;;
	
let tester n =
	let h = ref 0 in
	for i = 0 to n do
		h:= !h + test_hilo();
	done;
	float_of_int(!h)/.(float_of_int n);;

let hi_low t x = 
	if 0 <= x && x <= 4 then 1
	else if 5 <= x && x <= 7 then 0
		else  -1;;

let arreter h nb_carte = 
	if h <= -((nb_carte)/2) then 16
	else if -((nb_carte)/2) < h && h <= 0 && h < (nb_carte)/2  then 17
	else 18;;

let jouer d =
	let pas = ref (d/50) and aux = ref 0 and (* pas : valeur joué à chaque fois; aux : variable auxiliaire *)
	mise = ref d and t = tirage_paquet() and (* mise: mise au début; t : deck *)
	nb_carte = ref 0 and h = ref 0 and c = ref 0 and (* nb_carte: nbre de cartes ; h : valeur du hi-low; c: valeur de la carte cachée *)
	cache = ref 0 and joueur = ref 0 in (* cache: valeur cache du croupier; c: valeur du croupier ; joueur: valeur du joueur; h: nombre de cartes tirées *)
	
	while !nb_carte < 295 && !mise >= 0 do
		if !h < -(!nb_carte/(2)) && !nb_carte > 200 then pas := d/500
		else if !h < -( !nb_carte/(4) ) && !nb_carte > 200 then pas := d/400
		else if !h < -( !nb_carte/(4) ) then pas := d/300
		else if !h > ( !nb_carte/2) && !nb_carte > 200 then pas := d/10
		else if !h > ( !nb_carte/4) && !nb_carte > 200 then pas := d/20
		else if !h > (!nb_carte/2) then pas := d/30
		else pas := d/40;
	
		(* On tire pour le croupier *)
		c := 0; cache := 0;
		while !c < 17 do
			aux := t.(!nb_carte);
			h:= !h + (hi_low t !nb_carte);
			if !c > 11 && !aux = 12 then incr c 
			else if !aux > !cache then begin 
						if !aux <= 7 then begin 
								cache := !aux;c:= !cache + !c; incr nb_carte;
						end
						else if !aux >= 7 then begin
							cache := 10; c:= !cache + !c; incr nb_carte;
						end
					end
					else begin	
						if !aux <= 7 then begin 
								c:= !aux + !c; incr nb_carte;
						end
						else if !aux >= 7 then begin
							c:= 10 + !c; incr nb_carte;
						end
					end
		done;
		
		(* On tire pour le joueur *)
		
		joueur := 0;
		while (*!nb_carte < 100 &&*) !joueur <= 16 do (*!joueur < !c - !cache && !joueur <= arreter !h !nb_carte*) 
			aux := t.(!nb_carte);
			if !aux < 7 then joueur := !joueur + !aux
				else if !aux = 12 && !joueur > 11 then incr joueur
				else joueur := !joueur + 10;
			h:= !h + (hi_low t !nb_carte);
			incr nb_carte;
		done;
		(*while !nb_carte >= 100 && !joueur <= arreter !h !nb_carte do
			aux := t.(!nb_carte);
			if !aux < 7 then joueur := !joueur + !aux
				else if !aux = 12 && !joueur > 11 then incr joueur
				else joueur := !joueur + 10;
			h:= !h + (hi_low t !nb_carte);
			incr nb_carte;
		done;*)
		while (!joueur <= !c - !cache) do
			aux := t.(!nb_carte);
			if !aux < 7 then joueur := !joueur + !aux
				else if !aux = 12 && !joueur > 11 then incr joueur
				else joueur := !joueur + 10;
			(*h:= !h + (hi_low t !nb_carte);*)
			incr nb_carte;
		done;
		
		if !joueur > 21 then mise := !mise - !pas
				else if !joueur > !c || !c > 21 then mise := !mise + !pas
					else mise := !mise - !pas;
	done;
	
	!mise;;
				

let gain_moyen n =
	let f = ref 10000 and gagne = ref 0 and moyen = ref 0 and perte = ref 0 and aux = ref 0 in
	for i = 1 to n do
		aux := jouer !f;
		if !aux >= !f then incr gagne
			else if !aux <= 0 then incr perte
			else incr moyen;
	done;
	(!gagne,!moyen,!perte);;
	
gain_moyen 50000;;