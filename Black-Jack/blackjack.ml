type Couleur = Trèfle | Pique | Carreau | Coeur;;

type Carte = 
	 As of Couleur
	|Roi of Couleur
	|Dame of Couleur
	|Valet of Couleur
	|Petite of int * Couleur;;
	
type Paquet = list of Carte;;

type Joueur = { banque : bool ; mutable cartes_tirees : Paquet ; mutable valeur_paquet : int };;


(* Je test la probabilité d'atteindre une certaine carte sans brûler *)
(* n = carte du joueur *)

let probabilite_de_depasser_deux_coups n =
	let t = ref 0 and k = ref 0 and a = ref 0 in
	for i = 1 to 1000000 do
		t:= random__int (13) + 2; (* le joueur tire une première carte *)
		if !t = 12 || !t= 13 || !t = 14 then t:= 10; (* Comme ça pas besoin de changer la valeur de l'as *)
							
		a := random__int (13) + 2; (* le joueur retire une carte *)					
		if !a = 12 || !a = 13 || !a = 14 then t:= !t+ 10 (* Comme ça pas besoin de changer la valeur de l'as *)
								else if !t = 11 && !a = 11 then t:= 12
										else t := !t + !a;
									
		if !t > n && !t <= 21 && n <> 21 then incr k;
	done;
	float_of_int(!k)/.1000000. ;;
		
let probabilite_de_depasser n =
	let t = ref 0 and k = ref 0 and a = ref 0  in (*a:= variable auxiliaire pour stocker la carte cachée avant de filtrer selon que l'on ait une tête ou non *)
	for i = 1 to 1000000 do
		t:= random__int (13) + 2; (* le joueur tire une première carte *)
		if !t = 12 || !t= 13 || !t = 14 then t:= 10; (* Comme ça pas besoin de changer la valeur de l'as *)

		while !t <= n && !t < 21 do
			a:= random__int (13) + 2;
			if !a = 12 || !a= 13 || !a = 14 then t:= 10 + !t
				else if !a = 11 && !t + !a > 21 then t:= !t + 1
					else t := !t + !a;
		done;
		if !t <= 21 && n <> 21 then incr k;
	done;
	float_of_int(!k)/.1000000. ;;
	
let tirage() =
	let l = ref [] in
		for i = 21 downto 2 do
			l:= [|float_of_int i; probabilite_de_depasser i|]::(!l); done;
	l;;
	
tirage();;

let probabilite_depasser_conditionnel n c = (* le croupier est à n et le joueur est à c *)
	if n > c then 1.0
		else if n = 21 then 0. (* Impossible de battre le croupier s'il est à 21 *)
			else
				let a = ref 0 and k = ref 0 in
					for i = 0 to 100000 do
						