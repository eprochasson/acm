open Case
open Core.Std

let ()=
 	let my_machines =
	[   { day = 6 ; buy = 12 ; sell = 1 ; profit = 3 } ;
			{ day = 1 ; buy = 9 ; sell = 1 ; profit = 2 } ;
			{ day = 3 ; buy = 2 ; sell = 1 ; profit = 2 } ;
			{ day = 8 ; buy = 20 ; sell = 5 ; profit = 4 } ;
			{ day = 4 ; buy = 11 ; sell = 7 ; profit = 4 } ;
			{ day = 2 ; buy = 10 ; sell = 9 ; profit = 1 }
		] in
	let my_case = {
		n_machine = 6 ;
		balance = 10 ;
		days = 20 ;
		machines = my_machines
	} in
		solve my_case
		|> printf "Case 1: %3d\n"
