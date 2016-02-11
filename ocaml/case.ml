open Core.Std

(* A machine, and its properties *)
type machine =
{	day : int ;
	buy : int ;
	sell : int ;
	profit : int }

(* A case to solve, read from the input file *)
type case = {
	n_machine : int ;
	balance  : int ;
	days : int ;
	machines : machine list ;
}

(* Days left for a given case day *)
let days_left day case_days = case_days - day + 1

(* Cash all that can be cashed at the end *)
let cash_inventory balance days_left inventory =
	match inventory with
	| None -> balance
	| Some inventory -> balance + inventory.sell + days_left * inventory.profit

let is_affordable day balance machine =
	(machine.day = day) && (machine.buy <= balance)

(* List of machine affordable and available for buying that day *)
let get_available_machines day balance inventory machines =
	let potential_balance = match inventory with
		| None -> balance
		| Some inventory -> balance + inventory.sell
	in
	List.filter ~f:(is_affordable day potential_balance) machines

let list_max = function
	  [] -> invalid_arg "Empty list"
	| hd::tl -> List.fold_left ~f:max ~init:hd tl

let rec solve_ (case: case) (day: int) (balance: int) (inventory: machine option) (available_machines: machine list) =
	if day > case.days then (* End case 1 = time's up *)
		cash_inventory balance 0 inventory
	else if List.length available_machines = 0 then (* End case 2 = nothing left to trade *)
		cash_inventory balance (days_left day case.days) inventory
	else
		(* The list of machine we can buy that day *)
		let to_buy = get_available_machines day balance inventory available_machines in
		(* The money we could make by selling today *)
		let sell_price = match inventory with
			| None -> 0
			| Some inventory -> inventory.sell
		in
		(* The money we;ll make if we don't sell *)
		let daily_profit = match inventory with
			| None -> 0
			| Some inventory -> inventory.profit
    	in
		list_max (
				(* Branch where we just go to the next day *)
				(solve_ case (day+1) (balance+daily_profit) inventory (List.filter ~f:(fun t -> t.day > day) available_machines))
				::
				(* Branches where we buy a new machine *)
				(List.map
				~f:(fun m -> solve_
					case
					(day + 1)
					(balance + sell_price - m.buy) (* Current balance + sell price of previous machine - cost of new one*)
					(Some m) (* The machine we bought *)
					(List.filter ~f:(fun am -> (am <> m) && (am.day > day)) available_machines)
				)
				to_buy)) (* The machines we can buy that day *)


(* Wrapper function *)
let solve case =
	let available_machines = List.sort ~cmp:(fun x y -> compare x.day y.day) case.machines in
	solve_ case 0 case.balance None	available_machines

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
