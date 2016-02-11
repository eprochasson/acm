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

val solve: case -> int
