(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs = 
    List.filter (fn s => (Char.isUpper(String.sub(s, 0)))) xs

fun longest_string1 xs =
    foldl (fn (curr, acc) => if String.size(curr) > String.size(acc) then curr else acc) "" xs

fun longest_string2 xs =
    foldl (fn (curr, acc) => if String.size(curr) >= String.size(acc) then curr else acc) "" xs

fun longest_string_helper comparer xs =
    foldl (fn (curr, acc) => if comparer(String.size(curr), String.size(acc)) then curr else acc) "" xs

fun longest_string3 xs =
    let 
	val aux = longest_string_helper (fn (curr, acc) => curr > acc)
    in
	aux(xs)
    end

fun longest_string4 xs =
    let 
	val aux = longest_string_helper (fn (curr, acc) => curr >= acc)
    in
	aux(xs)
    end

fun longest_capitalized xs =
    (longest_string1 o only_capitals) xs

fun rev_string s =
    (String.implode o rev o String.explode) s

fun first_answer f xs =
    let
	fun aux xs =    
	    case xs of
		[] => raise NoAnswer
	      | SOME x :: _ => x
	      | NONE :: xs => aux xs
    in
	aux (map f xs)
    end
				   
fun all_answers f xs =
    let
	fun aux rest acc =
	    case rest of
		[] => SOME acc
	      | SOME x :: xs => aux xs (acc @ x)
	      | NONE :: _ => NONE
    in
	aux (map f xs) []
    end

fun count_wildcards p =
    g (fn () => 1) (fn s => 0) p

fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn s => String.size s) p

fun count_some_var (s, p) =
    g (fn () => 0) (fn s' => if s' = s then 1 else 0) p

fun check_pat p =
    let
	fun get_var_names p =
	    case p of
		Wildcard => []
	      | UnitP => []
	      | ConstP _ => []
	      | ConstructorP(_, p) => get_var_names p
	      | TupleP(ps) => foldl (fn (p, vs) => vs @ get_var_names p) [] ps
	      | Variable s => [s]
	
	fun has_repeats vs =
	    case vs of
		[] => false
	      | v :: vs => (List.exists (fn v' => v' = v) vs) orelse (has_repeats vs)
    in
	(not o has_repeats o get_var_names) p
    end

fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (Unit, UnitP) => SOME []
      | (Const v, ConstP p) => if p = v then SOME [] else NONE
      | (Constructor(s, v), ConstructorP(s', p)) => if s = s' then match(v, p) else NONE
      | (Tuple(vs), TupleP(ps)) => if List.length(ps) = List.length(vs) 
				   then all_answers (fn (p, v) => match(v, p)) (ListPair.zip (ps, vs))
				   else NONE
      | (v, Variable name) => SOME [(name, v)] 
      | _ => NONE

fun first_match v ps =
     SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
