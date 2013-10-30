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
    (longest_string2 o only_capitals) xs

fun rev_string s =
    (String.implode o rev o String.explode) s

fun first_answer f xs =
    case map f xs of
	[] => raise NoAnswer
      | SOME x :: _ => x
      | NONE :: xs => first_answer f xs
				   
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
