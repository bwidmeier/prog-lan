(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, source) = 
    let
	fun aux (source, acc) =
	    case source of
		[] => NONE
	      | x :: xs => if x = s 
			   then SOME(acc @ xs) 
			   else aux(xs, x :: acc) 
    in
	aux(source, [])
    end


fun get_substitutions1 (subs : string list list, s : string) = 
    case subs of
	[] => []
      | x :: xs  => case all_except_option(s, x) of
			NONE => get_substitutions1(xs, s)
		      | SOME(rest) => rest @ get_substitutions1 (xs, s)
			

fun get_substitutions2 (subs : string list list, s : string) = 
    let
	fun aux(subs : string list list, acc : string list) = 
	    case subs of
		[] => acc
	      | x :: xs => case all_except_option(s, x) of
			       NONE => aux(xs, acc)
			     | SOME(rest) => aux(xs, rest @ acc)  
    in
	aux(subs, [])
    end


type full_name = {first:string, middle:string, last:string}

fun similar_names (subs : string list list, {first=f, middle=m, last=l} : full_name) = 
    let
	fun aux (subs : string list, acc : full_name list) = 
	    case subs of
		[] => {first=f, middle=m, last=l} :: acc
	      | x :: xs => aux(xs, {first=x, middle=m, last=l} :: acc)
    in
	aux(get_substitutions2(subs, f), [])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit, _) =
    case suit of
	Clubs => Black
      | Spades => Black
      | Hearts => Red
      | Diamonds => Red

fun card_value (_, rank) =
    case rank of
	Num(n) => n
      | Jack => 10
      | Queen => 10
      | King => 10  
      | Ace => 11

fun remove_card (cards, card, e) =
    case all_except_option(card, cards) of
	NONE => raise e
      | SOME(rest) => rest

fun all_same_color (cards) =
    case cards of
	[] => true
      | c :: [] => true
      | c1 :: c2 :: cs => card_color(c1) = card_color(c2) andalso all_same_color(c2 :: cs)

fun sum_cards (cards) =
    let
	fun aux (cards, acc) =
	    case cards of
		[] => acc
	      | c :: cs => aux(cs, acc + card_value(c)) 
    in
	aux(cards, 0)
    end

fun abs (i) =
    if i < 0 then ~i else i

fun score_raw_data (sum, all_same_color, goal) =
    let
	val multiplier = if sum > goal then 3 else 1
	val divisor = if all_same_color then 2 else 1
	val diff = abs(sum - goal)
    in
	(multiplier * diff) div divisor
    end

fun score (cards, goal) = 
    score_raw_data(sum_cards(cards), all_same_color(cards), goal)

fun officiate (deck, moves, goal) = 
    let
	fun aux (deck, moves, hand, sum) = 
	    case (moves, deck) of
		([], _) => score(hand, goal)
	      | (Discard(c) :: ms, _) => aux(deck, ms, remove_card(hand, c, IllegalMove), sum - card_value(c))
	      | (Draw :: ms, []) => score(hand, goal)
	      | (Draw :: ms, c :: cs) => if card_value(c) + sum > goal 
					 then score(c :: hand, goal) 
					 else aux(cs, ms, c :: hand, card_value(c) + sum)
    in
	aux(deck, moves, [], 0)
    end

fun min_card_value (suit, rank) =
    case rank of 
	Ace => 1
      | King => 10
      | Queen => 10
      | Jack => 10   
      | Num n => n

fun count_rank (cards, rank) =
    case cards of
	[] => 0
      | (_, r) :: cs => if rank = r 
			then 1 + count_rank(cards, rank) 
			else count_rank(cards, rank)

fun min (i1, i2) = 
    if i1 < i2
    then i1
    else i2

fun score_challenge (cards, goal) = 
    let
	val same_color = all_same_color(cards)
	fun aux (num_aces, best_score, sum) =
	    case num_aces of
		0 => best_score
	      | n => aux(n - 1, min(best_score, score_raw_data(sum - 10, same_color, goal)), sum - 10)
    in
	aux(count_rank(cards, Ace), score(cards, goal), sum_cards(cards))
    end

fun officiate_challenge (deck, moves, goal) = 
    let
	fun aux (deck, moves, hand, sum) = 
	    case (moves, deck) of
		([], _) => score_challenge(hand, goal)
	      | (Discard(c) :: ms, _) => aux(deck, ms, remove_card(hand, c, IllegalMove), sum - min_card_value(c))
	      | (Draw :: ms, []) => score_challenge(hand, goal)
	      | (Draw :: ms, c :: cs) => if min_card_value(c) + sum > goal 
					 then score(c :: hand, goal) 
					 else aux(cs, ms, c :: hand, min_card_value(c) + sum)
    in
	aux(deck, moves, [], 0)
    end
