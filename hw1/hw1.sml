fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if #1 date1 > #1 date2
    then false
    else if #2 date1 > #2 date2
    then false
    else #3 date1 < #3 date2

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if month = #2 (hd dates)
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months (dates: (int * int * int) list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then dates
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates: (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (source : string list, n : int) =
    if n = 1
    then hd source
    else get_nth(tl source, n - 1)

fun get_nth_int (source : int list, n : int) =
    if n = 1
    then hd source
    else get_nth_int(tl source, n - 1)

fun date_to_string (date: int * int * int) =
    let 
	val month_list =  ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(month_list, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, source : int list) =   
    let
	fun helper (acc : int, sum : int, source : int list) =
	    let
		val head = hd source
	    in
		if head >= sum
		then acc
		else helper(acc + 1, sum - head, tl source)
	    end
    in
	helper(0, sum, source)
    end

val month_lengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun what_month (day_of_year : int) =
    number_before_reaching_sum(day_of_year, month_lengths) + 1

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else 
	let
	    val oldest_of_rest = oldest(tl dates)
	    val head = hd dates
	in
	    if isSome oldest_of_rest andalso is_older(valOf oldest_of_rest, head)
	    then oldest_of_rest
	    else SOME(head)
	end

fun contains (source : int list, elem : int) =
    if null source
    then false
    else if hd source = elem
    then true
    else contains(tl source, elem)

fun reverse (source : int list) =
    let
	fun helper(acc : int list, source : int list) = 
	    if null source
	    then acc
	    else helper((hd source) :: acc, tl source)
    in
	helper([], source)
    end

fun distinct (source : int list) =
    let
	fun helper(acc : int list, source : int list) =
	    if null source 
	    then source
	    else if not(contains(acc, hd source))
	    then helper((hd source) :: acc, tl source)
	    else helper(acc, tl source)
    in
	reverse(helper([], source))
    end

fun distinct2 (source : int list) = 
    if null source
    then source
    else let 
	val rest = distinct2(tl source)
	val head = hd source
    in
	if contains(rest, head)
	then rest
	else head :: rest
    end

fun number_in_months_challenge (source : (int * int * int) list, months : int list) =
    number_in_months(source, distinct2(months))

fun dates_in_months_challenge (source : (int * int * int) list, months : int list) =
    dates_in_months(source, distinct2(months))

fun get_max_day_for_month_year (month : int, year : int) = 
    if month = 2 andalso (year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0))
    then 29
    else get_nth_int(month_lengths, month)

fun reasonable_date (date : (int * int * int)) = 
    let
	val year = #1 date
	val month = #2 date
	val day = #3 date
	val max_day = get_max_day_for_month_year(month, year)
    in
	year >= 1 andalso month >= 1 andalso month <= 12 andalso day >= 0 andalso day <= max_day
    end
