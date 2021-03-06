(* written by Alon Rolnik *)
(* 2013.01.19 *)
(* ex 1 in coursera programming languages *)

(***************************************************)
(*************** helper values *********************)
(***************************************************)

val dates_in_string = ["January", "February", "March", "April",
		       "May", "June", "July", "August", "September",
		       "October", "November", "December"];
val days_in_month = [31, 28, 31, 30,
		     31, 30, 31, 31,
		     30, 31, 30, 31];

(***************************************************)
(********** end of helper values *******************)
(***************************************************)

(* 1 *)
(* checks if date1 is older then date 2 *)
(* fn : (int * int * int) * (int * int * int) -> bool *)
fun is_older(date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) < (#1 date2)
    then true
    else if  (#1 date1) > (#1 date2)
    then false
    else if (#2 date1) < (#2 date2)
    then true
    else if (#2 date1) > (#2 date2)
    then false
    else if (#3 date1) < (#3 date2)
    then true
    else false

(* 2 *)
(* count the times that month apear in the list of dates *)
(* fn : (int * int * int) list * int -> int *)
fun number_in_month(date_list : (int*int*int) list, month :int) =
    if null(date_list)
    then 0
    else if (#2 (hd(date_list))) <> month
    then number_in_month(tl(date_list), month)
    else 1 + number_in_month(tl(date_list), month)

(* 3 *)
(* count the number of dates in the list of dates that are in any of the months in the list of months *)
(* fn : (int*int*int) list * int list -> int *)
fun number_in_months(date_list : (int*int*int) list, month_list :  int list) =
    if null(month_list)
    then 0
    else number_in_month(date_list, hd(month_list)) + number_in_months(date_list, tl(month_list))


(* 4 *)(*
	fun dates_in_month (date_list : (int*int*int) list * month : int) =
	    if null (date_list)
	    then []
	    else if (#2 (hd(date_list))) <> month
	    then dates_in_month(tl(date_list), month)
	    else (hd(date_list)) :: dates_in_month(tl(date_list), month
	*)

(* 4 *)
(* fn : (int*int*int) list * int -> (int*int*int) list *)
fun dates_in_month (date_list : (int*int*int) list, month : int) =
    let fun dates_in_month (date_list : (int*int*int) list) =
	    if null (date_list)
	    then []
	    else if (#2 (hd(date_list))) <> month
	    then dates_in_month(tl(date_list))
	    else (hd(date_list)) :: dates_in_month(tl(date_list))
    in dates_in_month(date_list)
    end

(*
 (* 5 *)
 (* dates_in_month : (int*int*int) list * int list int -> (int*int*int) list *)
 fun dates_in_months(date_list : (int*int*int) list, month_list :  int list) =
     if null(month_list)
     then []
     else dates_in_month(date_list, hd(month_list)) @ dates_in_months(date_list, tl(month_list))
 *)

(* 5 *)
(* fn : (int*int*int) list * int list int -> (int*int*int) list *)
fun dates_in_months(date_list : (int*int*int) list, month_list :  int list) =
    let fun dates_in_months(month_list :  int list) =
	    if null(month_list)
	    then []
	    else dates_in_month(date_list, hd(month_list)) @ dates_in_months(tl(month_list))
    in dates_in_months(month_list)
    end

(* TO-DO: case where n > length(list) *)
(* 6 *)
(* fn : string list * int -> string *)
fun get_nth(string_list : string list, n : int) = 
    if n = 1
    then hd(string_list)
    else get_nth(tl(string_list), n-1)

(* 7 *)
(* fn : (int*int*int) -> string *)
fun date_to_string(date : (int*int*int)) = 
    get_nth(dates_in_string, #2 date) ^ " " ^ Int.toString( #3 date) ^ ", " ^ Int.toString(#1 date)

(* 8 *)
(* fn : int list * int -> int *)
fun number_before_reaching_sum(sum : int , list : int list) = 
    let fun number_before_reaching_sum(list : int list, total : int, counter : int) =
	    if (total + hd(list)) >= sum
	    then counter
	    else number_before_reaching_sum(tl(list), total + hd(list), counter+1)
    in
	number_before_reaching_sum(list, 0, 0)
    end

(* 9 *)
(* fn : int -> int *)
fun what_month(day : int) =
    number_before_reaching_sum( day, days_in_month) + 1

(* 10 *)
(* fn : int*int -> int list *)
fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else if day1 = day2 then [what_month(day1)]
    else [what_month(day1)] @ month_range(day1+1, day2)

(* 11 *)
(* fn : (int*int*int) list -> (int*int*int) option *)
fun oldest(dates : (int*int*int) list) =
    if null(dates)
    then NONE
    else if null(tl(dates))
    then SOME (hd(dates))
    else if is_older(hd(dates), hd(tl(dates)))
    then oldest([hd(dates)] @ tl(tl(dates)))
    else oldest(tl(dates))
