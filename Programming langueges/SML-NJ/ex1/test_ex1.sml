(* this is a test banch for ex1.sml *)
(* written by Alon Rolnik *)
use ("ex1.sml");
val dates = [(1999, 12, 2), (2008, 3, 12), (1995 ,4, 15), (1945, 2, 12), (2013, 1, 1), (2013, 1, 2), (2008, 3, 12), (2011,3, 20)];

val month = [1,2,5,4,7,3,8,9,12,11,10];
val numbers = [3, 23, 56, 43, 6, 7, 8, 10, 3, 2, 5, 78];

"--------------q1----------------";
is_older((1999, 12, 2), (2008, 3, 12));  (* true *)
is_older((2008, 3, 12), (2013, 1, 2));  (* true *)
is_older((2008, 3, 12), (2011,3, 20));  (* true *)
is_older((1995 ,4, 15), (1945, 2, 12));  (* false *)

"--------------q2----------------";
number_in_month(dates, 3); (* 3 *)
number_in_month(dates, 4); (* 1 *)
number_in_month(dates, 7); (* 0 *)
number_in_month(dates, 9); (* 0 *)

"--------------q3----------------";
number_in_months(dates, month);

"--------------q4----------------";
dates_in_month(dates, 3);
dates_in_month(dates, 4); 
dates_in_month(dates, 9);

"--------------q5----------------";
dates_in_months(dates, month);

"--------------q6----------------";
get_nth(dates_in_string, 3);(* march *)
get_nth(dates_in_string, 9);(* sep *)
get_nth(dates_in_string, 7);(* jul *)
get_nth(dates_in_string, 5);(* may *)

"--------------q7----------------";
date_to_string((2013,1,20));

"--------------q8----------------";
number_before_reaching_sum(56, numbers); (* 2 *)
number_before_reaching_sum(135, numbers); (* 5 *)

"--------------q9----------------";
what_month(245); (* 9 *)
what_month(2); (* 1 *)
what_month(24); (* 1 *)
what_month(59); (* 2 *)
what_month(75); (* 3 *)
what_month(100); (* 4 *)

"--------------q10----------------";
month_range(1, 365);(* [1,1,1,....] *) 
month_range(1, 100);(* [1,1,1...,2,2,2,2,2....] *) 
month_range(50, 100);(* [2,2,2,.....,3,3,3......4,4....] *) 

"--------------q11----------------";
oldest(dates); (*  (1945, 2, 12) option *)
