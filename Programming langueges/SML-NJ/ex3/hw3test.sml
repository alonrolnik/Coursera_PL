use "hw3provided.sml";

if match(Unit, UnitP) = SOME [] then "PASS" else "FAIL";

if match(Unit, ConstP 1) = NONE then "PASS" else "FAIL";

if match(Const 1, UnitP) = NONE then "PASS" else "FAIL";

if match(Const 1, Wildcard) = SOME [] then "PASS" else "FAIL";

if match(Const 1, Variable "xyz") = SOME [("xyz",Const 1)] then "PASS" else "FAIL";

if match(Unit, Variable "xyz") = SOME [("xyz",Unit)] then "PASS" else "FAIL";

if match(Tuple [], Variable "xyz") = SOME [("xyz", Tuple[])] then "PASS" else "FAIL";

if match(Constructor ("abc", Const 1), Variable "xyz") = SOME [("xyz", Constructor("abc", Const 1))] then "PASS" else "FAIL";

if match(Constructor ("abc", Const 1), ConstructorP("xyz", ConstP 2)) = NONE then "PASS" else "FAIL";

if match(Constructor ("abc", Const 1), ConstructorP("abc", ConstP 2)) = NONE then "PASS" else "FAIL";

if match(Constructor ("abc", Const 1), ConstructorP("abc", ConstP 1)) = SOME [] then "PASS" else "FAIL";

if match(Constructor ("abc", Const 1), ConstructorP("abc", Variable "xyz")) = SOME [("xyz", Const 1)] then "PASS" else "FAIL";

if match(Constructor ("a", Constructor("b", Unit)), ConstructorP("a", ConstructorP("b", UnitP))) = SOME [] then "PASS" else "FAIL";

if match(Tuple [Const 1, Unit, Const 2], TupleP [ConstP 1, UnitP, Wildcard]) = SOME [] then "PASS" else "FAIL";

if match(Tuple [Const 1, Unit, Const 2], TupleP [Variable "a", Variable "b", ConstP 2]) = SOME [("a", Const 1),("b", Unit)] then "PASS" else "FAIL";

if match(Tuple [Const 1, Tuple [Unit, Const 3], Const 2], TupleP [Variable "a", Variable "b", ConstP 2]) = SOME [("a", Const 1),("b", Tuple[Unit, Const 3])] then "PASS" else "FAIL";

if match(Tuple [Const 1, Tuple [Unit, Const 3], Const 2], TupleP [Variable "a", TupleP [Variable "b", Wildcard], ConstP 2]) = SOME [("a",Const 1),("b",Unit)] then "PASS" else "FAIL";

exception NotPassed
fun have_same_items l1 l2 = (* return true if l1 has the same items than l2 *)
    case l1 of
    head :: tail => not (List.exists (fn x => x = head) l2) 
                 orelse have_same_items tail l2
      | []           => true

fun same_items (lo1, lo2) =
    case (lo1, lo2) of
    (NONE, NONE)       => true
      | (SOME l1, SOME l2) => have_same_items l1 l2
      | _                  => false
fun test_match () =
  let val tests =
  [
   same_items(match(Const 10, Wildcard), SOME []),
   same_items(match(Unit, Wildcard), SOME []),
   same_items(match(Constructor("Test", Unit), Wildcard), SOME []),
   same_items(match(Tuple [Unit, Const 10], Wildcard), SOME []),
   same_items(match(Unit, UnitP), SOME []),
   same_items(match(Const 10, ConstP 10), SOME []),
   same_items(match(Const 10, ConstP 20), NONE),
   same_items(match(Const 10, Variable "x"), SOME [("x",Const 10)]),
   same_items(match(Constructor("Test", Const 35),
            ConstructorP("Test", Variable "y")), SOME [("y",Const 35)]),
   same_items(match(Constructor("Test", Const 35), 
            ConstructorP("Fail", Variable "y")), NONE),

   same_items(match(Tuple([Const 1, Const 2]), 
            TupleP([Variable "x", Variable "y", Variable "z"])), NONE),
   same_items(match(Tuple [Const 1, Const 2, Const 3, Const 4], 
            TupleP [Variable "w",Variable "x", Variable "y", 
                Variable "z"]), 
          SOME [("z",Const 4),("y",Const 3),("x",Const 2),("w",Const 1)]),
   same_items(match(Tuple [Const 1, Const 2, Const 3, Const 4], 
            TupleP [ConstP 1,Variable "x", ConstP 3, Variable "z"]),
          SOME [("z",Const 4),("x",Const 2)]),
   same_items(
     match(Constructor("A", 
               Tuple([Unit, Const 10, Const 20, 
                  Tuple([Unit, 
                     Constructor("B", Const 30)])])), 
       ConstructorP("A", 
            TupleP([UnitP, Variable "x", ConstP 20, 
                TupleP([UnitP, 
                    ConstructorP("B", 
                             Variable "y")])]))),
     SOME [("y", Const 30), ("x", Const 10)])
    ]
    in
    List.all (fn x => x) tests orelse raise NotPassed
    end

val test_q11 = test_match()

fun test_first_match () =
    (first_match (Const 10) [ConstP 10, Variable "x"] = SOME [] 
     andalso
     first_match (Const 10) [Variable "x", ConstP 10] = SOME [("x",Const 10)] 
     andalso
     first_match (Const 10) [Variable "x", Variable "y"] = SOME [("x",Const 10)]
     andalso
     first_match (Const 10) [UnitP, ConstructorP("Test", ConstP 10), Wildcard, 
                 Variable "y", ConstP 10] = SOME [] 
     andalso
     first_match (Const 10) [UnitP, ConstructorP("Test", ConstP 10), 
                 Variable "y", Wildcard, ConstP 10] = 
       SOME [("y", Const 10)]
     andalso first_match (Const 10) [UnitP] = NONE
     andalso
     first_match (Constructor ("foo", Unit)) [ConstructorP("foo", UnitP)] =
       SOME []
     andalso
     first_match (Constructor ("foo", Unit)) [ConstructorP("bar", UnitP)] = NONE
    )
    orelse raise NotPassed


val test_q12 = test_first_match()

(* `HW3` Tests *)
(* Problem 1 *)
val names = ["Andy","Bernard","Cathy","Doug","Elaine","Fred","Greg","Harry","Isis","Jack","Karen","Laura","Mike","Nancy","Ophelia","Petunia","Quincy","Ray","Supercalifragilisticexpialidocious","Tom","Uma","Velma","Wilma","Xan","Yan","Zed"]

(* names2 is same as names, but longest word is lower case *)
val names2 = ["Andy","Bernard","Cathy","Doug","Elaine","Fred","Greg","Harry","Isis","Jack","Karen","Laura","Mike","Nancy","Ophelia","Petunia","Quincy","Ray","supercalifragilisticexpialidocious","Tom","Uma","Velma","Wilma","Xan","Yan","Zed"]

val names3 = ["andy","bernard","cathy","doug","elaine"]

val empty = []
val list1b = ["Andy","bernard","Cindy"]
val list1c = ["andy","bernard","Cindy"]
val list1d = ["andy"]

val test1a = only_capitals(empty)=[]
val test1b = only_capitals(list1b)=["Andy","Cindy"]
val test1c = only_capitals(list1c)=["Cindy"]
val test1d = only_capitals(list1d)=[]

(* Problem 2 *)
val list2a = ["Andy","Billy","Blair"]

val test2a = longest_string1(empty) = ""
val test2b = longest_string1(list1b) = "bernard"
val test2c = longest_string1(names) = "Supercalifragilisticexpialidocious"
val test2d = longest_string1(list2a) = "Billy"

(* Problem 3 *)
val test3a = longest_string2(empty) = ""
val test3b = longest_string2(list1b) = "bernard"
val test3c = longest_string2(names) = "Supercalifragilisticexpialidocious"
val test3d = longest_string2(list2a) = "Blair"

(* Problem 4 *)
val test4a = longest_string3 empty = ""
val test4b = longest_string3 list1b = "bernard"
val test4c = longest_string3 names = "Supercalifragilisticexpialidocious"
val test4d = longest_string3 list2a = "Billy"

val test4e = longest_string4 empty = ""
val test4f = longest_string4 list1b = "bernard"
val test4g = longest_string4 names = "Supercalifragilisticexpialidocious"
val test4h = longest_string4 list2a = "Blair"

(* Problem 5 *)
val test5a = longest_capitalized(names) = "Supercalifragilisticexpialidocious"
val test5b = longest_capitalized(names2) = "Bernard"
val test5c = longest_capitalized(names3) = ""
val test5d = longest_capitalized([]) = ""

(* Problem 7, courtesy of Wan Kong Yew *)
fun is_even x =
    if x mod 2 = 0 then SOME([x]) else NONE

val first_answer_test1 = first_answer is_even [1, 2, 3, 4, 5, 6] = [2];
val first_answer_test2= (first_answer is_even [1, 3, 5] handle NoAnswer => [0]) = [0];
val first_answer_test3 = first_answer is_even [1, 3, 5, 6] = [6];
val first_answer_test4 = (first_answer is_even [] handle NoAnswer => [0]) = [0];

 (* Problem 8, courtesy of Wan Kong Yew *)
val all_answers_test1 = all_answers is_even [1, 2, 3, 4, 5, 6] = NONE;
val all_answers_test2 = all_answers is_even [2, 4, 6] = SOME ([2, 4, 6]);
val all_answers_test3 = all_answers is_even [] = SOME ([]);
