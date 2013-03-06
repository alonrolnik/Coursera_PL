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

(**** for the challenge problem only ****)


(* note this is "fold left" if order matters 
				     can also do "fold right" *)
fun foldl (f,acc,xs) =
    case xs of 
	[] => acc
      | x::xs' => foldl (f,f(acc,x),xs')

(* fn : string list -> string list *)
fun only_capitals strL =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) strL

(* fn : string list -> string *)
fun longest_string1 strL =
    foldl ((fn (acc, x) => 
	       if String.size(x) > String.size(acc)
	       then x
	       else acc), "" ,strL)

(* fn : string list -> string *)
fun longest_string2 strL =
    foldl ((fn (acc, x) => 
	       if String.size(x) >= String.size(acc)
	       then x
	       else acc), "" ,strL)

(* fn : (int*int -> bool) -> string list -> string *)
fun longest_string_helper f strL =
    foldl ((fn (acc, x) => if f(String.size(x), String.size(acc))
			    then x
			    else acc), "" ,strL)

val longest_string3 = longest_string_helper (fn(x, y) => x > y)
val longest_string4 = longest_string_helper (fn(x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

fun rev_string str = (String.implode o List.rev o String.explode) str

fun first_answer f [] = raise NoAnswer
  | first_answer f (H::T) =
    (case f H of
	 SOME(v) => v
       | NONE => first_answer f T) 

fun all_answers f L = let fun collect ([], acc) = SOME (List.rev(acc))
			    | collect (H::T, acc) =
			      case f H of
				  SOME(lst) => collect (T, lst @ acc)
				| NONE => NONE
		      in
			  collect(L, [])
		      end

val count_wildcards = g (fn() => 1) (fn x => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)
fun count_some_var (s, p) = g (fn () => 0) (fn (x) => if x=s then 1 else 0) p

fun check_pat p =
    let 
	fun list_me p =
   	    case p of
		Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p,i) => list_me(p) @ i ) [] ps
	      | ConstructorP(_,p) => list_me p
	      | _                 => []
	fun is_exists [] = true
	  | is_exists (H::T) =
	    case List.exists (fn(x) => H=x) T of
		true => false
	      | false => is_exists(T)
    in
	is_exists (list_me(p))
    end

fun match (v, p) =
    case (v, p) of
	(_, Wildcard)           => SOME []
      | (_, Variable s)         => SOME [(s, v)]
      | (Unit, UnitP)           => SOME []
      | (Const i, ConstP j)     => if i=j then SOME [] else NONE
      | (Tuple vs, TupleP ps)   => if List.length ps = List.length vs
				   then
				       all_answers (fn (v1, p1) => match(v1, p1)) (ListPair.zip(vs, ps))
				   else NONE
      | (Constructor(s1, v1), ConstructorP(s2, p2)) => if s1=s2 then match(v1 ,p2) else NONE
      | (_, _)                    => NONE

fun first_match v pL =
    SOME (first_answer (fn (p) => match (v, p)) pL)
    handle NoAnswer => NONE
