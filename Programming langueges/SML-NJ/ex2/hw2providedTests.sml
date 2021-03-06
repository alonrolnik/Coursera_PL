(* Dan Grossman, Coursera PL, HW2 Provided Tests *)
use "hw2provided.sml";



(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

fun test_score () =
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
in score([], 42)
end


fun test_sum_cards () =
sum_cards([]);

fun test3 () = (* should return 4 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Discard(Spades,Ace),Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

fun test4 () = (* should return 4 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Spades, King)]
	val moves = [Draw,Draw,Discard(Spades,Ace),Draw,Draw, Draw]
    in
 	officiate(cards,moves,44)
    end

fun test_remove_card () = (* should return 4 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Hearts, King)]
	val moves = [Draw,Draw,Discard(Spades,Ace),Draw,Draw, Draw]
    in
 	remove_card(cards,(Spades, Ace), IllegalMove)
    end
