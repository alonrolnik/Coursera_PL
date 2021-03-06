fun reverse (List) =
    let fun reverse ([], result) = result
	  | reverse (H::T, result) =
	    reverse(T, H::result)
    in reverse(List, [])
    end

fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str : string, strlst : string list) =
    let fun all_except_option ([], result) =  NONE
	  | all_except_option (H::T, result) =
	    case  same_string(str, H) of
		true=> SOME(reverse(result) @ T)
	      | false=> all_except_option(T, H::result)
    in
	all_except_option(strlst, [])
    end

fun get_substitutions1 (strlst : string list list, str : string)=
    let fun get_subs ([], result) = result
	  | get_subs (H::T, temp_result) =
	    case all_except_option(str, H) of
		NONE => get_subs(T, temp_result)
	      | SOME(result) => get_subs(T, temp_result @ result)
    in
	get_subs(strlst, [])
    end


fun get_substitutions2 (strlst : string list list, str : string)=
    let fun get_subs ([], result) = result
	  | get_subs (H::T, temp_result) =
	    case all_except_option(str, H) of
		NONE => get_subs(T, temp_result)
	      | SOME(result) => get_subs(T, temp_result @ result)
    in
	get_subs(strlst, [])
    end


fun similar_names (strlst: string list list, {first=F, middle=M, last=L}:{first: string, middle: string, last:string})=
    let fun similar_names ([], result) = {first=F, middle=M, last=L}::reverse(result)
	  | similar_names (H::T, result)=
	    similar_names (T, {first=H,
			       middle= M,
			       last= L}::result)
    in
	similar_names(get_substitutions1(strlst, F), [])
    end


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color ((suit,_) : card) =
    case suit of
	Clubs => Black
      |Spades => Black
      |_=> Red

fun card_value ((_,rank): card) =
    case rank of
	Num(num) => num
      |Ace => 11
      |_ => 10 

fun remove_card (cs : card list, c : card, e) =
    let fun remove_card([], result, flag) =
	    if flag = true then raise e
	    else result
	  | remove_card(H::T, result, flag) =
	    if H = c then remove_card([], reverse result @ T , false)
	    else remove_card(T, H::result, true)
    in remove_card(cs, [], true)
    end

fun all_same_color ([] : card list) = true
  | all_same_color(card2::[] : card list) = true
  | all_same_color(card1::card2::T : card list) =
    if card_color(card1) = card_color(card2)
    then all_same_color(card2::T : card list)
    else false

fun sum_cards (cards : card list) =
    let fun sum_cards([], result) = result
	  | sum_cards(H::T, result) = sum_cards(T, result + card_value(H))
    in
	sum_cards(cards, 0)
    end

fun score (cards : card list, goal : int) =
    let val sum = sum_cards(cards)
	val prelimanry = 
	    if (sum > goal) 
	    then (3*(sum-goal))
	    else (goal-sum)
    in
	if all_same_color(cards) 
	then prelimanry div 2
	else prelimanry
    end

fun officiate (cards : card list, moves : move list, goal : int) =
    let fun state([], cardH::cardT, moveH::moveT) =
	    (case moveH of
		 Draw => state(cardH::[], cardT, moveT)
	       | _ => raise IllegalMove)

	  | state(heldCards, cardsList, []) = score(heldCards, goal)
	  | state(heldCards, [], moves) = score(heldCards, goal)
	  | state(heldCards, cardH::cardT, moveH::moveT) =
	    (case moveH of
		 Draw => if sum_cards(cardH::heldCards) > goal
			 then score(cardH::heldCards, goal)
			 else state(cardH::heldCards, cardT, moveT)
	       | Discard(myCard) => state(remove_card(heldCards ,myCard, IllegalMove),cardH::cardT, moveT))
    in
	state([], cards, moves)
    end
