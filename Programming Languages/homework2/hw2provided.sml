(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s1 : string, xs : string list) = 
  case xs of
       []     => NONE
     | x::xs' => if same_string(x, s1)
                 then SOME xs'
                 else case all_except_option(s1, xs') of
                           NONE   => NONE
                         | SOME y => SOME (x::y)

fun get_substitutions1(xs : string list list, s1 : string) =
  case xs of
       []     => []
     | x::xs' => case all_except_option(s1, x) of
                      NONE   => get_substitutions1(xs', s1)
                    | SOME y => y @ get_substitutions1(xs', s1)

fun get_substitutions2(xs : string list list, s1 : string) =
  let fun aux(ys, acc) = 
        case ys of
             []     => acc
           | y::ys' => case all_except_option(s1, y) of
                            NONE   => aux(ys', acc)
                          | SOME z => aux(ys', acc @ z)
  in
    aux(xs, [])
  end

fun similar_names(subs : string list list, name : {first : string, middle : string, last : string}) =
  let fun get_first(s : string) =
        case name of
             {first=a, middle=b, last=c} => {first=s, middle=b, last=c}
      fun full_names(names : string list) =
        case names of
             [] => []
           | s::ss' => get_first(s) :: full_names(ss')
  in name :: full_names(get_substitutions2(subs, case name of {first=f, middle=m, last=l} => f)) 
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

fun card_color the_card =
  case the_card of
       (Clubs,    _) => Black
     | (Spades,   _) => Black
     | (Hearts,   _) => Red
     | (Diamonds, _) => Red

fun card_value the_card = 
  case the_card of
       (_, Num x) => x
     | (_, Ace  ) => 11
     | (_, _    ) => 10

fun remove_card(cards, card, e) =
  let fun remove(cs) =
        case cs of
             []     => raise e
           | c::cs' => if c = card
                       then cs'
                       else c::remove(cs')
  in
    remove(cards)
  end

fun all_same_color cards =
  case cards of
       []       => true
     | x::[]    => true
     | x::y::cs => card_color(x) = card_color(y) andalso all_same_color(y::cs)

fun sum_cards cards =
  let fun aux(hand, acc) =
        case hand of
             [] => acc
           | c::cs => aux(cs, acc + card_value(c))
  in
    aux(cards, 0)
  end

fun score(cards, goal) =
  let 
    val sum    = sum_cards(cards)
    val result = if sum > goal then 3*(sum - goal) else goal - sum
    val score  = if all_same_color(cards) then result div 2 else result
  in
    score
  end 

fun officiate(deck_cards, moves, goal) =
  let fun make_move(deck, hand, moves) =
        if sum_cards(hand) > goal
        then score(hand, goal)
        else case moves of
                  []    => score(hand, goal)
                | m::mv => case m of
                                Discard c => make_move(deck, remove_card(hand, c, IllegalMove), mv)
                              | Draw      => case deck of
                                                  []    => score(hand, goal)
                                                | d::dc => make_move(dc, d::hand, mv)
  in
    make_move(deck_cards, [], moves)
  end

fun score_challenge(cards, goal) =
  let fun replace_ace(cs) = 
        case cs of
             []     => []
           | c::cs' => case c of
                            (color, Ace) => (color, Num 1)::cs'
                          | (_, _)       => c :: replace_ace(cs')
      fun best_result(old_hand) =
        let val old_score = score(old_hand, goal)
            val new_hand  = replace_ace(old_hand)
            val new_score = score(new_hand, goal)
        in
          if new_score < old_score
          then best_result(new_hand)
          else old_score
        end
  in
    best_result(cards)
  end
