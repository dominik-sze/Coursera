(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_0 = all_except_option("string", [])                       = NONE
val test1_1 = all_except_option("string", ["string"])               = SOME []
val test1_2 = all_except_option("string", ["string", "foo", "bar"]) = SOME(["foo", "bar"])
val test1_3 = all_except_option("foo", ["string", "foo", "bar"])    = SOME(["string", "bar"])
val test1_4 = all_except_option("bar", ["string", "foo", "bar"])    = SOME(["string", "foo"])
val test1_5 = all_except_option("aaa", ["string", "foo", "bar"])    = NONE

val test2_0 = get_substitutions1([], "foo")                        = []
val test2_1 = get_substitutions1([["foo"],["there"]], "baz")       = []
val test2_2 = get_substitutions1([["foo"],["there"]], "foo")       = []
val test2_3 = get_substitutions1([["foo","bar"],["there"]], "foo") = ["bar"]
val test2_4 = get_substitutions1([["baz","foo","bar"],["there"],["nothing","sth"],["aaa","foo"],["foo"]], "foo") = ["baz","bar","aaa"]

val test3_0 = get_substitutions2([], "foo")                        = []
val test3_1 = get_substitutions2([["foo"],["there"]], "baz")       = []
val test3_2 = get_substitutions2([["foo"],["there"]], "foo")       = []
val test3_3 = get_substitutions2([["foo","bar"],["there"]], "foo") = ["bar"]
val test3_4 = get_substitutions2([["baz","foo","bar"],["there"],["nothing","sth"],["aaa","foo"],["foo"]], "foo") = ["baz","bar","aaa"]

val test4_0 = similar_names([], {first="Fred", middle="W", last="Smith"})                                  = [{first="Fred", last="Smith", middle="W"}]
val test4_1 = similar_names([["George", "Greg"]], {first="George", middle="W",last="Bush"})                = [{first="George", last="Bush", middle="W"},{first="Greg", last="Bush",middle="W"}]
val test4_2 = similar_names([["George", "Greg"],["C", "D"],["Al", "Ed"]], {first="A",middle="W",last="F"}) = [{first="A", last="F", middle="W"}]
val test4_3 = similar_names([["George", "Greg"],["C", "D"],["Al", "Ed"]], {first="C",middle="W",last="F"}) = [{first="C", last="F", middle="W"},{first="D", last="F", middle="W"}]
val test4_4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	      [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	       {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5_0 = card_color((Clubs,    Num 2)) = Black
val test5_1 = card_color((Spades,   Num 2)) = Black
val test5_2 = card_color((Hearts,   Num 2)) = Red
val test5_3 = card_color((Diamonds, Num 2)) = Red

val test6_0 = card_value((Clubs, Num 2 )) =  2
val test6_1 = card_value((Clubs, Num 5 )) =  5
val test6_2 = card_value((Clubs, Num 10)) = 10
val test6_3 = card_value((Clubs, Jack  )) = 10 
val test6_4 = card_value((Clubs, Queen )) = 10 
val test6_5 = card_value((Clubs, King  )) = 10 
val test6_6 = card_value((Clubs, Ace   )) = 11 

val test7_0 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove)                                  = []
val test7_1 = remove_card([(Hearts, Ace),(Clubs, Num 2)], (Hearts, Ace), IllegalMove)                   = [(Clubs, Num 2)]
val test7_2 = remove_card([(Hearts, Ace),(Clubs, Num 2),(Hearts, Queen)], (Hearts, Queen), IllegalMove) = [(Hearts, Ace),(Clubs, Num 2)]
val test7_3 = (remove_card([], (Hearts, Ace), IllegalMove); false) handle IllegalMove => true
val test7_4 = (remove_card([(Hearts, Ace),(Clubs, Num 2),(Hearts, Queen)], (Diamonds, Num 8), IllegalMove); false) handle IllegalMove => true

val test8_0 = all_same_color([])                                                = true
val test8_1 = all_same_color([(Clubs, Num 4)])                                  = true
val test8_2 = all_same_color([(Hearts, Ace), (Diamonds, Ace)])                  = true
val test8_3 = all_same_color([(Hearts, Ace), (Hearts, Ace)])                    = true
val test8_4 = all_same_color([(Hearts, Ace), (Hearts, Ace), (Diamonds, Queen)]) = true
val test8_5 = all_same_color([(Hearts, Ace), (Clubs, Ace), (Diamonds, Queen)])  = false
val test8_6 = all_same_color([(Spades, Ace), (Clubs, Ace), (Spades, Queen)])    = true

val test9_0 = sum_cards([])                                                     =  0
val test9_1 = sum_cards([(Clubs, Ace)])                                         = 11
val test9_2 = sum_cards([(Clubs, Queen)])                                       = 10
val test9_3 = sum_cards([(Clubs, Num 10)])                                      = 10
val test9_4 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)])                        =  4
val test9_5 = sum_cards([(Clubs, Ace),(Clubs, Num 2)])                          = 13
val test9_6 = sum_cards([(Clubs, Ace),(Clubs, Ace),(Clubs, Ace),(Clubs, King)]) = 43

val test10_0 = score([(Hearts, Ace)], 10)                     =  1
val test10_1 = score([(Hearts, Num 2),(Clubs, Num 4)], 10)    =  4
val test10_2 = score([(Hearts, Num 6),(Clubs, Num 4)], 10)    =  0
val test10_3 = score([(Hearts, Num 2),(Hearts, Num 4)], 10)   =  2
val test10_4 = score([(Hearts, Num 2),(Diamonds, Num 4)], 10) =  2
val test10_5 = score([(Clubs, Ace),(Diamonds, Ace)],12)       = 30

val test11_0 = officiate([(Hearts, Num 2),(Clubs, Num 4)], [Draw], 15)                                          =  6
val test11_1 = officiate([(Hearts, Num 2)], [Draw], 15)                                                         =  6
val test11_2 = officiate([(Spades,Queen),(Hearts, Num 2)], [Draw,Draw], 12)                                     =  0
val test11_3 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42)   =  3
val test11_4 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw,Draw,Draw,Draw], 42)   =  6
val test11_5 = officiate([(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42)                            = 10
val test11_6 = officiate([(Clubs,Ace),(Hearts,Ace)], [Draw,Draw,Draw,Draw,Draw], 42)                            = 20
val test11_7 = officiate([(Clubs,Ace),(Spades,Queen),(Hearts, Num 2)], [Draw,Discard(Clubs,Ace),Draw,Draw], 12) =  0
val test11_8 = officiate([(Clubs,Ace),(Spades,Queen),(Hearts, Num 2)], [Draw,Discard(Clubs,Ace),Draw,Discard(Spades,Queen),Draw], 12) = 5
val test11_9 = ((officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42);
               false) handle IllegalMove => true)

val test12_0 = score_challenge([(Clubs,Ace)], 11)                                = 0
val test12_1 = score_challenge([(Diamonds,Queen),(Clubs,Ace)], 20)               = 3
val test12_2 = score_challenge([(Clubs,Queen),(Clubs,Ace)], 20)                  = 1
val test12_3 = score_challenge([(Clubs,Queen),(Clubs,Ace)], 11)                  = 0
val test12_4 = score_challenge([(Clubs,Queen),(Clubs,Ace),(Spades,Ace)], 22)     = 0
val test12_5 = score_challenge([(Clubs,Num 3),(Hearts,Ace)], 17)                 = 3
val test12_6 = score_challenge([(Clubs,Num 3),(Clubs,Ace)], 17)                  = 1
val test12_7 = score_challenge([(Clubs,Num 3),(Clubs,Ace),(Hearts,Ace)], 17)     = 2
val test12_8 = score_challenge([(Clubs,Num 3),(Clubs,Ace),(Clubs,Ace)], 17)      = 1
val test12_9 = score_challenge([(Clubs,Num 3),(Clubs,Queen),(Clubs,King)], 17)   = 9
val test12_10 = score_challenge([(Clubs,Num 3),(Clubs,Queen),(Hearts,King)], 17) = 18
val test12_11 = score_challenge([(Clubs,Ace),(Clubs,Ace),(Hearts,Ace)], 3)       = 0
val test12_12 = score_challenge([(Clubs,Ace),(Clubs,Ace),(Clubs,Ace)], 4)        = 0
