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

(**** you can put all your code here ****)

fun only_capitals xs =
  List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
  foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2) "" xs

fun longest_string2 xs =
  foldl (fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2) "" xs

fun longest_string_helper f xs =
  foldl (fn (s1, s2) => if f(String.size(s1), String.size(s2)) then s1 else s2) "" xs

val longest_string3 = longest_string_helper (fn(x, y) => x > y)

val longest_string4 = longest_string_helper (fn(x, y) => x >= y)

val longest_capitalized = foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) andalso Char.isUpper(String.sub(s1, 0)) then s1 else s2) ""

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
  case xs of
       []     => raise NoAnswer
     | x::xs' => case f x of
                      NONE   => first_answer f xs'
                    | SOME y => y

fun all_answers f ys =
  let fun aux([], acc)     = SOME acc
        | aux(x::xs', acc) = case f x of
                                  NONE => NONE
                                | SOME y => aux(xs', acc @ y)
  in
    aux(ys, [])
  end

fun count_wildcards p =
  g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
  g (fn () => 1) (fn x => String.size x) p

fun count_some_var(str, p) =
  g (fn () => 0) (fn x => if x = str then 1 else 0) p


fun check_pat p =
  let
     fun all_vals(p, acc) =
       case p of
            Variable x        => acc @ [x]
          | TupleP ps         => foldl (fn (p1, p2) => p2 @ all_vals(p1, acc)) acc ps
          | ConstructorP(_,p) => all_vals(p, acc)
          | _                 => []
     fun unique []       = true
       | unique (x::xs') = if List.exists (fn y => y = x) xs'
                           then false
                           else unique xs'
  in
    unique(all_vals(p, []))
  end

fun match e =
  case e of
       (_, Wildcard) => SOME []
     | (Unit, UnitP) => SOME []
     | (Const v, ConstP p) => if v = p 
                              then SOME [] 
                              else NONE
     | (x, Variable s) => SOME [(s, x)]
     | (Tuple(ps), TupleP(vs)) => if List.length(ps) = List.length(vs) 
                                  then all_answers match (ListPair.zip(ps, vs))
                                  else NONE
     | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 
                                                    then match(v, p)
                                                    else NONE
     | (_, _) => NONE
 
fun first_match v ps =
  case ps of
       [] => NONE
     | p::ps' => SOME(first_answer match [(v,p)]) handle NoAnswer => first_match v ps'

