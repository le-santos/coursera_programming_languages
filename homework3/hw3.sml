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

fun only_capitals (strings) =
    List.filter(fn x => Char.isUpper(String.sub(x, 0))) strings

fun longest_string1 (strings) =
  let
    fun compare (a,b) = if String.size(a) > String.size(b) then a else b
  in
    List.foldl(compare) "" strings
  end

fun longest_string2 (strings) =
  let
    fun compare (a,b) = if String.size(a) >= String.size(b) then a else b
  in
    List.foldl(compare) "" strings
  end

fun longest_string_helper(compare_fn, list) =
  List.foldl(fn (s, acc) => if compare_fn(String.size(s), String.size(acc)) then s else acc) "" list

fun longest_string3(strings) =
  let
    val comparison = fn (a,b) => a > b
  in
    longest_string_helper(comparison,strings)
  end

fun longest_string4(strings) =
  let
    val comparison = fn (a,b) => a >= b
  in
    longest_string_helper(comparison,strings)
  end

(* returns the longest string in the list that begins with
an uppercase letter, or "" if there are no such strings. *)

fun longest_capitalized(strings) =
  let
    val helper = longest_string1 o only_capitals
  in
    helper(strings)
  end

fun rev_string(string) = (String.implode o List.rev o String.explode) string

fun first_answer f list =
  case list of
      []    => raise NoAnswer
    | x::xs => case f(x) of
                    NONE   => first_answer f xs
                  | SOME y => y

fun all_answers f items =
  let
    fun helper (function, list, acc) = 
      case list of
          []    => SOME []
        | x::xs => case f(x) of
                        NONE   => NONE
                      | SOME y => helper((function, xs, y @ acc))
  in
    helper(f, items, [])
  end

fun count_wildcards(pattern) = g (fn x => 1) (fn y => 0) pattern

fun count_wild_and_variable_lengths(pattern) = g (fn x => 1) (fn y => String.size(y)) pattern

fun count_some_var(char, pattern) =
  let
    fun helper(text) = if text = char then 1 else 0
  in
    g (fn x => 0) (helper) pattern
  end

fun check_pat(pattern) =
  let
    fun extract_variables(pat) =
      case pat of
          Variable x        => [x]
        | TupleP ps         => List.foldl (fn (v,vs) => vs @ extract_variables(v)) [] ps
        | ConstructorP(_,p) => extract_variables(p)
        | _                 => []

    fun has_duplication(list) =
      case list of
         [x] => true
       | x::xs => if List.exists(fn f => f = x) xs
                  then false
                  else has_duplication(xs)
  in
    has_duplication(extract_variables(pattern))
  end
