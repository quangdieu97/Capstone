(* algo-new.ml *)

let silence = ref true;;

let show_int n =
 (* show_int : int -> string *)
  if n < 0
  then "~" ^ string_of_int n
  else string_of_int n;;

let show_list show_yourself vs =
  if vs = []
  then "[]"
  else let v = List.hd vs
       and vs' = List.tl vs
       in let rec show_list_aux vs v =
            if vs = []
            then show_yourself v
            else let v' = List.hd vs
                 and vs' = List.tl vs
                 in let uvs' = show_list_aux vs' v'
                    in (show_yourself v) ^ "; " ^ uvs'
          in "[" ^ (show_list_aux vs' v) ^ "]";;

(* ********** *)

type token = LP | RP | INT of int | PLUS | MINUS | TIMES | QUOTIENT | SUCC | PRED;;

let show_raw_token t =
  match t with
  | LP ->
     "LP"
  | RP ->
     "RP"
  | INT n ->
     "INT " ^ string_of_int n
  | PLUS ->
     "PLUS"
  | MINUS ->
     "MINUS"
  | TIMES ->
     "TIMES"
  | QUOTIENT ->
     "QUOTIENT"
  | SUCC ->
     "SUCC"
  | PRED ->
     "PRED";;

let show_token t =
  match t with
  | LP ->
     "("
  | RP ->
     ")"
  | INT n ->
     string_of_int n
  | PLUS ->
     " + "
  | MINUS ->
     " - "
  | TIMES ->
     " * "
  | QUOTIENT ->
     " / "
  | SUCC ->
     "succ "
  | PRED ->
     "pred ";;

let show_tokens xs =
  String.concat "" (List.map show_token xs);;

(* ********** *)

let test_evaluate candidate =
  let (* b00 = (candidate [LP; INT 0; RP] = 0)
  (* ***** *)
  and *) b01 = (candidate [LP; SUCC; INT 0; RP] = 1)
  (* ***** *)
  and b02 = (candidate [LP; INT 1; PLUS; INT 10; RP] = 11)
  (* ***** *)
  and b03 = (candidate [LP; INT 12; MINUS; INT 1; RP] = 11)
  (* ***** *)
  and b04 = (candidate [LP; INT 2; TIMES; INT 5; RP] = 10)
  (* ***** *)
  and b05 = (candidate [LP; LP; SUCC; INT 0; RP; PLUS; INT 10; RP] = 11)
  and b06 = (candidate [LP; INT 1; PLUS; LP; SUCC; INT 9; RP; RP] = 11)
  and b07 = (candidate [LP; LP; SUCC; INT 0; RP; PLUS; LP; SUCC; INT 9; RP; RP] = 11)
  (* ***** *)
  and b08 = (candidate [LP; LP; SUCC; INT 11; RP; MINUS; INT 1; RP] = 11)
  and b09 = (candidate [LP; INT 12; MINUS; LP; SUCC; INT 0; RP; RP] = 11)
  and b10 = (candidate [LP; LP; SUCC; INT 11; RP; MINUS; LP; SUCC; INT 0; RP; RP] = 11)
  (* ***** *)
  and b11 = (candidate [LP; LP; SUCC; INT 1; RP; TIMES; INT 5; RP] = 10)
  and b12 = (candidate [LP; INT 2; TIMES; LP; SUCC; INT 4; RP; RP] = 10)
  and b13 = (candidate [LP; LP; SUCC; INT 1; RP; TIMES; LP; SUCC; INT 4; RP; RP] = 10)
  (* ***** *)
  and b14 = (candidate [LP; LP; INT 10; MINUS; INT 1; RP; PLUS; INT 1; RP] = 10)
  and b15 = (candidate [LP; INT 9; PLUS; LP; INT 11; MINUS; INT 10; RP; RP] = 10)
  and b16 = (candidate [LP; LP; INT 10; MINUS; INT 1; RP; PLUS; LP; INT 11; MINUS; INT 10; RP; RP] = 10)
  (* ***** *)
  and b17 = (candidate [LP; LP; LP; INT 16; MINUS; INT 3; RP; MINUS; INT 2; RP; MINUS; INT 1; RP] = 10)
  and b18 = (candidate [LP; INT 13; MINUS; LP; INT 4; MINUS; LP; INT 2; MINUS; INT 1; RP; RP; RP] = 10)
  and b19 = (candidate [LP; LP; LP; LP; INT 10; MINUS; INT 1; RP; PLUS; LP; INT 11; MINUS; INT 10; RP; RP; PLUS; INT 3; RP; MINUS; LP; INT 4; MINUS; LP; INT 2; MINUS; INT 1; RP; RP; RP] = 10)
  (* ***** *)
  and b20 = (candidate [LP; LP; SUCC; INT 0; RP; PLUS; LP; LP; INT 1; PLUS; INT 10; RP; TIMES; LP; INT 2; TIMES; INT 5; RP; RP; RP] = 111)
  and b21 = (candidate [LP; LP; INT 2; MINUS; INT 1; RP; PLUS; LP; LP; INT 1; PLUS; INT 10; RP; TIMES; LP; INT 2; TIMES; INT 5; RP; RP; RP] = 111)
  and b22 = (candidate [LP; LP; INT 2; MINUS; INT 1; RP; PLUS; LP; LP; INT 12; MINUS; INT 1; RP; TIMES; LP; INT 2; TIMES; INT 5; RP; RP; RP] = 111)
  (* ***** *)
  in (* b00 && *) b01 && b02 && b03 && b04 && b05 && b06 && b07 && b08 && b09 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19 && b20 && b21 && b22;;
  
(* ********** *)

exception Stack_underflow;;
exception Stack_overflow;;

type rator = ADD | SUB | MUL | QUO | INC | DEC;;

let show_rator rator =
  match rator with
  | ADD ->
     "ADD"
  | SUB ->
     "SUB"
  | MUL ->
     "MUL"
  | QUO ->
     "QUO"
  | INC ->
     "INC"
  | DEC ->
     "DEC";;

let evaluate_v0 xs_given =
  let rec visit xs rands rators =
    match xs with
    | [] -> 
       (match rands with
        | [] ->
           raise Stack_underflow
        | v :: rands' ->
           (match rands' with
            | [] ->
               v
            | _ ->
              raise Stack_overflow))
    | x :: xs' ->
       match x with
       | LP ->
          visit xs' rands rators
       | PLUS ->
          visit xs' rands (ADD :: rators)
       | MINUS ->
          visit xs' rands (SUB :: rators)
       | TIMES ->
          visit xs' rands (MUL :: rators)
       | QUOTIENT ->
          visit xs' rands (QUO :: rators)
       | SUCC ->
          visit xs' rands (INC :: rators)
       | PRED ->
          visit xs' rands (DEC :: rators)
       | RP ->
          (match rands with
           | [] ->
              raise Stack_underflow
           | (n2 :: rands') ->
              (match rands' with
               | [] ->
                  (match rators with
                   | [] ->
                      visit xs' rands rators
                   | (INC :: rators') ->
                      visit xs' (succ n2 :: rands') rators'
                   | (DEC :: rators') ->
                      visit xs' (pred n2 :: rands') rators'
                   | _ ->
                      raise Stack_underflow)
               | (n1 :: rands'') ->
                  match rators with
                  | [] ->
                     raise Stack_underflow
                  | (ADD :: rators') ->
                     visit xs' ((n1 + n2) :: rands'') rators'
                  | (SUB :: rators') ->
                     visit xs' ((n1 - n2) :: rands'') rators'
                  | (MUL :: rators') ->
                     visit xs' ((n1 * n2) :: rands'') rators'
                  | (QUO :: rators') ->
                     visit xs' ((n1 / n2) :: rands'') rators'
                  | (INC :: rators') ->
                     visit xs' ((succ n2) :: n1 :: rands'') rators'
                  | (DEC :: rators') ->
                     visit xs' ((pred n2) :: n1 :: rands'') rators'))
       | INT n ->
          visit xs' (n :: rands) rators
  in visit xs_given [] [];;

let () = assert (test_evaluate evaluate_v0);;

(* ********** *)

(* unfortunately, the program above does not work for unparenthesized source programs *)

(*
   # evaluate_v0 [LP; INT 1; PLUS; INT 10; PLUS; INT 100; RP];;
   Exception: Stack_overflow.
   # 
*)

let unparse_token t =
  match t with
  | LP ->
     "("
  | RP ->
     ")"
  | INT n ->
     if n < 0
     then "~" ^ string_of_int n
     else string_of_int n
  | PLUS ->
     " + "
  | MINUS ->
     " - "
  | TIMES ->
     " * "
  | QUOTIENT ->
     " / "
  | SUCC ->
     "succ "
  | PRED ->
     "pred "

let unparse_tokens ts =
  String.concat "" (List.map unparse_token ts);;

exception Incorrect_string;;

let test_lex candidate =
  let b0 = (candidate "1 22 333 ~-4444" = [INT 1; INT 22; INT 333; INT (-4444)])
  and b1 = (candidate "+ -/predsucc" = [PLUS; MINUS; QUOTIENT; PRED; SUCC])
  and b2 = (candidate ")(((" = [RP; LP; LP; LP])
  and b3 = (candidate "((1 + 2) * ~-3)" = [LP; LP; INT 1; PLUS; INT 2; RP; TIMES; INT (-3); RP])
  in b0 && b1 && b2 && b3;;
(* *)
let lex cs =
  let length_cs = String.length cs
  in let rec visit i =
       if i = length_cs
       then []
       else match String.get cs i with
            | ' ' ->
               visit (i + 1)
            | '(' ->
               LP :: visit (i + 1)
            | ')' ->
               RP :: visit (i + 1)
            | '+' ->
               PLUS :: visit (i + 1)
            | '-' ->
               MINUS :: visit (i + 1)
            | '*' ->
               TIMES :: visit (i + 1)
            | '/' ->
               QUOTIENT :: visit (i + 1)
            | 's' ->
               if i + 3 < length_cs && String.get cs (i + 1) = 'u' && String.get cs (i + 2) = 'c' && String.get cs (i + 3) = 'c'
               then SUCC :: visit (i + 4)
               else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                     raise Incorrect_string)
            | 'p' ->
               if i + 3 < length_cs && String.get cs (i + 1) = 'r' && String.get cs (i + 2) = 'e' && String.get cs (i + 3) = 'd'
               then PRED :: visit (i + 4)
               else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                     raise Incorrect_string)
            | '~' ->
               if i + 2 < length_cs && String.get cs (i + 1) = '-'
               then let rec compute i n =
                      if i = length_cs
                      then INT (~- n) :: []
                      else let c = String.get cs i
                           in if int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'
                              then compute (i + 1) (10 * n + (int_of_char c - int_of_char '0'))
                              else INT (- n) :: visit i
                    in let c = String.get cs (i + 2)
                       in if int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'
                          then compute (i + 3) (int_of_char c - int_of_char '0')
                          else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                                raise Incorrect_string)
               else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                     raise Incorrect_string)
            | c ->
               let rec compute i n =
                 if i = length_cs
                 then INT n :: []
                 else let c = String.get cs i
                      in if int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'
                         then compute (i + 1) (10 * n + (int_of_char c - int_of_char '0'))
                         else INT n :: visit i
               in if int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'
                  then compute (i + 1) (int_of_char c - int_of_char '0')
                  else (Printf.printf "Incorrect string \"%s\" at index %d\n" cs i;
                        raise Incorrect_string)
  in visit 0;;

let () = assert (test_lex lex);;

(* ********** *)

(* what does the final interpreter look like? what's the shape? *)
(* programs that are stack-based vs non-stack-based programs*)
(* taking sedgewick and shunting yard and go through its successive versions *)

type answer =
  | Res of int
  | Err of string;;

let show_answer a =
  match a with 
  | Res n ->
     "Res " ^ show_int n
  | Err s ->
     "Err " ^ s;;

let test_interpret_once name candidate input expected_output test =
  let actual_output = candidate input
  in if actual_output = expected_output
     then true
     else let () = Printf.printf
                     "test_interpret_once failed for %s on input %s with output (%s) instead of (%s) in test %s\n"
                     name (show_list show_raw_token input) (show_answer actual_output) (show_answer expected_output) test
          in false;;

let test_interpret name candidate =
  let  b00 = (test_interpret_once name candidate [LP; INT 0; RP] (Res 0) "b00")
  (* ***** *)
  and  b01 = (test_interpret_once name candidate [LP; SUCC; INT 0; RP] (Res 1) "b01")
  (* ***** *)
  and b02 = (test_interpret_once name candidate [LP; INT 1; PLUS; INT 10; RP] (Res 11) "b02")
  (* ***** *)
  and b03 = (test_interpret_once name candidate [LP; INT 12; MINUS; INT 1; RP] (Res 11) "b03")
  (* ***** *)
  and b04 = (test_interpret_once name candidate [LP; INT 2; TIMES; INT 5; RP] (Res 10) "b04")
  (* ***** *)
  and b05 = (test_interpret_once name candidate [LP; LP; SUCC; INT 0; RP; PLUS; INT 10; RP] (Res 11) "b05")
  and b06 = (test_interpret_once name candidate [LP; INT 1; PLUS; LP; SUCC; INT 9; RP; RP] (Res 11) "b06")
  and b07 = (test_interpret_once name candidate [LP; LP; SUCC; INT 0; RP; PLUS; LP; SUCC; INT 9; RP; RP] (Res 11) "b07")
  (* ***** *)
  and b08 = (test_interpret_once name candidate [LP; LP; SUCC; INT 11; RP; MINUS; INT 1; RP] (Res 11) "b08")
  and b09 = (test_interpret_once name candidate [LP; INT 12; MINUS; LP; SUCC; INT 0; RP; RP] (Res 11) "b09")
  and b10 = (test_interpret_once name candidate [LP; LP; SUCC; INT 11; RP; MINUS; LP; SUCC; INT 0; RP; RP] (Res 11) "b10")
  (* ***** *)
  and b11 = (test_interpret_once name candidate [LP; LP; SUCC; INT 1; RP; TIMES; INT 5; RP] (Res 10) "b11")
  and b12 = (test_interpret_once name candidate [LP; INT 2; TIMES; LP; SUCC; INT 4; RP; RP] (Res 10) "b12")
  and b13 = (test_interpret_once name candidate [LP; LP; SUCC; INT 1; RP; TIMES; LP; SUCC; INT 4; RP; RP] (Res 10) "b13")
  (* ***** *)
  and b14 = (test_interpret_once name candidate [LP; LP; INT 10; MINUS; INT 1; RP; PLUS; INT 1; RP] (Res 10) "b14")
  and b15 = (test_interpret_once name candidate [LP; INT 9; PLUS; LP; INT 11; MINUS; INT 10; RP; RP] (Res 10) "b15")
  and b16 = (test_interpret_once name candidate [LP; LP; INT 10; MINUS; INT 1; RP; PLUS; LP; INT 11; MINUS; INT 10; RP; RP] (Res 10) "b16")
  (* ***** *)
  and b17 = (test_interpret_once name candidate [LP; LP; LP; INT 16; MINUS; INT 3; RP; MINUS; INT 2; RP; MINUS; INT 1; RP] (Res 10) "b17")
  and b18 = (test_interpret_once name candidate [LP; INT 13; MINUS; LP; INT 4; MINUS; LP; INT 2; MINUS; INT 1; RP; RP; RP] (Res 10) "b18")
  and b19 = (test_interpret_once name candidate [LP; LP; LP; LP; INT 10; MINUS; INT 1; RP; PLUS; LP; INT 11; MINUS; INT 10; RP; RP; PLUS; INT 3; RP; MINUS; LP; INT 4; MINUS; LP; INT 2; MINUS; INT 1; RP; RP; RP] (Res 10) "b19")
  (* ***** *)
  and b20 = (test_interpret_once name candidate [LP; LP; SUCC; INT 0; RP; PLUS; LP; LP; INT 1; PLUS; INT 10; RP; TIMES; LP; INT 2; TIMES; INT 5; RP; RP; RP] (Res 111) "b20")
  and b21 = (test_interpret_once name candidate [LP; LP; INT 2; MINUS; INT 1; RP; PLUS; LP; LP; INT 1; PLUS; INT 10; RP; TIMES; LP; INT 2; TIMES; INT 5; RP; RP; RP] (Res 111) "b21")
  and b22 = (test_interpret_once name candidate [LP; LP; INT 2; MINUS; INT 1; RP; PLUS; LP; LP; INT 12; MINUS; INT 1; RP; TIMES; LP; INT 2; TIMES; INT 5; RP; RP; RP] (Res 111) "b22")
  (* ***** *)
  in  b00 && b01 && b02 && b03 && b04 && b05 && b06 && b07 && b08 && b09 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19 && b20 && b21 && b22;;

let show_tuple (ts, rands, rators) =
  "(" ^ show_list show_raw_token ts ^ ", " ^ show_list show_int rands ^ ", " ^ show_list show_rator rators ^ ")";;

let interpret_v0' ts_given =
  let rec visit tuple =
(*  let () = Printf.printf "visit %s ->\n" (show_tuple tuple) in *)
    match tuple with
    | ([], rands, rators) ->
       Err (Printf.sprintf
              "out of tokens with rands = %s and rators = %s"
              (show_list show_int rands)
              (show_list show_rator rators))
    | ((INT n) :: ts', rands, rators) ->
       visit (ts', n :: rands, rators)
    | (LP :: ts', rands, rators) ->
       visit (ts', rands, rators)
    | (PLUS :: ts', rands, rators) ->
       visit (ts', rands, ADD :: rators)
    | (MINUS :: ts', rands, rators) ->
       visit (ts', rands, SUB :: rators)
    | (TIMES :: ts', rands, rators) ->
       visit (ts', rands, MUL :: rators)
    | (QUOTIENT :: ts', rands, rators) ->
       visit (ts', rands, QUO :: rators)
    | (SUCC :: ts', rands, rators) ->
       visit (ts', rands, INC :: rators)
    | (PRED :: ts', rands, rators) ->
       visit (ts', rands, DEC :: rators)
    | (RP :: [], rands, rators) ->
       finish (rators, rands)
    | (RP :: ts', n1 :: rands, INC :: rators) ->
       visit (ts', succ n1 :: rands, rators)
    | (RP :: ts', n1 :: rands, DEC :: rators) ->
       visit (ts', pred n1 :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, ADD :: rators) ->
       visit (ts', (n1 + n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, SUB :: rators) ->
       visit (ts', (n1 - n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, MUL :: rators) ->
       visit (ts', (n1 * n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, QUO :: rators) ->
       visit (ts', (n1 / n2) :: rands, rators)
    | (RP :: _, rands, rators) ->
       Err (Printf.sprintf
              "not enough rands in %s for %s"
              (show_list show_int rands)
              (show_list show_rator rators))
    and finish pair =
      match pair with
      | ([], rands) ->
         terminate rands
      | (INC :: rators, n1 :: rands) ->
         finish (rators, (n1 + 1) :: rands)
      | (DEC :: rators, n1 :: rands) ->
         finish (rators, (n1 - 1) :: rands)
      | (ADD :: rators, n2 :: n1 :: rands) ->
         finish (rators, (n1 + n2) :: rands)
      | (SUB :: rators, n2 :: n1 :: rands) ->
         finish (rators, (n1 - n2) :: rands)
      | (MUL :: rators, n2 :: n1 :: rands) ->
         finish (rators, (n1 * n2) :: rands)
      | (QUO :: rators, n2 :: n1 :: rands) ->
         finish (rators, (n1 / n2) :: rands)
      | (rators, rands) ->
         Err (Printf.sprintf
                "not enough rands in %s for %s"
                (show_list show_int rands)
                (show_list show_rator rators))
    and terminate rands =
      match rands with
      | [] ->
         Err "empty rand at the end"
      | n :: [] ->
         Res n
      | _ ->
         Err "too many rands at the end"         
  in visit (ts_given, [], []);;

let () = assert (test_interpret "interpret_v0'" interpret_v0');;

let interpret_v0 ts_given =
  let rec visit tuple =
(*let () = Printf.printf "visit %s ->\n" (show_tuple tuple) in *)
    match tuple with
    | ([], rands, rators) ->
       Err (Printf.sprintf
              "out of tokens with rands = %s and rators = %s"
              (show_list show_int rands)
              (show_list show_rator rators))
    | ((INT n) :: ts', rands, rators) ->
       visit (ts', n :: rands, rators)
    | (LP :: ts', rands, rators) ->
       visit (ts', rands, rators)
    | (PLUS :: ts', rands, rators) ->
       visit (ts', rands, ADD :: rators)
    | (MINUS :: ts', rands, rators) ->
       visit (ts', rands, SUB :: rators)
    | (TIMES :: ts', rands, rators) ->
       visit (ts', rands, MUL :: rators)
    | (QUOTIENT :: ts', rands, rators) ->
       visit (ts', rands, QUO :: rators)
    | (SUCC :: ts', rands, rators) ->
       visit (ts', rands, INC :: rators)
    | (PRED :: ts', rands, rators) ->
       visit (ts', rands, DEC :: rators)
    | (RP :: [], rands, rators) ->
       (match (rators, rands) with
        | ([], rands) ->
           terminate rands
        | (INC :: [], n1 :: rands) ->
           terminate ((n1 + 1) :: rands)
        | (DEC :: [], n1 :: rands) ->
           terminate ((n1 - 1) :: rands)
        | (ADD :: [], n2 :: n1 :: rands) ->
           terminate ((n1 + n2) :: rands)
        | (SUB :: [], n2 :: n1 :: rands) ->
           terminate ((n1 - n2) :: rands)
        | (MUL :: [], n2 :: n1 :: rands) ->
           terminate ((n1 * n2) :: rands)
        | (QUO :: [], n2 :: n1 :: rands) ->
           terminate ((n1 / n2) :: rands)
        | (rators, rands) ->
           Err (Printf.sprintf
                  "problem in finish with rands = %s and rators = %s"
                  (show_list show_int rands)
                 (show_list show_rator rators)))
    | (RP :: ts', n1 :: rands, INC :: rators) ->
       visit (ts', succ n1 :: rands, rators)
    | (RP :: ts', n1 :: rands, DEC :: rators) ->
       visit (ts', pred n1 :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, ADD :: rators) ->
       visit (ts', (n1 + n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, SUB :: rators) ->
       visit (ts', (n1 - n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, MUL :: rators) ->
       visit (ts', (n1 * n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, QUO :: rators) ->
       visit (ts', (n1 / n2) :: rands, rators)
    | (RP :: _, rands, rators) ->
       Err (Printf.sprintf
              "not enough rands in %s for %s"
              (show_list show_int rands)
              (show_list show_rator rators))
     and terminate rands =
      match rands with
      | [] ->
         Err "empty rand at the end"
      | n :: [] ->
         Res n
      | _ ->
         Err "too many rands at the end"         
  in visit (ts_given, [], []);;

let () = assert (test_interpret "interpret_v0" interpret_v0);;

let interpret_v1 ts_given =
  let rec visit tuple =
(*let () = Printf.printf "visit %s ->\n" (show_tuple tuple) in *)
    match tuple with
    | ([], rands, rators) ->
       Err (Printf.sprintf
              "out of tokens with rands = %s and rators = %s"
              (show_list show_int rands)
              (show_list show_rator rators))
    | ((INT n) :: ts', rands, rators) ->
       visit (ts', n :: rands, rators)
    | (LP :: ts', rands, rators) ->
       visit (ts', rands, rators)
    | (PLUS :: ts', rands, rators) ->
       visit (ts', rands, ADD :: rators)
    | (MINUS :: ts', rands, rators) ->
       visit (ts', rands, SUB :: rators)
    | (TIMES :: ts', rands, rators) ->
       visit (ts', rands, MUL :: rators)
    | (QUOTIENT :: ts', rands, rators) ->
       visit (ts', rands, QUO :: rators)
    | (SUCC :: ts', rands, rators) ->
       visit (ts', rands, INC :: rators)
    | (PRED :: ts', rands, rators) ->
       visit (ts', rands, DEC :: rators)
    | (RP :: [], rands, rators) ->
       finish (rators, rands)
    | (RP :: ts', n1 :: rands, INC :: rators) ->
       visit (ts', succ n1 :: rands, rators)
    | (RP :: ts', n1 :: rands, DEC :: rators) ->
       visit (ts', pred n1 :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, ADD :: rators) ->
       visit (ts', (n1 + n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, SUB :: rators) ->
       visit (ts', (n1 - n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, MUL :: rators) ->
       visit (ts', (n1 * n2) :: rands, rators)
    | (RP :: ts', n2 :: n1 :: rands, QUO :: rators) ->
       visit (ts', (n1 / n2) :: rands, rators)
    | (RP :: _, rands, rators) ->
       Err (Printf.sprintf
              "not enough rands in %s for %s"
              (show_list show_int rands)
              (show_list show_rator rators))
    and finish pair =
      match pair with
      | ([], rands) ->
         terminate rands
      | (INC :: [], n1 :: rands) ->
         terminate ((n1 + 1) :: rands)
      | (DEC :: [], n1 :: rands) ->
         terminate ((n1 - 1) :: rands)
      | (ADD :: [], n2 :: n1 :: rands) ->
         terminate ((n1 + n2) :: rands)
      | (SUB :: [], n2 :: n1 :: rands) ->
         terminate ((n1 - n2) :: rands)
      | (MUL :: [], n2 :: n1 :: rands) ->
         terminate ((n1 * n2) :: rands)
      | (QUO :: [], n2 :: n1 :: rands) ->
         terminate ((n1 / n2) :: rands)
      | (rators, rands) ->
         Err (Printf.sprintf
                "problem in finish with rands = %s and rators = %s"
                (show_list show_int rands)
                (show_list show_rator rators))
    and terminate rands =
      match rands with
      | [] ->
         Err "empty rand at the end"
      | n :: [] ->
         Res n
      | _ ->
         Err "too many rands at the end"         
  in visit (ts_given, [], []);;

let () = assert (test_interpret "interpret_v1" interpret_v1);;

let interpret_v2 ts_given =
  let rec visit tuple =
    let () = if !silence
             then ()
             else Printf.printf "visit %s ->\n" (show_tuple tuple)
    in match tuple with
       | ([], rands, rators) ->
         finish (rands, rators)
(*
          Err (Printf.sprintf
                 "out of tokens with rands = %s and rators = %s"
                 (show_list show_int rands)
                 (show_list show_rator rators))
*)
       | ((INT n) :: ts', rands, rators) ->
          visit (ts', n :: rands, rators)
       | (LP :: ts', rands, rators) ->
          visit (ts', rands, rators)
       | (PLUS :: ts', rands, rators) ->
          visit (ts', rands, ADD :: rators)
       | (MINUS :: ts', rands, rators) ->
          visit (ts', rands, SUB :: rators)
       | (TIMES :: ts', rands, rators) ->
          visit (ts', rands, MUL :: rators)
       | (QUOTIENT :: ts', rands, rators) ->
          visit (ts', rands, QUO :: rators)
       | (SUCC :: ts', rands, rators) ->
          visit (ts', rands, INC :: rators)
       | (PRED :: ts', rands, rators) ->
          visit (ts', rands, DEC :: rators)
       | (RP :: ts', n1 :: rands, INC :: rators) ->
          visit (ts', succ n1 :: rands, rators)
       | (RP :: ts', n1 :: rands, DEC :: rators) ->
          visit (ts', pred n1 :: rands, rators)
       | (RP :: ts', n2 :: n1 :: rands, ADD :: rators) ->
          visit (ts', (n1 + n2) :: rands, rators)
       | (RP :: ts', n2 :: n1 :: rands, SUB :: rators) ->
          visit (ts', (n1 - n2) :: rands, rators)
       | (RP :: ts', n2 :: n1 :: rands, MUL :: rators) ->
          visit (ts', (n1 * n2) :: rands, rators)
       | (RP :: ts', n2 :: n1 :: rands, QUO :: rators) ->
          visit (ts', (n1 / n2) :: rands, rators)
(*
       | (RP :: [], rands, rators) ->
          finish (rands, rators)
*)
       | (RP :: _, rands, rators) ->
          Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_list show_rator rators))
  and finish ((rands, rators) as pair) =
    let () = if !silence
             then ()
             else Printf.printf "finish (%s, %s) ->\n" (show_list show_int rands) (show_list show_rator rators)
    in match pair with
       | (rands, []) ->
          terminate rands
       | (rands, rators) ->
          Err (Printf.sprintf "problem in finish with rands = %s and rators = %s" (show_list show_int rands) (show_list show_rator rators))
  and terminate rands =
    let () = if !silence
             then ()
             else Printf.printf "terminate %s ->\n" (show_list show_int rands)
    in match rands with
       | [] ->
          Err "empty rand at the end"
       | n :: [] ->
          Res n
       | _ ->
          Err "too many rands at the end"         
  in visit (ts_given, [], []);;

let () = assert (test_interpret "interpret_v2" interpret_v2);;
(* fail for b0*)
(* disentangle: *)

let interpret_v3 ts_given =
  let rec visit tuple =
    let () = if !silence
             then ()
             else Printf.printf "visit %s ->\n" (show_tuple tuple)
    in match tuple with
       | ([], rands, rators) ->
         finish (rands, rators)
(*
          Err (Printf.sprintf
                 "out of tokens with rands = %s and rators = %s"
                 (show_list show_int rands)
                 (show_list show_rator rators))
*)
       | ((INT n) :: ts', rands, rators) ->
          visit (ts', n :: rands, rators)
       | (LP :: ts', rands, rators) ->
          visit (ts', rands, rators)
       | (PLUS :: ts', rands, rators) ->
          visit (ts', rands, ADD :: rators)
       | (MINUS :: ts', rands, rators) ->
          visit (ts', rands, SUB :: rators)
       | (TIMES :: ts', rands, rators) ->
          visit (ts', rands, MUL :: rators)
       | (QUOTIENT :: ts', rands, rators) ->
          visit (ts', rands, QUO :: rators)
       | (SUCC :: ts', rands, rators) ->
          visit (ts', rands, INC :: rators)
       | (PRED :: ts', rands, rators) ->
          visit (ts', rands, DEC :: rators)
       | (RP :: ts', rands, rators) ->
          visit_RP (ts', rands, rators)
  and visit_RP ((ts, rands, rators) as tuple) =
    let () = if !silence
             then ()
             else Printf.printf "visit_RP %s ->\n" (show_tuple tuple)
    in match rators with
       | INC :: rators' ->
          (match rands with
           | n1 :: rands' ->
              visit (ts, succ n1 :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator INC)))
       | DEC :: rators' ->
          (match rands with
           | n1 :: rands' ->
              visit (ts, pred n1 :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator DEC)))
       | ADD :: rators' ->
          (match rands with
           | n2 :: n1 :: rands' ->
              visit (ts, (n1 + n2) :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator ADD)))
       | SUB :: rators' ->
          (match rands with
           | n2 :: n1 :: rands' ->
              visit (ts, (n1 - n2) :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator SUB)))
       | MUL :: rators' ->
          (match rands with
           | n2 :: n1 :: rands' ->
              visit (ts, (n1 * n2) :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator MUL)))
       | QUO :: rators' ->
          (match rands with
           | n2 :: n1 :: rands' ->
              visit (ts, (n1 / n2) :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator QUO)))
       | [] ->
          Err (Printf.sprintf "not enough rators for %s" (show_list show_int rands))
  and finish ((rands, rators) as pair) =
    let () = if !silence
             then ()
             else Printf.printf "finish (%s, %s) ->\n" (show_list show_int rands) (show_list show_rator rators)
    in match pair with
       | (rands, []) ->
          terminate rands
       | (rands, rators) ->
          Err (Printf.sprintf "problem in finish with rands = %s and rators = %s" (show_list show_int rands) (show_list show_rator rators))
  and terminate rands =
    let () = if !silence
             then ()
             else Printf.printf "terminate %s ->\n" (show_list show_int rands)
    in match rands with
       | [] ->
          Err "empty rand at the end"
       | n :: [] ->
          Res n
       | _ ->
          Err "too many rands at the end"         
  in visit (ts_given, [], []);;

let () = assert (test_interpret "interpret_v3" interpret_v3);;

let interpret_v3' ts_given =
  let rec visit tuple =
    let () = if !silence
             then ()
             else Printf.printf "visit %s ->\n" (show_tuple tuple)
    in match tuple with
       | ([], rands, rators) ->
          Err (Printf.sprintf
                 "out of tokens with rands = %s and rators = %s"
                 (show_list show_int rands)
                 (show_list show_rator rators))
       | ((INT n) :: ts', rands, rators) ->
          visit (ts', n :: rands, rators)
       | (LP :: ts', rands, rators) ->
          visit (ts', rands, rators)
       | (PLUS :: ts', rands, rators) ->
          visit (ts', rands, ADD :: rators)
       | (MINUS :: ts', rands, rators) ->
          visit (ts', rands, SUB :: rators)
       | (TIMES :: ts', rands, rators) ->
          visit (ts', rands, MUL :: rators)
       | (QUOTIENT :: ts', rands, rators) ->
          visit (ts', rands, QUO :: rators)
       | (SUCC :: ts', rands, rators) ->
          visit (ts', rands, INC :: rators)
       | (PRED :: ts', rands, rators) ->
          visit (ts', rands, DEC :: rators)
       | (RP :: ts', rands, rators) ->
          visit_RP (ts', rands, rators)
  and visit_RP ((ts, rands, rators) as tuple) =
    let () = if !silence
             then ()
             else Printf.printf "visit_RP %s ->\n" (show_tuple tuple)
    in match ts with
       | [] ->
          terminate rands
       | _ ->
             Err (Printf.sprintf "not enough rators for %s" (show_list show_int rands))
    match rators with
       | INC :: rators' ->
          (match rands with
           | n1 :: rands' ->
              visit (ts, succ n1 :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator INC)))
       | DEC :: rators' ->
          (match rands with
           | n1 :: rands' ->
              visit (ts, pred n1 :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator DEC)))
       | ADD :: rators' ->
          (match rands with
           | n2 :: n1 :: rands' ->
              visit (ts, (n1 + n2) :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator ADD)))
       | SUB :: rators' ->
          (match rands with
           | n2 :: n1 :: rands' ->
              visit (ts, (n1 - n2) :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator SUB)))
       | MUL :: rators' ->
          (match rands with
           | n2 :: n1 :: rands' ->
              visit (ts, (n1 * n2) :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator MUL)))
       | QUO :: rators' ->
          (match rands with
           | n2 :: n1 :: rands' ->
              visit (ts, (n1 / n2) :: rands', rators')
           | _ ->
              Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_rator QUO)))
       | [] ->
          match ts with
          | [] ->
             terminate rands
          | _ ->
             Err (Printf.sprintf "not enough rators for %s" (show_list show_int rands))
  and terminate rands =
    let () = if !silence
             then ()
             else Printf.printf "terminate %s ->\n" (show_list show_int rands)
    in match rands with
       | [] ->
          Err "empty rand at the end"
       | n :: [] ->
          Res n
       | _ ->
          Err "too many rands at the end"         
  in visit (ts_given, [], []);;


(* refunctionalize: *)

let interpret_v4 ts_given =
  let rec visit ((ts, rands, rators) as tuple) =
    let () = if !silence
             then ()
             else Printf.printf "visit (%s, %s, continuation) ->\n" (show_list show_raw_token ts) (show_list show_int rands)
    in match tuple with
       | ([], rands, rators) ->
          rators ([], rands)
       | ((INT n) :: ts', rands, rators) ->
          visit (ts', n :: rands, rators)
       | (LP :: ts', rands, rators) ->
          visit (ts', rands, rators)
       | (PLUS :: ts', rands, rators) ->
          visit (ts', rands, (fun (ts'', rands) ->
                               match rands with
                               | n2 :: n1 :: rands' ->
                                  visit (ts'', (n1 + n2) :: rands', rators)
                               | _ ->
                                  Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token PLUS))))
       | (MINUS :: ts', rands, rators) ->
          visit (ts', rands, (fun (ts'', rands) ->
                               match rands with
                               | n2 :: n1 :: rands' ->
                                  visit (ts'', (n1 - n2) :: rands', rators)
                               | _ ->
                                  Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token MINUS))))
       | (TIMES :: ts', rands, rators) ->
          visit (ts', rands, (fun (ts'', rands) ->
                               match rands with
                               | n2 :: n1 :: rands' ->
                                  visit (ts'', (n1 * n2) :: rands', rators)
                               | _ ->
                                  Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token TIMES))))
       | (QUOTIENT :: ts', rands, rators) ->
          visit (ts', rands, (fun (ts'', rands) ->
                               match rands with
                               | n2 :: n1 :: rands' ->
                                  visit (ts'', (n1 / n2) :: rands', rators)
                               | _ ->
                                  Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token QUOTIENT))))
       | (SUCC :: ts', rands, rators) ->
          visit (ts', rands, (fun (ts'', rands) ->
                               match rands with
                               | n1 :: rands' ->
                                  visit (ts'', succ n1 :: rands', rators)
                               | _ ->
                                  Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token SUCC))))
       | (PRED :: ts', rands, rators) ->
          visit (ts', rands, (fun (ts'', rands) ->
                               match rands with
                               | n1 :: rands' ->
                                  visit (ts'', pred n1 :: rands', rators)
                               | _ ->
                                  Err (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token PRED))))
       | (RP :: ts', rands, rators) ->
          rators (ts', rands)
  in visit (ts_given, [], (fun (ts, rands) ->
                            match ts with
                            | [] ->
                               (match rands with
                                | [] ->
                                   Err "empty rand at the end"
                                | n :: [] ->
                                   Res n
                                | _ ->
                                   Err "too many rands at the end")
                            | _ ->
                               Err (Printf.sprintf "non-empty list of tokens in the initial continuation: %s for rands = %s" (show_list show_token ts) (show_list show_int rands))));;

let () = assert (test_interpret "interpret_v4" interpret_v4);;

exception Errare_humanum_est of string;;

let interpret_v5 ts_given =
  let rec visit ((ts, rands) as tuple) =
    let () = if !silence
             then ()
             else Printf.printf "visit (%s, %s) ->\n" (show_list show_raw_token ts) (show_list show_int rands)
    in match tuple with
       | ([], rands) ->
          ([], rands)
       | ((INT n) :: ts', rands) ->
          visit (ts', n :: rands)
       | (LP :: ts', rands) ->
          visit (ts', rands)
       | (PLUS :: ts', rands) ->
          let (ts'', rands) = visit (ts', rands)
          in (match rands with
              | n2 :: n1 :: rands' ->
                 visit (ts'', (n1 + n2) :: rands')
              | _ ->
                 raise (Errare_humanum_est (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token PLUS))))
       | (MINUS :: ts', rands) ->
          let (ts'', rands) = visit (ts', rands)
          in (match rands with
              | n2 :: n1 :: rands' ->
                 visit (ts'', (n1 - n2) :: rands')
              | _ ->
                 raise (Errare_humanum_est (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token MINUS))))
       | (TIMES :: ts', rands) ->
          let (ts'', rands) = visit (ts', rands)
          in (match rands with
              | n2 :: n1 :: rands' ->
                 visit (ts'', (n1 * n2) :: rands')
              | _ ->
                 raise (Errare_humanum_est (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token TIMES))))
       | (QUOTIENT :: ts', rands) ->
          let (ts'', rands) = visit (ts', rands)
          in (match rands with
              | n2 :: n1 :: rands' ->
                 visit (ts'', (n1 / n2) :: rands')
              | _ ->
                 raise (Errare_humanum_est (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token QUOTIENT))))
       | (SUCC :: ts', rands) ->
          let (ts'', rands) = visit (ts', rands)
          in (match rands with
              | n1 :: rands' ->
                 visit (ts'', succ n1 :: rands')
              | _ ->
                 raise (Errare_humanum_est (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token SUCC))))
       | (PRED :: ts', rands) ->
          let (ts'', rands) = visit (ts', rands)
          in (match rands with
              | n1 :: rands' ->
                 visit (ts'', pred n1 :: rands')
              | _ ->
                 raise (Errare_humanum_est (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token PRED))))
       | (RP :: ts', rands) ->
          (ts', rands)
  in try let (ts, rands) = visit (ts_given, [])
         in match ts with
            | [] ->
               (match rands with
                | [] ->
                   Err "empty rand at the end"
                | n :: [] ->
                   Res n
                | _ ->
                   Err "too many rands at the end")
            | _ ->
               Err (Printf.sprintf "non-empty list of tokens at the end: %s for rands = %s" (show_list show_token ts) (show_list show_int rands)) with
     | Errare_humanum_est msg ->
        Err msg;;

let () = assert (test_interpret "interpret_v5" interpret_v5);;

(* without exceptions, i.e., purely functional *)

type intermediate =
  | Ituple of token list * int list
  | Ierror of string;;

let interpret_v6 ts_given =
  let rec visit ((ts, rands) as tuple) =
    let () = if !silence
             then ()
             else Printf.printf "visit (%s, %s) ->\n" (show_list show_raw_token ts) (show_list show_int rands)
    in match tuple with
       | ([], rands) ->
          Ituple ([], rands)
       | ((INT n) :: ts', rands) ->
          visit (ts', n :: rands)
       | (LP :: ts', rands) ->
          visit (ts', rands)
       | (PLUS :: ts', rands) ->
          (match visit (ts', rands) with
           | Ituple (ts'', rands) ->
              (match rands with
               | n2 :: n1 :: rands' ->
                  visit (ts'', (n1 + n2) :: rands')
               | _ ->
                  Ierror (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token PLUS)))
           | Ierror msg ->
              Ierror msg)
       | (MINUS :: ts', rands) ->
          (match visit (ts', rands) with
           | Ituple (ts'', rands) ->
              (match rands with
               | n2 :: n1 :: rands' ->
                  visit (ts'', (n1 - n2) :: rands')
               | _ ->
                  Ierror (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token MINUS)))
           | Ierror msg ->
              Ierror msg)
       | (TIMES :: ts', rands) ->
          (match visit (ts', rands) with
           | Ituple (ts'', rands) ->
              (match rands with
               | n2 :: n1 :: rands' ->
                  visit (ts'', (n1 * n2) :: rands')
               | _ ->
                  Ierror (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token TIMES)))
           | Ierror msg ->
              Ierror msg)
       | (QUOTIENT :: ts', rands) ->
          (match visit (ts', rands) with
           | Ituple (ts'', rands) ->
              (match rands with
               | n2 :: n1 :: rands' ->
                  visit (ts'', (n1 / n2) :: rands')
               | _ ->
                  Ierror (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token QUOTIENT)))
           | Ierror msg ->
              Ierror msg)
       | (SUCC :: ts', rands) ->
          (match visit (ts', rands) with
           | Ituple (ts'', rands) ->
              (match rands with
               | n1 :: rands' ->
                  visit (ts'', (succ n1) :: rands')
               | _ ->
                  Ierror (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token SUCC)))
           | Ierror msg ->
              Ierror msg)
       | (PRED :: ts', rands) ->
          (match visit (ts', rands) with
           | Ituple (ts'', rands) ->
              (match rands with
               | n1 :: rands' ->
                  visit (ts'', (pred n1) :: rands')
               | _ ->
                  Ierror (Printf.sprintf "not enough rands in %s for %s" (show_list show_int rands) (show_token PRED)))
           | Ierror msg ->
              Ierror msg)
       | (RP :: ts', rands) ->
          Ituple (ts', rands)
  in match visit (ts_given, []) with
     | Ituple (ts, rands) ->
        (match ts with
         | [] ->
            (match rands with
             | [] ->
                Err "empty rand at the end"
             | n :: [] ->
                Res n
             | _ ->
                Err "too many rands at the end")
         | _ ->
            Err (Printf.sprintf "non-empty list of tokens at the end: %s for rands = %s" (show_list show_token ts) (show_list show_int rands)))
     | Ierror msg ->
        Err msg;;

let () = assert (test_interpret "interpret_v6" interpret_v6);;

(* ********** *)

(* end of algo_new.ml *)

type parenthesis = | L | R;;
type word = parenthesis list;;

let recognize ps =
  let rec run pair =
    match pair with
    | ([] , 0) ->
       true
    | ([],  c) ->
       false
    | (L :: ps', c) ->
       run (ps', succ c)
    | (R :: ps', 0) ->
       false
    | (R :: ps', c) ->
       run (ps', pred c)
  in run (ps,0);;

let recognize_disengtangled ps =
  let rec run pair =
    match pair with
    | ([],c) ->
       run_nil c
    | (L :: ps', c) ->
       run (ps, succ c)
    | (R :: ps', c) ->
       run_par (c,ps)
  and run_nil c =
    match c with
    | 0 -> true
    | _ -> false
  and run_par pair =
    match pair with
    | (0, ps) -> false
    | (c, ps) -> run (ps, pred c) 
 in run (ps, 0);;

let recognize_merged ps =
  let rec run pair =
    match pair with
    | ([] , c) ->
       run_aux (c, None)
    | (L :: ps',c) ->
       run (ps', succ c)
    | (R :: ps', c) ->
       run_aux (c, Some ps')
  and run_aux pair =
    match pair with
    | (0, None) ->
       true
    | (c, None) ->
       false
    | (0, Some ps) ->
       false
    | (c , Some ps) ->
       run (ps, pred c)
  in run (ps, 0);;

let recognize_refunctionalized ps =
  let rec run pair =
    match pair with
    | ([] , c) ->
       c None
    | (L :: ps',c) ->
       run (ps', fun pso ->
                 match pso with
                 | None -> false
                 | Some ps -> run (ps,c))
    | (R :: ps', c) ->
       c (Some ps')
  in run (ps, fun pso ->
              match pso with
              | None -> true
              | Some _ -> false);;


let recognize_direct_style ps =
  let rec run ps =
    match ps with
    | [] -> None
    | L :: ps' ->
       (match run ps' with
       | None -> Some []
       | Some ps ->
          run ps)
    | R :: ps' ->
       Some ps'
  in match run ps with
     | None -> true
     | Some _ -> false;;

