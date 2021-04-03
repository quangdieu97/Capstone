(* Higher-order*)
(fun x -> fun y -> y) 1;;

List.map (fun x -> x + 1) [1;2;3;4;5];;

(*first-order*)
let add x y = x + y;;

let add6 x = add x (add 1 (add 2 3));;

let a = add6 1;;

(*defunctionalization*)
let test_foo candidate =
  let b1 = (candidate 0 = 3) 
  and b2 = (candidate 1 = 5)
  and b3 = (candidate 2 = 7) 
  and b4 = (candidate 3 = 9)
  and b5 = (let k = Random.int 50
            in candidate k = 2 * k + 3)
             (*etc*)
  in b1 && b2 && b3 && b4 && b5;;

      
let foo x = ((fun y -> y + 1) x) + ((fun y -> y + 2 ) x);;

let () = assert (test_foo foo);;


type add_def = |ADD1 |ADD2 ;;

let apply (add,x) =
  match add with
  |ADD1 -> x + 1
  |ADD2 -> x + 2;;

let foo_def x = apply(ADD1,x) + apply(ADD2,x);;
let () = assert (test_foo foo_def);;


let apply_bool (b,x) =
  match b with
  |true -> x + 1
  |false -> x + 2;;

let foo_def_v2 x = apply_bool(true,x) + apply_bool(false,x);;
let () = assert (test_foo foo_def_v2);;


let foo_def_v3 x = 2*x + 3;;
let () = assert (test_foo foo_def_v3);;


(* refunctionalization *)
let test_add a =
      (* an instance of the base case: *)
  let b0 = (let y = Random.int 2000
            in a 0 y = y)
      (* an instance of the induction step: *)
  and b1 = (let y = Random.int 2000
            and x' = Random.int 1000
            in a (succ x') y = succ (a x' y))
      (* a few handpicked numbers: *)
  and b2 = (a 0 100 = 100)
  and b3 = (a 1 100 = 101)
  and b4 = (a 2 100 = 102)
      (* using a witness implementation of the addition function *)
  and b5 = (let x = Random.int 1000
            and y = Random.int 2000
            in a x y = x + y)    (* so much for the dramatic spillage *)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5;;

let add_with_accumulator x_init y_init =
  let () = assert (x_init >= 0) in
  let rec visit x y =
    if x = 0
    then y
    else visit (pred x) (succ y)
  in visit x_init y_init;;
let () = assert (test_add add_with_accumulator);;

let add_cps x_init y_init =
  let () = assert (x_init >= 0) in
  let rec visit x k =
    if x = 0
    then k y_init
    else visit (pred x) (fun y -> k (succ y))
  in visit x_init (fun y -> y);; 
let () = assert (test_add add_cps);;

let add_with_acc_desugar x_init y_init =
  let ()= assert (x_init >= 0) in
  let rec visit x y a =
    if x = 0
    then a
    else visit (pred x) y (succ a)
  in visit x_init y_init y_init;;
let () = assert (test_add add_with_acc_desugar);;

let add_ref x_init y_init =
  let () = assert (x_init >= 0) in
  let rec visit x y k =
    k (if x = 0 then y else visit (pred x) y (fun a -> succ a))
  in visit x_init y_init (fun a -> a);;
let () = assert (test_add add_ref);;

let add_ref_lambda_dropped x_init y_init =
  let () = assert (x_init >= 0) in
  let rec visit x k =
    k (if x = 0 then y_init else visit (pred x) (fun a -> succ a))
  in visit x_init (fun a -> a);;
let () = assert (test_add add_ref_lambda_dropped);;

(*so ref and cps are not entirely the same - cps is a subset of ref?*)


(* *********)
let test_power a =
      (* an instance of the base case: *)
  let b0 = (let y = Random.int 2000
            in a 0 y = 0)
      (* an instance of the induction step: *)
  and b1 = (let y' = Random.int 10
            and x = Random.int 10
            in a x (succ y') = x * (a x y'))
      (* a few handpicked numbers: *)
  and b2 = (a 0 100 = 0)
  and b3 = (a 1 100 = 1)
  and b4 = (a 2 10 = 1024)
  in b0 && b1 && b2 && b3 && b4 ;;

let power x_init n_init =
  let () = assert (n_init >= 0) in
  let rec visit x n =
    if n = 0
    then 1
    else x * (visit x (pred n))
  in visit x_init n_init;;
let () = assert (test_power power);;

let power_acc x_init n_init =
  let () = assert (n_init >= 0) in
  let rec visit x n a =
    if n = 0
    then a
    else visit x (pred n) (x * a)
  in visit x_init n_init 1;;
let () = assert (test_power power_acc);;

let power_cps x_init n_init =
  let () = assert (n_init >= 0) in
  let rec visit x n k =
    if n = 0
    then k 1
    else visit x (pred n) (fun a -> k (x * a))
  in visit x_init n_init (fun a -> a);;
let () = assert (test_power power_cps);;

let power_ref x_init n_init =
  let () = assert (n_init >= 0) in
  let rec visit x n k =
    k (if n = 0 then 1 else visit x (pred n) (fun a -> a * x))
  in visit x_init n_init (fun a -> a);;
let () = assert (test_power power_ref);;

(* ************* *)
let test_prefixes candidate =
  let b0 = (candidate [] =
              [[]])
  and b1 = (candidate [1] =
              [[]; [1]])
  and b2 = (candidate [1; 2] =
              [[]; [1]; [1; 2]])
  and b3 = (candidate [1; 2; 3] =
              [[]; [1]; [1; 2]; [1; 2; 3]])
  and b4 = (candidate [1; 2; 3; 4] =
              [[]; [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]])
  in b0 && b1 && b2 && b3 && b4;;

(* ********** *)

let prefixes_v0 xs_given =
  let rec visit xs a =
    List.rev_append a [] :: (match xs with
                             | [] ->
                                []
                             | x :: xs' ->
                                visit xs' (x :: a))
  in visit xs_given [];;
let () = assert (test_prefixes prefixes_v0);;

let make_prefixes apply nil_case cons_case xs_given =
  let rec visit xs a = (*visit: 'd list -> a -> 'c list *)
    apply a [] :: (match xs with
                   | [] ->
                      []
                   | x :: xs' ->
                      visit xs' (cons_case x a))
  in visit xs_given nil_case;;

(* ***** *)
let prefixes_v1 xs =
  make_prefixes List.rev_append [] (fun v vs -> v :: vs) xs;;
let () = assert (test_prefixes prefixes_v1);;

(* inlining the call to make_prefixes in the definition of prefixes_v1 yields the definition of prefixes_v0 *)


let prefixes_v2 xs =
  make_prefixes (fun h -> h) (fun a -> a) (fun v h a -> h (v :: a)) xs;;
let () = assert (test_prefixes prefixes_v2);;


let prefixes_v3 xs_given =
  let rec visit xs a = (*visit: 'd list -> a -> 'c list *)
    a [] :: (match xs with
                   | [] ->
                      []
                   | x :: xs' ->
                      visit xs' ((fun v h a -> h (v :: a)) x a))
  in visit xs_given (fun a -> a);;
let () = assert (test_prefixes prefixes_v3);;


let prefixes_v4 xs_given =
  let rec visit xs a = (*visit: 'd list -> a -> 'c list *)
    a [] :: (match xs with
                   | [] ->
                      []
                   | x :: xs' ->
                      visit xs' (fun a' -> a (x :: a')))
  in visit xs_given (fun a -> a);;
let () = assert (test_prefixes prefixes_v4);;


let prefixes_v5 xs_given =
  let rec visit xs h = (*visit: 'd list -> a -> 'c list *)
    h [] :: (match xs with
                   | [] ->
                      []
                   | x :: xs' ->
                      visit xs' (fun a -> h (x :: a)))
  in visit xs_given (fun a -> a);;
let () = assert (test_prefixes prefixes_v5);;

let curried_cons x xs = x :: xs;;
  let prefixes_v6 xs_given =
  let rec visit xs h = (*visit: 'd list -> a -> 'c list *)
    h [] :: (match xs with
                   | [] ->
                      []
                   | x :: xs' ->
                      visit xs' (fun a -> h (curried_cons x a)))
  in visit xs_given (fun a -> a);;
let () = assert (test_prefixes prefixes_v6);;

  
let prefixes_v7 xs_given =
  let rec visit xs h = (*visit: 'd list -> a -> 'c list *)
    h [] :: (match xs with
                   | [] ->
                      []
                   | x :: xs' ->
                      visit xs' (fun a -> h ((curried_cons x) a)))
  in visit xs_given (fun a -> a);;
let () = assert (test_prefixes prefixes_v7);;

let compose f g x = f (g x);;

let prefixes_v8 xs_given =
  let rec visit xs h = (*visit: 'd list -> a -> 'c list *)
    h [] :: (match xs with
                   | [] ->
                      []
                   | x :: xs' ->
                      visit xs' (compose h (curried_cons x)))
  in visit xs_given (fun a -> a);;
(* h incremented by composing it to the right of identity*)
let () = assert (test_prefixes prefixes_v8);;

let identity = fun a -> a;;
let prefixes_v9 xs_given =
  let rec visit xs h = (*visit: 'd list -> a -> 'c list *)
    h [] :: (match xs with
                   | [] ->
                      []
                   | x :: xs' ->
                      visit xs' (compose h (curried_cons x)))
  in visit xs_given identity;;
let () = assert (test_prefixes prefixes_v9);;


(* ********************* *)
let test_inject candidate =
  let b0 = (candidate 1 [] = 
              [[1]])
  and b1 = (candidate 2 [1] = 
              [[2; 1]; [1; 2]])
  and b2 = (candidate 3 [1; 2] =
              [[3; 1; 2]; [1; 3; 2]; [1; 2; 3]])
  and b3 = (candidate 4 [1; 2; 3] =
              [[4; 1; 2; 3]; [1; 4; 2; 3]; [1; 2; 4; 3]; [1; 2; 3; 4]])
  and b4 = (candidate 5 [1; 2; 3; 4] =
              [[5; 1; 2; 3; 4]; [1; 5; 2; 3; 4]; [1; 2; 5; 3; 4]; [1; 2; 3; 5; 4]; [1; 2; 3; 4; 5]])
  in b0 && b1 && b2 && b3 && b4;;

(* ********** *)

let inject x xs_given = (*inject : 'a -> 'a list -> 'a list list*)
  let rec visit xs a = (*visit : 'a list -> 'a list -> 'a list list*)
    (List.rev_append a (x :: xs)) :: (match xs with
                                      | [] ->
                                         []
                                      | x' :: xs' ->
                                         visit xs' (x' :: a))
  in visit xs_given [];;

let inject_ref_v1 x xs_given = (* 'a -> 'a list -> 'a list list*)
  let rec visit xs h = (* visit: 'a list -> ('b list -> 'b list) -> 'a list list *)
    h (x :: xs) :: (match xs with
                      [] ->
                      []
                    | x' :: xs' ->
                       visit xs' (compose h (curried_cons x')))
  in visit xs_given (fun a -> a);;


let () = assert (test_inject inject_ref_v1);;

let inject_ref_v2 x xs_given =
  let rec visit xs h =
    h (x :: xs) ::  (match xs with
                     | [] ->
                        []
                     | x' :: xs' ->
                        visit xs' (fun a -> h (x' :: a)))
  in visit xs_given (fun a -> a);;

let () = assert (test_inject inject_ref_v2);;


(* ************** *)
(* define predicates - first order functions *)
let isEven x = (x mod 2 =0);; (* so tu nhien -> dung/sai *)
let isPrime n = (*code found online *)
    let rec checkZero x d = match d with
        | 1 -> true    
        | _ -> (x mod d <> 0) && checkZero x (d-1)
    in match n with
    | 0 | 1 -> false
    | _ -> checkZero n (n-1) ;;

let isPositive x = if x > 0 then true else false;;

(*filter_v0 is a high class function*)
let rec filter_v0 f l = (* (nat-> bool) -> list nat -> list nat *)
    match l with
    |[] -> []
    | (x::xs) -> if f x then x :: filter_v0 f xs else filter_v0 f xs;;

(*defunctionalize*)
type lam = | IsEven | IsPrime | IsPositive |And of lam * lam | Or of lam*lam;;


let rec apply p (*lam*) x = match p with
  |IsEven -> isEven x
  |IsPrime -> isPrime x
  |IsPositive -> isPositive x 
  |And (f,g) -> apply f x && apply g x
  |Or (f,g) -> apply f x || apply g x;; (* a better option?*)

let rec filter p x = match x with (*lam -> list nat -> list nat *) 
  |[] -> []
  | (x' :: xs) -> if apply p x' then x' :: (filter p xs) else filter p xs;;
