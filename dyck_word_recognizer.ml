(*dyck_word_recognizer.ml*)

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

(*end of dyck_word_recognizer.ml *)
