type team = Korea | France |Usa |Brazil | Japan | Zigeria | Cameroon |Poland
          | Portugal | Italy | Germany | Norway | Sweden | England | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let string_of_team : team -> string = fun (team) ->
  match team with
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Zigeria -> "Zigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"


let rec parenize : tourna -> string = fun tournament ->
  match tournament with
  | LEAF team -> string_of_team team
  | NODE (ltour, rtour) -> "(" ^ parenize (ltour) ^ " " ^ parenize (rtour) ^ ")"

(* let a = parenize (NODE(LEAF Norway, NODE(NODE(LEAF Cameroon, LEAF Poland), LEAF Sweden)))
let b = parenize(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil))

let _ = print_endline (a) *)
