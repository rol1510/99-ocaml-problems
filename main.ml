(* Problem 1 *)
let rec last (x: 'a list): 'a option =
  match x with
  | [] -> None
  | [a] -> Some a
  | _ :: t -> last t

let rec last_v2 (x: 'a list): 'a option =
  match List.rev x with
  | [] -> None
  | [a] -> Some a
  | a :: _ -> Some a

(* Problem 2 *)
let rec last_two (x: 'a list): ('a * 'a) option =
  match x with
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: t -> last_two t

