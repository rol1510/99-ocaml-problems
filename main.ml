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


(* Problem 3 *)
let rec list_nth k = function
  | [] -> None
  | h :: t -> if k = 0 then Some h else list_nth (k - 1) t


(* Problem 4 *)
let length (x: 'a list): int =
  let rec inner n = function
    | [] -> n
    | _ :: t -> inner (n + 1) t
  in 
    inner 0 x

(* Problem 5 *)
let reverse (x: 'a list): 'a list =
  let rec inner acc = function
    | [] -> acc
    | h :: t -> inner (h :: acc) t
  in  
    inner [] x

(* Problem 5 *)
let is_palindrome (x: 'a list): bool =
  x = reverse x

(* Problem 6 *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten x =
  let rec inner acc = function
    | [] -> acc
    | One h :: t -> inner (h :: acc) t
    | Many h :: t -> inner (inner acc h) t
  in 
    reverse (inner [] x)

