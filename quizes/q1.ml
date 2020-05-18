type ’a tree = Empty | Node of 'a * ' a tree * ' a tree

(* let mt f l =
  match l with
  | [] -> Node(Empty, Empty)
  | h::t -> let (t1, t2) = split t
    Node(f h, mt t1, mt t2) *)


let rec split xs =
    match xs with
  | x::y::tail ->
        let a,b = separate tail in
        x::a, y::b
  | x::[] -> [x],[]
  | [] -> [],[];;
