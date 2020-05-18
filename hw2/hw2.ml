(* Zachary Talarick
I pledge my honor that I have abided by the Stevens Honor System. *)

type ('a, 'b) dtree =
  | Leaf of 'b
  | Node of 'a * ('a, 'b) dtree * ('a, 'b) dtree

let tLeft = Node ('w', Node('x', Leaf 2, Leaf 5), Leaf 8)
let tRight = Node('w', Node('x', Leaf 2, Leaf 5), Node('y', Leaf 7, Leaf 5))

(* let rec dtree_height t =
  match t with
  | Leaf(_)  -> 0
  | Node(_, l, r) -> 1 + (max (dtree_height l) (dtree_height r)) *)

let rec dtree_height: ('a, 'b) dtree -> int = function
| Leaf(_)  -> 0
| Node(_, l, r) -> 1 + (max (dtree_height l) (dtree_height r))

let rec dtree_size: ('a, 'b) dtree -> int = function
| Leaf(_)  -> 1
| Node(_, l, r) -> 1 + (dtree_size l) + (dtree_size r)

(* let rec dtree_paths: ('a, 'b) dtree -> int list list = function
| Leaf(_) ->  []
| Node(_, l, r) -> [ [0] @ (dtree_paths_help l); [1] (dtree_paths_help r)]

let rec dtree_paths_help: ('a, 'b) dtree -> int list = function
  | Leaf(_) ->  []
  | Node(_, l, r) ->
    match l with
      | Leaf (_) -> [0]
      | Node (_, l, r) -> dtree_paths_help l
    match r with
      | Leaf (_) -> [1]
      | Node (_, l, r) -> dtree_paths_help r *)

let dTree_is_perfect: ('a, 'b) dtree -> bool = function
  | Leaf(_) -> true
  | Node(_, l, r) -> (dtree_height l) = (dtree_height r)

let rec dTree_map (f:('a->'c)) (g:('b->'d)) (t:(('a, 'b)dtree)) : ('c, 'd)dtree =
  match t with
  | Leaf(v) -> Leaf(g v)
  | Node(v, l, r) -> Node (f v, (dTree_map f g l), (dTree_map f g r))

let rec list_to_tree (l:char list): (char, int)dtree =
  match l with
  | [] -> Leaf(0)
  | h::t -> Node(h, list_to_tree t, list_to_tree t)


let rec replace_leaf_at_helper (t: ('a, int)dtree) (li: int list) (y:int) : ('a, int)dtree =
 match li, t with
 | [], _ -> Leaf(y)
 | _, Leaf(v) -> Leaf(y)
 | h::t , Node(v, l, r) when h = 0 -> Node(v, replace_leaf_at_helper l t y, r)
 | h::t , Node(v, l, r) when h = 1 -> Node(v, l, replace_leaf_at_helper r t y)


let rec replace_leaf_at t g =
  match g with
  | [] -> t
  | (a, b)::tail -> replace_leaf_at (replace_leaf_at_helper t a b) tail

let bf_to_tree (a, b) = replace_leaf_at (list_to_tree a) b
