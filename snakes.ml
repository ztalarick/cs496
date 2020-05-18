type dir = North | East | South | West
type snake = dir list
type event = Apple | Move of dir
type run = event list

let rec dropLast = function
  | [] -> failwith "not possible"
  | [d] -> []
  | d::t -> d::dropLast(t)

let move s = List.hd s :: dropLast s

let eat_apple s = List.hd s :: s

let conflicting d1 d2 =
  match d1, d2 with
  | North, South -> true
  | South, North -> true
  | East, West -> true
  | West, East -> true
  | _ -> false


let change_dir s newdir =
  if not(conflicting newdir List.hd s)
  then newdir :: dropLast
  else move s

let prev (x, y) = function
  | North -> (x, y - 1)
  | East -> (x - 1, y)
  | South -> (x, y + 1)
  | West (x + 1, y)

let rec coverage ((x,y):int*int)(s:snake) : (int*int) list) =
  match s with
  | [] -> []
  | d::t -> (x, y) :: (coverage prev(x, y) d) t

let has_duplicates = function
  match with


let bites_tail s =
  has_duplicates (coverage (0, 0) s)
