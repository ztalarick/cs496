(*Zachary Talarick*)
(* I plege my honor that I have abided by the stevens honor system. *)

type coord = int*int
type coded_pic = coord list

let cp1:coded_pic = [(0,0);(2,0);(2,2);(0,2);(0,0)]
let cp2:coded_pic = [(0,0);(4,0);(4,4);(0,0)]

let tupple_mult (a, b) c = (a * c, b *c)

let rec stretch (p:coded_pic) (factor:int) : coded_pic =
  match p with
  | [] -> []
  | h::t ->  tupple_mult h factor :: stretch t factor

let stretch_m (p:coded_pic) (factor:int) : coded_pic = List.map (fun (x : int * int) -> tupple_mult x factor) p

let rec range a b =
    if a > b then []
    else a :: range (a + 1) b

let rec stutter n m = if m <= 0 then [] else n :: stutter n (m - 1)

(* this function needs testing, Check when the diagonal alignment, when each cord is seperate by the same distance *)
let rec segment (cx,cy) (nx,ny) =
  if cx == nx && cy == ny then []
  else (cx + compare nx cx, cy + compare ny cy) :: segment (cx+compare nx cx, cy+compare ny cy) (nx, ny)

let rec coverage_help ((start::p):coded_pic):coord list =
  match p with
  | [] -> []
  | h :: t -> List.append (segment start h) (coverage_help p)

let coverage ((start::p):coded_pic):coord list = start :: coverage_help p


let coverage_f_help l cord  = l @ (segment (List.hd (List.rev l)) cord)

let coverage_f ((start::p):coded_pic):coord list = List.fold_left coverage_f_help [start] p


let equivalent_pics (cp1:coded_pic) (cp2:coded_pic):bool = ((coverage_f cp1) = (coverage_f cp2)) || (cp1 = (coverage_f cp2)) || ((coverage_f cp1) = cp2)

let rec max_list (l:int list) (a:int) =
  match l with
  | [] -> a
  | h :: t when h > a -> max_list t h
  | h :: t -> max_list t a

let get0 (a, _) = a
let get1 (_, a) = a

let rec min_list_help l a =
  match l with
  | [] -> a
  | h :: t when h < a -> min_list_help t h
  | h :: t -> min_list_help t a

let min_list l = min_list_help l (List.hd l)

let height (p:coded_pic):int = (max_list (get1 (List.split p)) 0) - (min_list (get1(List.split p)))

let width (p:coded_pic):int = (max_list (get0 (List.split p)) 0) - (min_list (get0 (List.split p)))

let tile ((dx,dy):coord) (p:coded_pic) : coded_pic list list =
  failwith "Implement"


let tri_aligned ((x1,y1):coord) ((x2,y2):coord) ((x3,y3):coord):bool = ((abs (x1 - x2 - x3)) = (abs (y1 - y2 - y3))) || ((x1 = x2) && (x1 = x3))  || ((y1 = y2) &&  (y1 = y3))

let rec inList v l =
  match l with
  | [] -> false
  | h :: t when v = h -> true
  | h :: t -> inList v t

let three_join a b c = a::b::c

let rec compress (p:coded_pic):coded_pic =
  match p with
  | [] -> p
  | a::b::c::t -> if (inList b (segment a c)) then (compress (three_join a c t)) else a :: (compress (three_join b c t))
  | _ -> p
