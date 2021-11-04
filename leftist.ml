(* Na czas testowania zmianiamy *)
(*
type 'a queue =
| Leaf
| Node of 'a queue * int *int * 'a queue
*)
type 'a queue =     (* LewySyn ; WartoscKorzenia ; DlugoscPrawego ; PrawySyn *)
|   Leaf
|   Node of 'a queue * 'a * int * 'a queue
let empty =     (* Pusty Lisc *)
    Leaf

exception Empty;; (* Dwa wyjatki, drugi powinien sie nie wlaczac *)
exception Blad;;

let wysokosc a =    (* Zwraca DlugoscPrawego *)
    match a with
    |   Node(_,_,h,_)  ->  h
    |   Leaf -> 0

let korzen a =      (* Zwraca WartoscKorzenia *)
    match a with
    |   Node(_,w,_,_) -> w
    |   Leaf -> raise Empty;;

let is_empty a =    (* Mowi czy drzewo jest puste *)
    if(wysokosc a = 0) then 
        true
    else
        false

let popraw a =      (* Ustawia poprawnie synow i liczy DlugoscPrawego *)
    match a with
    | Node(l,w,h,r)
        -> let x = wysokosc l in 
            let y = wysokosc r in
            if(x < y) then
                Node(r,w,x + 1,l)
            else
                Node(l,w,y + 1,r)
    | _ -> Leaf
    
let rec join a b =      (* Laczy drzewa *)
    match a,b with
    |   (Leaf,_) -> b
    |   (_,Leaf) -> a
    |   (Node(l1,w1,h1,r1),Node(l2,w2,h2,r2)) 
        ->  if(w1 > w2) then
                join b a
            else
                popraw (Node(l1,w1,0,join b r1))

let add e q =   (* Dodaje element do drzewa *)
    join (Node(Leaf,e,1,Leaf)) q

let delete_min a = (* Usuwa element z drzewa *)
    if(is_empty a = true) then
        raise Empty
    else
        match a with
        |   Node(l1,w,_,r1) -> (w, (join l1 r1))
        |   _ -> raise Blad;; (* nie powinno byc mozliwe *)
(*
(* WYPISANIE DRZEWA *)
let war a =
    (Printf.sprintf "%d" a)^" ";;
let rec dodaj q x n = 
    if n = 0 then
        q
    else
        dodaj (add  x q) (x+2) (n-1)
let tree q =
    let rec wypisz q a =
        match q with
        | Leaf -> a
        | Node(l,w,h,r) ->
            if(l = empty) then
                if(r = empty) then
                    (war w)^((war w)^a)
                else
                    (war w)^(wypisz r ((war w)^a))
            else
                if(r = empty) then
                    (war w)^(wypisz l ((war w)^a))
                else
                    (war w)^(wypisz l (war w)^(wypisz r (war w)^a))
    in wypisz q ""
    *)
(* Testy *)

(* Int tests *)
(*
let a = empty;;
assert (is_empty a = true);;
assert (try let _ = delete_min a in false with Empty -> true);;

let a = add (-5) a;;
assert ((delete_min a) |> fst = -5);;

let a = add 6 a;;
assert ((delete_min a) |> fst = -5);;

let a = add (-9) a;;
assert ((delete_min a) |> fst = -9);;

let a = add (-6) a;;
assert ((delete_min a) |> fst = -9);;

let a = add (-7) a;;
assert ((delete_min a) |> fst = -9);;

let a = add 0 a;;
assert ((delete_min a) |> fst = -9);;

let a = add (-5) a;;
assert ((delete_min a) |> fst = -9);;


let b = empty;;
let b = add 7 b;;
assert (is_empty b <> true);;

let b = add 3 b;;
assert ((delete_min b) |> fst = 3);;

let b = add (-1) b;;
assert ((delete_min b) |> fst = -1);;

let b = add 6 b;;
assert ((delete_min b) |> fst = -1);;

let b = add (-5) b;;
assert ((delete_min b) |> fst = -5);;

let b = add 2 b;;
assert ((delete_min b) |> fst = -5);;
(* siemano *)
let b = add 6 b;;
assert ((delete_min b) |> fst = -5);;

let b = add (-7) b;;
assert ((delete_min b) |> fst = -7);;

let b = add 6 b;;
assert ((delete_min b) |> fst = -7);;

let b = add (-7) b;;
assert ((delete_min b) |> fst = -7);;


let a = join a b;;


let (x, a) = delete_min a;;
assert (x = -9);;

let (x, a) = delete_min a;;
assert (x = -7);;

let (x, a) = delete_min a;;
assert (x = -7);;

let (x, a) = delete_min a;;
assert (x = -7);;

let (x, a) = delete_min a;;
assert (x = -6);;

let (x, a) = delete_min a;;
assert (x = -5);;

let (x, a) = delete_min a;;
assert (x = -5);;

let (x, a) = delete_min a;;
assert (x = -5);;

let (x, a) = delete_min a;;
assert (x = -1);;

let (x, a) = delete_min a;;
assert (x = 0);;

let (x, a) = delete_min a;;
assert (x = 2);;

let (x, a) = delete_min a;;
assert (x = 3);;

let (x, a) = delete_min a;;
assert (x = 6);;

let (x, a) = delete_min a;;
assert (x = 6);;

let (x, a) = delete_min a;;
assert (x = 6);;

let (x, a) = delete_min a;;
assert (x = 6);;

let (x, a) = delete_min a;;
assert (x = 7);;

assert (is_empty a = true);;
assert (try let _ = delete_min a in false with Empty -> true);;


(* Float tests *)
let a = empty;;
assert (is_empty a = true);;
assert (try let _ = delete_min a in false with Empty -> true);;

let a = add 5.657683 a;;
assert ((delete_min a) |> fst = 5.657683);;

let a = add 2.856745 a;;
assert ((delete_min a) |> fst = 2.856745);;

let a = add (-7.789243) a;;
assert ((delete_min a) |> fst = -7.789243);;

let a = add 9.999999 a;;
assert ((delete_min a) |> fst = -7.789243);;

let a = add (-8.123456) a;;
assert ((delete_min a) |> fst = -8.123456);;

let (x, a) = delete_min a;;
assert (x = -8.123456);;

let (x, a) = delete_min a;;
assert (x = -7.789243);;

let (x, a) = delete_min a;;
assert (x = 2.856745);;

let (x, a) = delete_min a;;
assert (x = 5.657683);;

let (x, a) = delete_min a;;
assert (x = 9.999999);;

assert (is_empty a = true);;
assert (try let _ = delete_min a in false with Empty -> true);;


(* String tests *)
let a = empty;;
assert (is_empty a = true);;
assert (try let _ = delete_min a in false with Empty -> true);;

let a = add "WPF" a;;
assert ((delete_min a) |> fst = "WPF");;

let a = add "LoveUW" a;;
assert ((delete_min a) |> fst = "LoveUW");;

let a = add "DrzewaLewicowe" a;;
assert ((delete_min a) |> fst = "DrzewaLewicowe");;

let a = add "Ocaml" a;;
assert ((delete_min a) |> fst = "DrzewaLewicowe");;

let a = add "2021/2022" a;;
assert ((delete_min a) |> fst = "2021/2022");;

let (x, a) = delete_min a;;
assert (x = "2021/2022");;

let (x, a) = delete_min a;;
assert (x = "DrzewaLewicowe");;

let (x, a) = delete_min a;;
assert (x = "LoveUW");;

let (x, a) = delete_min a;;
assert (x = "Ocaml");;

let (x, a) = delete_min a;;
assert (x = "WPF");;

assert (is_empty a = true);;
assert (try let _ = delete_min a in false with Empty -> true);;*)