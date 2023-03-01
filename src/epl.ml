open PMap
open Delimcc

type prob = float
module type ProbSig = sig
  type 'a pm
  type ('a,'b) arr
  val n    : int -> int pm
  val dist : (prob * 'a) list -> 'a pm
  val neg  : int pm -> int pm
  val sum  : int pm -> int pm -> int pm
  val prod : int pm -> int pm -> int pm
  val div  : int pm -> int pm -> int pm
  val modulo  : int pm -> int pm -> int pm
  val is_null : int pm -> bool pm
  val con  : bool pm -> bool pm -> bool pm
  val dis  : bool pm -> bool pm -> bool pm  
  val if_  : bool pm -> (unit -> 'a pm) -> (unit -> 'a pm) -> 'a pm
  val lam  : ('a pm -> 'b pm) -> ('a,'b) arr pm
  val app  : ('a,'b) arr pm -> ('a pm -> 'b pm)
end

                        
type 'a vc = V of 'a | C of (unit -> 'a pV)
and  'a pV = (prob * 'a vc) list

let explore (maxdepth : int option) (choices : 'a pV) : 'a pV =
  let rec loop p depth down choices ((ans,susp) as answers) =
    match choices with
    | [] -> answers
    | (pt, V v)::rest ->
       loop p depth down rest
         (PMap.insert_with (+.) v (pt *. p) ans, susp)
    | (pt, C t)::rest when down ->
       let down' =
         match maxdepth with
         | Some x -> depth < x
         | None -> true
       in loop p depth down rest
            (loop (pt *. p) (depth + 1) down' (t ()) answers)
    | (pt, c)::rest ->
       loop p depth down rest (ans, (pt *. p,c)::susp) in
  let (ans,susp) = loop 1.0 0 true choices (PMap.empty,[])
  in PMap.foldi (fun v p a -> (p, V v)::a) ans susp

let normalize l = 
  let total = List.fold_left (fun acc (p,_) -> p +. acc) 0.0 l in
  (total, List.map (fun (p,v) -> (p /. total,v)) l)   

let pv_unit (x : 'a) : 'a pV = [(1.0, V x)]
let rec pv_bind (m : 'a pV) (f : 'a -> 'b pV) : 'b pV =
  List.map (function
      | (p, V x) -> (p, C (fun () -> f x))
      | (p, C t) -> (p, C (fun () -> pv_bind (t ()) f)))
    m       


module Dice(S: ProbSig) = struct
  open S
     let is_even e = is_null (modulo e (n 2))

     let let_ e f = app (lam f) e

     let dice_model () =
       let p = 1./.6. in
       let_ (dist [(p, 1); (p, 2); (p, 3); (p, 4); (p, 5); (p, 6)]) (fun die1 ->
       let_ (dist [(p, 1); (p, 2); (p, 3); (p, 4); (p, 5); (p, 6)]) (fun die2 ->
       let_ (sum die1 die2) (fun sum_dice ->
       if_ (dis (is_even die1) (is_even die2)) (fun () -> sum_dice) (fun () -> dist []))))
end
               

module SearchTree = struct
  type 'a pm = 'a pV
  type ('a,'b) arr = 'a -> 'b pV

  let n = pv_unit
  let dist ch = List.map (fun (p,v) -> (p, V v)) ch
  let neg e = pv_bind e (fun x -> pv_unit (- x))
  let sum e1 e2 = pv_bind e1 (fun v1 ->
                      pv_bind e2 (fun v2 -> pv_unit (v1 + v2)))
  let prod e1 e2 = pv_bind e1 (fun v1 ->
                       pv_bind e2 (fun v2 -> pv_unit (v1 * v2)))
  let div e1 e2 = pv_bind e2 (fun v2 ->
                      if v2=0 then raise Division_by_zero else pv_bind e1 (fun v1 -> pv_unit (v1 / v2)))
  let modulo e1 e2 = pv_bind e2 (fun v2 ->
                       if v2=0 then raise Division_by_zero else pv_bind e1 (fun v1 -> pv_unit (v1 mod v2)))
  let is_null e = pv_bind e (fun x -> pv_unit (x=0))
  let con e1 e2 = pv_bind e1 (fun v1 ->
                      if v1 then e2 else (pv_unit false))
  let dis e1 e2 = pv_bind e1 (fun v1 ->
                      if v1 then (pv_unit true) else e2)
  let if_ b e1 e2 = pv_bind b (fun t ->
                        if t then e1 () else e2 ())
  let lam e = pv_unit (fun x -> e (pv_unit x))
  let app e1 e2 = pv_bind e1 (pv_bind e2)
end

                      
module CPS = struct
  type 'a pm = ('a -> int pV) -> int pV
  type ('a,'b) arr = 'a -> ('b -> int pV) -> int pV

  let n x = fun k -> k x
  let dist ch = fun k ->
    List.map (function (p,v) -> (p, C (fun () -> k v))) ch
  let neg e = fun k -> e (fun v -> k (- v))
  let sum e1 e2 = fun k -> e1 (fun v1 ->
                               e2 (fun v2 ->
                                   k (v1 + v2)))
  let prod e1 e2 = fun k -> e1 (fun v1 ->
                                e2 (fun v2 ->
                                    k (v1 * v2)))
  let div e1 e2 = fun k -> e2 (fun v2 ->
                               if v2=0 then raise Division_by_zero else e1 (fun v1 ->
                                                                            k (v1 / v2)))
  let modulo e1 e2 = fun k -> e2 (fun v2 ->
                               if v2=0 then raise Division_by_zero else e1 (fun v1 ->
                                                                            k (v1 mod v2)))
  let is_null e = fun k -> e (fun v -> k (v=0))
  let con e1 e2 = fun k -> e1 (fun v1 ->
                               if v1 then e2 k else k false)
  let dis e1 e2 = fun k -> e1 (fun v1 ->
                               if v1 then (k true) else e2 k)
  let if_ et e1 e2 = fun k -> et (fun t ->
                                  if t then e1 () k else e2 () k)
  let lam e = fun k -> k (fun x -> e (fun k -> k x))
  let app e1 e2 = fun k -> e1 (fun f -> e2 (fun x -> f x k))
  let reify0 m = m pv_unit
end
  
module Direct = struct
  type 'a pm = 'a
  type ('a,'b) arr = 'a -> 'b

  let n x = x
  let (p0 : int pV prompt) = new_prompt ()
  let dist ch = shift p0 (fun k ->
                    List.map (function (p,v) -> (p, C (fun () -> k v))) ch)
  let neg e = - e
  let sum e1 e2 = e1 + e2
  let prod e1 e2 = e1 * e2
  let div e1 e2 = if e2=0 then raise Division_by_zero else e1/e2
  let modulo e1 e2 = if e2=0 then raise Division_by_zero else e1 mod e2
  let is_null e = e=0
  let con e1 e2 = e1 && e2
  let dis e1 e2 = e1 || e2
  let if_ et e1 e2 = if et then e1 () else e2 ()
  let lam e = e
  let app e1 e2 = e1 e2
  let reify0 m =
    push_prompt p0 (fun () -> pv_unit (m ()))
end                    
