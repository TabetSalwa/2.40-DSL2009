open Delimcc
open Epl
   
let rec print_pV =
  let print_vc = function
    | V i -> Format.printf "V %d" i
    | C _ -> Format.printf "C <fun>"
  in let rec print_pV_aux = function
    | [] -> ()
    | [(p,a)] -> Format.printf "(%f, " p; print_vc a; Format.printf ")"
    | (p,a)::l -> Format.printf "(%f, " p; print_vc a; Format.printf "); "; print_pV_aux l
  in fun m ->
     Format.printf "[";
     print_pV_aux m;
     Format.printf "]\n"
   
let normalize l = 
  let total = List.fold_left (fun acc (p,_) -> p +. acc) 0.0 l in
  List.map (fun (p,v) -> (p /. total,v)) l
   
module SST = Dice(SearchTree)
let sst1 = SST.dice_model ()
let sste1 = explore (Some 1) sst1
let sste10 = explore (Some 10) sst1
let sste13 = explore (Some 13) sst1
let sste15 = explore (Some 15) sst1
let () = Format.printf "\nThe search tree for an exploration depth of 1\n"
let () =  print_pV sste1
let () = Format.printf "\nThe search tree for an exploration depth of 10\n"
let () = print_pV sste10
let () = Format.printf "\nThe search tree for an exploration depth of 13\n"
let () = print_pV sste13
let () = Format.printf "\nThe search tree for an exploration depth of 15 with normalization\n"
let () = print_pV (normalize sste15)

module SCP = Dice(CPS)
let scp1 = CPS.reify0 (SCP.dice_model ())
let scpe0 = explore (Some 0) scp1
let scpe1 = explore (Some 1) scp1
let () = Format.printf "\n\n\nCPS search tree for an exploration depth of 0\n"
let () =  print_pV scpe0
let () = Format.printf "\nCPS search tree for an exploration depth of 1 with normalization\n"
let () = print_pV (normalize scpe1)
           
module SDI = Dice(Direct)
let sdi1 = Direct.reify0 (SDI.dice_model)
let sdie0 = explore (Some 0) sdi1
let sdie1 = explore (Some 1) sdi1
let () = Format.printf "\n\n\nSDI search tree for an exploration depth of 0\n"
let () =  print_pV sdie0
let () = Format.printf "\nSDI search tree for an exploration depth of 1 with normalization\n"
let () = print_pV (normalize sdie1)

(* Alternative syntax *)
open Direct
let dice_model () =
  let p = 1./.6. in
  let die1 = dist [(p, 1); (p, 2); (p, 3); (p, 4); (p, 5); (p, 6)] in
  let die2 = dist [(p, 1); (p, 2); (p, 3); (p, 4); (p, 5); (p, 6)] in
  if (die1 mod 2 = 0) || (die2 mod 2 = 0) then die1 + die2 else dist []
let sdi2 = reify0 dice_model
let sdie0 = explore (Some 0) sdi2
let sdie1 = explore (Some 1) sdi2
let () = Format.printf "\n\n\nSDI search tree for an exploration depth of 0(alternative syntax)\n"
let () =  print_pV sdie0
let () = Format.printf "\nSDI search tree for an exploration depth of 1 (alternative syntax) with normalization\n"
let () = print_pV (normalize sdie1)

