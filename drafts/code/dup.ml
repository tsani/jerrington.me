type 'a expf =
    | Var of 'a ref
    | Lam of 'a ref * 'a expf
    | App of 'a expf * 'a expf
    | LetDup of ('a ref * 'a ref) * 'a expf * 'a expf

(* Idea: implement substitutions thru mutable references.
   Problem: copies!
   If we 
 *)

type exp = Fold of exp option expf
let unfold (Fold e) = e

let var (r : exp option ref) : exp = Fold (Var r)

let lam (f : exp -> exp) : exp =
    let v = ref None in
    Fold (Lam (v, unfold @@ f (var v)))

let app (e1 : exp) (e2 : exp) : exp =
    Fold (App (unfold e1, unfold e2))


