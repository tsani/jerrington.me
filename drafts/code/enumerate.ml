type 'a susp = unit -> 'a

type 'a lazy_list =  'a cell susp
and 'a cell =
    | Nil
    | Cons of 'a * 'a lazy_list

type assignment = bool list

let rec print_assignment a = match a with
    | [] -> print_string "\n"
    | b :: bs -> print_string (if b then "1" else "0"); print_assignment bs

let enumerate_assignments (action : assignment -> (unit -> 'r) -> ('a -> 'r) -> 'r)
    (n : int)
    (fail : unit -> 'r) (return : 'a -> 'r) : 'r =
    let rec go n (a : assignment) (next : unit -> 'r) : 'r =
        if n = 0 then action a next return else
            go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
    in
    go n [] fail

let test n = enumerate_assignments
    (fun a fail return -> print_assignment a; fail ())
    n
    (fun () -> print_string "done!\n")
    (fun _ -> ())

let lazy_collect_assignments n : assignment lazy_list = fun () -> enumerate_assignments
    (fun a next select -> Cons (a, next))
    n
    (fun () -> Nil)
    (fun _ -> Nil)

let rec take n l =
    if n = 0 then [] else
    match l () with
    | Nil -> []
    | Cons (x, l) -> x :: take (n-1) l

let dispense_assignments n : (unit -> assignment option) =
    let rec dispense = ref begin fun () ->
        go n [] (fun () -> None)
    end and go n a next =
        if n = 0 then (dispense := next; Some a) else
            go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
    in
    fun () -> !dispense ()

let _ = Seq.of_dispenser (dispense_assignments 5) |> Seq.iter print_assignment
