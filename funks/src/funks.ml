module type Stack = sig
    type elt
    type t
    val create : unit -> t
    val push : elt -> t -> t
    val pop : t -> (elt * t) option
    val peek : t -> elt option
end

module type StackElt = sig
    type t
end

module StackImpl (X : StackElt) : Stack with type elt = X.t = struct
    type elt = X.t
    type t = X.t list

    let create () = []

    let push x stack = x :: stack

    let pop stack =
        match stack with
        | [] -> None
        | x :: xs -> Some (x, xs)

    let peek stack =
        match stack with
        | [] -> None
        | x :: _ -> Some x
end

module IntStack = StackImpl(struct type t = int end)
module StringStack = StackImpl(struct type t = string end)

let print_top peek stack =
    match peek stack with
    | Some top -> Printf.printf "Top of the stack: %s\n" top
    | None -> Printf.printf "Stack is empty\n"

let () =
    let stack = IntStack.(create () |> push 1 |> push 2 |> push 3) in
    print_top (fun stack -> match IntStack.peek stack with Some x -> Some (string_of_int x) | None -> None) stack;

    let stack = StringStack.(create () |> push "meow" |> push "meeeoow" |> push "meeew") in
    print_top StringStack.peek stack;

    (*
    let stack = IntStack.(create () |> push "meow" |> push "meow" |> push "meow") in
    print_top IntStack.peek stack;
    *)
