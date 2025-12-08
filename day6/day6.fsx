open System.IO

let readFile () =
    File.ReadLines "input.txt" |> Seq.toList

let parseOperators (operators: char list) =
    operators
    |> List.fold
        (fun s n ->
            match n with
            | ' ' -> s
            | '*' -> ((*), 1L) :: s
            | '+' -> ((+), 0L) :: s
            | _ -> failwith $"Unexpected operator: {n}")
        []

let calculate (columns: int64 list list) (operators: ((int64 -> int64 -> int64) * int64) list) =
    columns
    |> List.indexed
    |> List.map (fun (i, c) ->
        let (op, identity) = operators[i]
        List.fold (fun a n -> op n a) identity c)
    |> List.sum

let task1 =
    let lines =
        readFile ()
        |> List.map (fun l -> l.Split(" ") |> Array.toList |> List.map (fun e -> e.Trim()))

    let operators =
        lines |> List.last |> List.collect (fun s -> s |> Seq.toList) |> parseOperators

    let numbers =
        lines
        |> List.take (List.length lines - 1)
        |> List.map (fun l -> l |> List.fold (fun s n -> if n = "" then s else int64 n :: s) [])

    let height = List.length numbers
    let width = List.length numbers[0]

    let columns =
        [ 0 .. width - 1 ]
        |> List.map (fun x -> [ 0 .. height - 1 ] |> List.fold (fun s y -> numbers[y][x] :: s) [])

    calculate columns operators


let task2 =
    let lines = readFile () |> List.map (fun s -> s |> Seq.toList |> List.rev)
    let height = List.length lines - 1
    let width = List.length lines[0]

    let columns =
        [ 0 .. width - 1 ]
        |> List.map (fun x -> [ 0 .. height - 1 ] |> List.fold (fun s y -> lines[y][x] :: s) [] |> List.rev)

    let numbers =
        columns
        |> List.map (fun c -> System.String.Concat(c) |> fun str -> str.Trim())
        |> List.fold (fun (acc, curr) s -> if s = "" then (curr :: acc, []) else (acc, int64 s :: curr)) ([], [])
        |> (fun (acc, curr) -> curr :: acc)

    let operators = lines |> List.last |> parseOperators

    calculate numbers operators

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
