open System.IO

let readFile () =
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map Seq.toList
    |> List.map (List.map (fun c -> int64 c - int64 '0'))

let findMax line idx =
    let first = List.item idx line
    let rest = List.splitAt (idx + 1) line |> snd
    first * 10L + List.max rest

let rec findmax2 size acc =
    if size = 0 then
        acc
    else
        let newAcc =
            acc
            |> List.collect (fun (a, line) ->
                let firstPart = line |> List.splitAt (List.length line - size + 1) |> fst
                let max = List.max firstPart
                let idxs = firstPart |> List.indexed |> List.filter (fun (i, e) -> e = max)
                idxs |> List.map (fun (i, e) -> (e :: a, List.splitAt (i + 1) line |> snd)))

        findmax2 (size - 1) newAcc

let rec makeNum l acc =
    match l with
    | [] -> acc
    | x :: xs -> makeNum xs (acc * 10L + x)

let task1 =
    readFile ()
    |> List.map (fun l -> [ 0 .. List.length l - 2 ] |> List.map (findMax l))
    |> List.map List.max
    |> List.sum

let task2 =
    readFile ()
    |> List.map (fun l -> findmax2 12 ([ [], l ]))
    |> List.map (fun l -> l |> List.map (fun (a, b) -> makeNum (List.rev a) 0L) |> List.max)
    |> List.sum

printfn $"Task 1: {task1}" // 17263
printfn $"Task 2: {task2}" // 170731717900423
