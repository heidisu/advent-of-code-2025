open System.IO
open System

let readFile () =
    File.ReadLines "input.txt" |> Seq.toList

let rec rotate acc curr (codes: string list) =
    match codes with
    | [] -> curr :: acc
    | x :: xs ->
        let dir = x[0]
        let steps = x.Substring(1) |> int

        match dir with
        | 'L' ->
            let diff = (curr - steps) % 100
            let newCurr = if diff < 0 then 100 + diff else diff
            rotate (curr :: acc) newCurr xs
        | 'R' ->
            let newCurr = (curr + steps) % 100
            rotate (curr :: acc) newCurr xs
        | _ -> failwith "unsupported dir"


let rec rotate2 acc curr (codes: string list) =
    match codes with
    | [] -> acc
    | x :: xs ->
        let dir = x[0]
        let steps = x.Substring(1) |> int

        match dir with
        | 'L' ->
            let diff = (curr - steps) % 100
            let newCurr = if diff < 0 then 100 + diff else diff
            let rounds = abs ((curr - steps) / 100)

            let passedZero =
                if diff < 0 && curr <> 0 || newCurr = 0 then
                    rounds + 1
                else
                    rounds

            rotate2 (acc + passedZero) newCurr xs
        | 'R' ->
            let newCurr = (curr + steps) % 100
            let passedZero = (curr + steps) / 100
            rotate2 (acc + passedZero) newCurr xs
        | _ -> failwith "unsupported dir"

let task1 =
    let lines = readFile ()
    rotate [] 50 lines |> List.filter (fun n -> n = 0) |> List.length

let task2 =
    let lines = readFile ()
    rotate2 0 50 lines

printfn $"Task 1: {task1}" // 1036
printfn $"Task 2: {task2}" // 6228
