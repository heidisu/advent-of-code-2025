open System.IO

type Machine = { Indicators: string }

let readFile () =
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l ->
        let parts1 = l.Split("] ")
        let str = parts1[0].Replace("[", "")

        let indicators =
            str
            |> Seq.mapi (fun i c -> (i, if c = '#' then 1 else 0))
            |> Seq.fold (fun a (i, n) -> if n = 1 then Set.add i a else a) Set.empty

        let parts2 = parts1[1].Split(" {")

        let buttons =
            parts2[0].Split(" ")
            |> Array.toList
            |> List.map (fun l -> l.Replace("(", "").Replace(")", "").Split(",") |> Array.map int |> Set.ofArray)

        let joltage =
            parts2[1].Replace("}", "").Split(",")
            |> Array.toList
            |> List.map (fun n -> int n)

        indicators, buttons, joltage)

let rec search (acc: int Set list) i indicators buttons =
    if acc |> List.contains indicators then
        i
    else
        let newAcc =
            if acc = [] then
                buttons
            else
                acc
                |> List.collect (fun l ->
                    buttons
                    |> List.map (fun b ->
                        b
                        |> Set.fold (fun a i -> if Set.contains i a then Set.remove i a else Set.add i a) l))

        search newAcc (i + 1) indicators buttons

let task1 = readFile () |> List.map (fun (i, b, _) -> search [] 0 i b) |> List.sum
let task2 = ""
printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
