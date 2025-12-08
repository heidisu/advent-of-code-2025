open System.IO

let readFile () =
    File.ReadAllText "input.txt"
    |> _.Split(",")
    |> Array.toList
    |> List.map (fun s ->
        let parts = s.Split("-")
        parts[0], parts[1])

let search (a, b) =
    let aInt = a |> int64
    let bInt = b |> int64

    [ aInt..bInt ]
    |> List.map (fun i ->
        let length = i.ToString().Length
        let half = length / 2
        let factor = pown 10L half

        if length % 2 = 0 && i / factor = i % factor then
            Some i
        else
            None)
    |> List.choose id

let search2 (a, b) =
    let aInt = a |> int64
    let bInt = b |> int64

    [ aInt..bInt ]
    |> List.map (fun i ->
        let str = i.ToString()
        let factors = [ 1 .. str.Length - 1 ] |> List.filter (fun n -> str.Length % n = 0)

        let repeats =
            factors
            |> List.map (fun f -> Seq.chunkBySize f str |> Seq.toList)
            |> List.map (fun a -> a |> List.map (fun a -> new System.String(a)))
            |> List.filter (fun a -> a |> List.groupBy id |> List.length = 1)
            |> List.map (fun s -> s |> List.fold (+) "" |> int64)

        if List.length repeats > 0 then Some repeats else None

    )
    |> List.choose id
    |> List.collect id
    |> List.distinct

let task1 =
    readFile ()
    |> List.filter (fun (a, b) -> Seq.length a % 2 = 0 || Seq.length b % 2 = 0 || Seq.length a <> Seq.length b)
    |> List.map search
    |> List.collect id
    |> List.sum

let task2 = readFile () |> List.map search2 |> List.collect id |> List.sum

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
