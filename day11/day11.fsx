open System.IO
open System.Collections.Generic

let readFile () =
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l ->
        let parts = l.Split(": ")
        let source = parts[0]
        let destination = parts[1].Split(" ") |> Array.toList
        (source, destination))
    |> Map.ofList

let rec naiveSearch endKey paths map =
    let activePaths = Set.filter (fun l -> List.head l <> endKey) paths

    if Set.isEmpty activePaths then
        Set.count paths
    else
        let newPaths =
            Set.fold
                (fun s l ->
                    let nexts = Map.tryFind (List.head l) map

                    match nexts with
                    | Some nexts -> nexts |> List.map (fun n -> n :: l) |> List.fold (fun s l -> Set.add l s) s
                    | None -> s)
                Set.empty
                activePaths

        let removeOldPaths = activePaths |> Set.fold (fun s l -> Set.remove l s) paths
        let newSet = newPaths |> Set.fold (fun s l -> Set.add l s) removeOldPaths
        naiveSearch endKey newSet map

let rec fasterSearch (keyToNumPaths: Dictionary<string, int64>) curr endKey paths =
    if curr = endKey then
        1L
    else
        match keyToNumPaths.TryGetValue(curr) with
        | true, i -> i
        | false, _ ->
            let total =
                match Map.tryFind curr paths with
                | Some nexts ->
                    nexts
                    |> List.map (fun v -> fasterSearch keyToNumPaths v endKey paths)
                    |> List.sum
                | None -> 0L

            keyToNumPaths.Add(curr, total)
            total

let task1 = readFile () |> naiveSearch "out" (Set.singleton [ "you" ])

let task2 =
    let paths = readFile ()

    let dacFFtPaths =
        fasterSearch (Dictionary<string, int64>()) "svr" "dac" paths
        * fasterSearch (Dictionary<string, int64>()) "dac" "fft" paths
        * fasterSearch (Dictionary<string, int64>()) "fft" "out" paths

    let fftDacPaths =
        fasterSearch (Dictionary<string, int64>()) "svr" "fft" paths
        * fasterSearch (Dictionary<string, int64>()) "fft" "dac" paths
        * fasterSearch (Dictionary<string, int64>()) "dac" "out" paths

    dacFFtPaths + fftDacPaths

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
