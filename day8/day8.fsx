open System.IO

type JunctionBox = { X: int64; Y: int64; Z: int64 }

let readFile () =
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun s ->
        let parts = s.Split(",")

        { X = int64 parts[0]
          Y = int64 parts[1]
          Z = int64 parts[2] })


let junctionBoxes = readFile ()
let size = List.length junctionBoxes

let distance a b =
    pown (a.X - b.X) 2 + pown (a.Y - b.Y) 2 + pown (a.Z - b.Z) 2

let pairs =
    [ 0 .. size - 1 ]
    |> List.collect (fun x -> [ x + 1 .. size - 1 ] |> List.map (fun y -> (x, y)))

let pairToDist =
    pairs
    |> List.map (fun (x, y) -> ((x, y), distance junctionBoxes[x] junctionBoxes[y]))
    |> Map.ofList

let distances = pairToDist |> Map.values |> Seq.toList |> List.sort

let rec group groups pairToDist distances =
    match distances with
    | [] -> groups
    | x :: xs ->
        let candidates = pairToDist |> Map.filter (fun k v -> v = x) |> Map.keys

        if Seq.length candidates <> 1 then
            failwith "more than one match"

        let (a, b) = Seq.head candidates
        let groupsWithA = groups |> Set.filter (fun s -> Set.contains a s) |> Set.toList
        let groupsWithB = groups |> Set.filter (fun s -> Set.contains b s) |> Set.toList

        let groupA =
            if List.isEmpty groupsWithA then
                Set.empty
            else
                List.head groupsWithA

        let groupB =
            if List.isEmpty groupsWithB then
                Set.empty
            else
                List.head groupsWithB

        let setAB = Set.empty |> Set.add a |> Set.add b
        let newSet = Set.union groupA groupB |> Set.union setAB
        let newGroups = groups |> Set.remove groupA |> Set.remove groupB |> Set.add newSet
        group newGroups pairToDist xs

let rec group2 groups pairToDist distances =
    match distances with
    | [] -> None
    | x :: xs ->
        let candidates = pairToDist |> Map.filter (fun k v -> v = x) |> Map.keys

        if Seq.length candidates <> 1 then
            failwith "more than one match"

        let (a, b) = Seq.head candidates
        let groupsWithA = groups |> Set.filter (fun s -> Set.contains a s) |> Set.toList
        let groupsWithB = groups |> Set.filter (fun s -> Set.contains b s) |> Set.toList

        let groupA =
            if List.isEmpty groupsWithA then
                Set.empty
            else
                List.head groupsWithA

        let groupB =
            if List.isEmpty groupsWithB then
                Set.empty
            else
                List.head groupsWithB

        let setAB = Set.empty |> Set.add a |> Set.add b
        let newSet = Set.union groupA groupB |> Set.union setAB
        let newGroups = groups |> Set.remove groupA |> Set.remove groupB |> Set.add newSet

        if
            Set.count newGroups = 1
            && newGroups |> Set.toList |> List.head |> Set.count = size
        then
            Some(junctionBoxes[a].X * junctionBoxes[b].X)
        else
            group2 newGroups pairToDist xs

let task1 =
    let distances = distances |> List.take 1000

    group Set.empty pairToDist distances
    |> Set.toList
    |> List.map (fun s -> Set.count s)
    |> List.sort
    |> List.rev
    |> List.take 3
    |> List.reduce (*)

let task2 = group2 Set.empty pairToDist distances |> Option.get

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
