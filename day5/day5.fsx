open System.IO

let readFile () =
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.fold
        (fun (a, b) l ->
            if l = "" then
                (a, b)
            else if l.Contains("-") then
                let parts = l.Split("-")
                let fresh = (int64 parts[0], int64 parts[1])
                (fresh :: a, b)
            else
                (a, int64 l :: b))
        ([], [])

let (fresh, ingredients) = readFile ()

let task1 =
    ingredients
    |> List.choose (fun i -> fresh |> List.tryFind (fun (a, b) -> i >= a && i <= b))
    |> List.length

let task2 =
    fresh
    |> List.fold
        (fun s (a, b) ->
            let overlaps =
                s
                |> Set.filter (fun (x, y) -> (a >= x && a <= y) || (b >= x && b <= y) || (x >= a && y <= b))
                |> Set.toList

            if List.isEmpty overlaps then
                Set.add (a, b) s
            else
                let min = min (overlaps |> List.map fst |> List.min) a
                let max = max (overlaps |> List.map snd |> List.max) b
                let newSet = List.fold (fun s i -> Set.remove i s) s overlaps
                Set.add (min, max) newSet)
        Set.empty
    |> Set.fold (fun a (x, y) -> a + (y - x) + 1L) 0L

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
