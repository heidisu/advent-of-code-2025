open System.IO

type Present = { Id: int; Shape: (int * int) list }

type Region =
    { Size: int * int
      PresentIdToQuantity: Map<int, int> }

let readFile () =
    let (presents, regions, _, _) =
        File.ReadLines "input.txt"
        |> Seq.toList
        |> List.fold
            (fun (p, r, (i: int Option), currentPiece) l ->
                if l = "" then
                    let grid =
                        currentPiece
                        |> List.mapi (fun y l -> l |> Seq.toList |> List.mapi (fun x c -> (x, y, c)))
                        |> List.collect id
                        |> List.filter (fun (x, y, c) -> c = '#')
                        |> List.map (fun (x, y, _) -> (x, y))

                    let newPresent = { Id = Option.get i; Shape = grid }
                    (newPresent :: p, r, None, [])
                else if l.Contains "x" then
                    let parts = l.Split(": ")

                    let presentRequirements =
                        parts[1].Split(" ") |> Array.mapi (fun i v -> (i, int v)) |> Map.ofArray

                    let size = parts[0].Split("x") |> Array.map int

                    let requirement =
                        { Size = (size[0], size[1])
                          PresentIdToQuantity = presentRequirements }

                    (p, requirement :: r, None, [])
                else if l.Contains "#" || l.Contains "." then
                    (p, r, i, currentPiece @ [ l ])
                else
                    let i = l.Split(":")[0] |> int
                    (p, r, Some i, currentPiece))
            ([], [], None, [])

    (presents, regions)

let task1 =
    let presents, regions = readFile ()

    let obviousValidRegions =
        regions
        |> List.filter (fun r ->
            let tileAreas =
                presents
                |> List.map (fun p ->
                    let quantity = Map.find p.Id r.PresentIdToQuantity
                    9 * quantity // prentend shapes are 3 x 3, if the filled blocks can fit, the actual shapes will fit
                )
                |> List.sum

            let regionArea = fst r.Size * snd r.Size
            tileAreas <= regionArea)

    let obviousInvalidRegions =
        regions
        |> List.filter (fun r ->
            let tileAreas =
                presents
                |> List.map (fun p ->
                    let quantity = Map.find p.Id r.PresentIdToQuantity
                    List.length p.Shape * quantity)
                |> List.sum

            let regionArea = fst r.Size * snd r.Size
            tileAreas > regionArea)

    if List.length regions = List.length obviousValidRegions + List.length obviousInvalidRegions then
        obviousValidRegions |> List.length
    else
        failwith "Inconclusive"

let task2 = "finished!"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
