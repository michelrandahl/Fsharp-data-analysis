let rnd = System.Random()

let merge_maps m1 m2 =
    [Map.toSeq m1; Map.toSeq m2]
    |> Seq.concat
    |> Map.ofSeq

let euclid_distance v1 v2 =
    Seq.zip v1 v2
    |> Seq.map (fun xy -> (fst xy - snd xy)**2.0)
    |> Seq.sum


let build_distance_map xs =
    let dist_map =
        xs |> Seq.map (
            fun x -> x,
                     xs
                     |> Seq.filter ((<>)x)
                     |> Seq.map (fun x' -> x', euclid_distance x x')
                     |> Map.ofSeq
            )
        |> Map.ofSeq
    xs
    |> Seq.map (fun x -> 
        x,
        xs
        |> Seq.filter ((<>)x)
        |> Seq.map (fun x' -> 
            let lookup_attempt1 = lazy( dist_map |> Map.find x |> Map.tryFind x' )
            let lookup_attempt2 = lazy( dist_map |> Map.find x' |> Map.find x )
            match lookup_attempt1.Value with
            | Some(d) -> x',d
            | None -> x',lookup_attempt2.Value)
        |> List.ofSeq)
    |> Map.ofSeq


let get_k_nearest k vs =
    vs |> List.sortBy snd |> List.take k

let calc_density (distances: float list) =
    let k = Seq.length distances |> float
    (Seq.sum distances / k)**(-1.0)

let calc_avg_relative_density curr_density k_nearest_densities =
    let k = Seq.length k_nearest_densities |> float
    curr_density / (Seq.map ((/)k) k_nearest_densities |> Seq.sum)


[<EntryPoint>]
let main argv = 
    let K = 5
    let vs = [for _ in 0..10 -> [for _ in 1..3 -> rnd.NextDouble()]] @ [[for _ in 1..3 -> rnd.NextDouble()+1.0]]
    let dmap = build_distance_map vs
    let k_nearest_dist_map =
        dmap
        |> Map.map (fun v vs -> get_k_nearest K vs)
    let k_nearest_density_map =
        k_nearest_dist_map
        |> Map.map (fun v vs -> vs |> List.map snd |> calc_density)
    k_nearest_dist_map
    |> Map.map (fun v vs -> vs |> List.map fst |> List.map (fun v' -> Map.find v' k_nearest_density_map))
    |> Map.map (fun v densities -> calc_avg_relative_density (Map.find v k_nearest_density_map) densities)
    |> Map.toList
    |> List.sortBy snd
    |> printfn "%A"
    0 // return an integer exit code
