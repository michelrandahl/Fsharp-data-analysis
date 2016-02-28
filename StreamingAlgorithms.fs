// The simplest streaming algorithm for getting the most frequent element of a stream
// the x argument to the inner loop function, is the most frequent result so far
let get_most_frequent_element xs =
    let rec loop x counter = function
    | x'::tail when counter > 0 -> 
            if x' = x then
                loop x (counter + 1) tail
            else
                loop x (counter - 1) tail
    | x'::tail when counter = 0 -> 
            loop x' (counter + 1) tail
    | [] -> x // returning the most frequent element

    loop (List.head xs) 1 (List.tail xs)

// The Karp-Shenker-Papadimitriou Algorithm.
// We basically define two mutually recursive functions, and
// update a Map structure which, at most, contains the M most frequent elements and 
// their count.
// The function insert_phase describes the behavior of inserting and incrementing values,
// and the function delete_phase describe the behavior that determines when a phase has passed
// and how to decrement counters and remove elements from the Map
let karp_shenker_papdimitriou freq data_stream =
    let M = 1.0 / freq |> int

    let map_length map =
        Map.toSeq map
        |> Seq.length

    let rec insert_phase data_stream (map: Map<int,int>) =
        match data_stream with
        | data_elem::data_stream when Map.containsKey data_elem map ->
            let count = Map.find data_elem map // get the current count of data_elem
            Map.remove data_elem map
            |> Map.add data_elem (count+1) //update the counter for the data_elem in the map
            |> delete_phase data_stream  
        | data_elem::data_stream -> // insert fresh data_elem into the map
            Map.add data_elem 1 map
            |> delete_phase data_stream 
        | [] -> map // the stream has now finished and we can return the resulting map

    and delete_phase data_stream (map: Map<int,int>) =
        if map_length(map) > M then // determine if a 'phase' has passed
            Map.map (fun k c -> c - 1) map // decrement counters for all elements
            |> Map.filter (fun k c -> c > 0) // keep only elements where the counter is greater than zero
            |> insert_phase data_stream // send the updated Map to the insert_phase function
        else
            insert_phase data_stream map //if a phase has yet to pass, we just call insert_phase immediatly

    // initiate the inner function with the data_stream and a empty Map
    insert_phase data_stream Map.empty
