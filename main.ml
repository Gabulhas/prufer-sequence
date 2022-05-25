type graph = (int * int) list
type prufer_sequence = int list

(*GRAPH OPERATION*)
let remove_vertice vertice graph=
    graph |> List.fold_left (fun r (a, b) ->
        if a == vertice || b == vertice then 
            r
        else
            (a, b) :: r
    ) []

(*GRAPH OPERATION*)
let add_edge graph a b=
    (a, b) :: graph

(*Há uma forma melhor de fazer isto, mas envolve usar outras DSs *)
(*GRAPH OPERATION*)
let get_node_degrees graph =
    let rec find_and_update n = function
        | (a, count) :: tl when n  == a -> (n, count + 1) :: tl
        | h :: tl ->  h :: find_and_update n tl
        | [] -> (n, 1) :: []
    in
    List.fold_left (fun init (a, b) -> init |> find_and_update a |> find_and_update b) [] graph
        

(*GRAPH OPERATION*)
let find_leafs graph =
    let counts = get_node_degrees graph in
    List.fold_left (fun init (n, count) -> if count == 1 then n::init else init) [] counts


let rec minimum lowest =
    function 
    | a :: tl when a < lowest -> minimum a tl
    | a :: tl when a >= lowest -> minimum lowest tl
    | _ -> lowest

let find_lowest_leaf graph =
    minimum max_int (find_leafs graph)

(*GRAPH OPERATION*)
let get_node_neighbors n graph =
    List.fold_left (fun init (a,b) ->
        if n == a then
            b :: init
    else if n == b then
        a :: init
        else
            init
    ) [] graph

let get_leaf_neighbor n graph = 
    let rec aux = function 
        | (a,b) :: tl when a == n -> b
        | (a,b) :: tl when b == n -> a
        | h :: tl -> aux tl
        | _ -> assert false
    in
    aux graph


(*GRAPH OPERATION*)
let get_graph_size graph =
    List.length graph

let encode (graph:graph): prufer_sequence=
    let rec encode_aux g result =
        if get_graph_size g >= 2 then
            let lowest = find_lowest_leaf g in
            let neighbour = get_leaf_neighbor lowest g in
            encode_aux (remove_vertice lowest g) (neighbour :: result)
    else
        List.rev result
            in 
    encode_aux graph []

let rec generate_list n =
    let rec aux l result =
        if l > 0 then
            aux (l - 1) (l :: result)
        else
            List.rev result
    in
    aux n []

(* mais uma vez, se fosse usar outras estruturas de dados seria mais rápido*)
(* e altas nomes *)
let rec smallest_in_list_not_in_sequence the_list sequence = 
    List.filter (fun x -> not (List.mem x sequence)) the_list |> minimum max_int

let rec remove_value_from_list value = function
    | h :: tl -> if h == value then tl
        else h:: remove_value_from_list value tl
    | [] -> []



let decode (sequence:prufer_sequence) : graph=
    let nodes = generate_list (2 + List.length sequence) in
    let rec decode_aux s the_list graph = 
        match the_list with
        | a :: b :: [] -> add_edge graph a b 
        | a :: the_list_tl -> (
            match s with
            | h :: s_tl -> let smallest = smallest_in_list_not_in_sequence the_list sequence in
                          decode_aux (s_tl) (remove_value_from_list smallest the_list) (add_edge graph smallest h)
            | [] -> assert false

        )
        | [] -> assert false
    in
    decode_aux sequence nodes []
