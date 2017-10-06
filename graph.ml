exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let find_leaves g =
  List.filter (fun n -> n.n_link_to = []) g.g_nodes

let has_cycle g = 
	let roots = find_roots g in
	if roots = [] then true 
    else 
	    begin 
	        clear_marks g ;
	        let rec aux l = match l with
	          | [] -> false
	          | h :: t -> match h.n_mark with 
	            | InProgress | Visited -> true
	            | _ -> h.n_mark <- InProgress ;
	                   aux h.n_link_to ;
	                   aux t
	        in
	        aux roots
        end
    ;
;;


let topological g = 
	let tri = ref [] in
	let rec aux l = match l with
	  | [] -> ()
	  | h :: tail -> h.n_mark <- InProgress ;
                     aux (List.filter (fun n -> n.n_mark = NotVisited ) h.n_linked_by)  ;
                     tri := h.n_label :: !tri ;
                     h.n_mark <- Visited ;
                     aux tail
    in 
    aux (find_leaves g) ;
    List.rev !tri
;;
