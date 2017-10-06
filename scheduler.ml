open Netlist_ast
open Graph

exception Combinational_cycle

let rec flat_map f l = match l with
  |[] -> []
	|h :: t -> (f h) @ (flat_map f t)

let read_exp eq =
	let l = ref [] in
	let add a l = match a with
	  |Avar(x) ->if not (List.mem x !l) then l := x :: !l ;
		|Aconst(x) -> () in
	let aux ex = match ex with
		|Earg(x) -> add x l
	  |Ereg(x) -> ()
		|Enot(x) -> add x l
		|Ebinop(_,a,b) -> add a l ; add b l
    |Emux(a,b,c) -> add a l ; add b l ; add c l
    |Erom(_,_,x) -> add x l
    |Eram(_,_,a,b,c,d) -> add a l
    |Econcat(a,b) -> add a l ; add b l
    |Eslice(_,_,x) -> add x l
    |Eselect(_,x) -> add x l
  in
  aux (snd eq) ;
  !l

let schedule p =
	let g = mk_graph () in
	List.iter (fun (x,y) -> add_node g x) p.p_eqs ;
	List.iter (fun x -> add_node g x) p.p_inputs ;
	List.iter (fun (x,y) -> List.iter (fun z -> add_edge g x z) (read_exp (x,y))) p.p_eqs ;
	let l = topological g in
	if has_cycle g then raise Combinational_cycle
	else
	  begin
	    { p_eqs = List.rev (flat_map (fun x ->if List.mem x p.p_inputs then [] else [(x , List.assoc x p.p_eqs)];) l);
      p_inputs = p.p_inputs ;
      p_outputs = p.p_outputs ;
      p_vars = p.p_vars  }
		end ;
