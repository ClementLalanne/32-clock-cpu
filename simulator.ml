open Graph
open Netlist_ast
open Scheduler

exception Error
exception RomError
exception RamError

let rec fast_exp a q = match q with
 | 0 -> 1
 | 1 -> a
 | _ -> (fast_exp a (q/2)) * (fast_exp a (q/2)) * (fast_exp a (q mod 2))

let int_of_signed_array t =
  let n = Array.length t in
  let r = ref 0 in
  r := !r - (fast_exp 2 (n-1)) * t.(0) ;
  for k = 1 to (n-1) do
    r := !r + (fast_exp 2 (n-1-k)) * t.(k)
  done;
  !r

let int_of_char_new c = match c with
  | '0' -> 0
  | '1' -> 1
  | _ -> failwith (String.make 2 c)


let bool_of_int i = match i with
  | 0 -> false
  | 1 -> true
  | -1 -> true
  | _ -> failwith (string_of_int i)

let int_of_bool b = match b with
  | true -> 1
  |false -> 0

let value_of_string s =
  let n = String.length s in
  match n with
   |0 -> failwith "error l35 simulator"
   |1 -> VBit (bool_of_int (int_of_string s))
   | _ -> let t = Array.make n false in
      for k = 0 to (n-1) do
        t.(k) <- (bool_of_int (int_of_char_new s.[k]))
      done;
      VBitArray t

let string_of_value v = match v with
  |VBit(b) -> string_of_int (int_of_bool b)
  |VBitArray(t) -> let s = ref "" in
                   for k = 0 to (Array.length t) - 1 do
                     s := !s ^ (string_of_int (int_of_bool t.(k)))
                   done ;
                   !s

module Rmap = Map.Make (struct type t = ident let compare = compare end)

let ini_t ty = match ty with
  |  TBit  -> VBitArray (Array.make 1 false)
  | TBitArray n -> VBitArray (Array.make (n) false)

let initialise_regmap regmap prog =
  Env.iter ( fun c v -> regmap := Rmap.add c (ini_t v) !regmap) prog.p_vars

let rec ask_inputs l regmap= match l with
  |[] -> ()
  |t :: q -> try
    print_endline(t^" = ?");
    let n =  (read_line ()) in
    regmap := Rmap.add t (value_of_string n) (Rmap.remove t !regmap) ;
    ask_inputs q regmap
             with
             _ -> (print_endline("Invalid Syntax, try again.");  ask_inputs l regmap)

let rec print_outputs l regmap= match l with
 | [] -> ()
 | t :: q -> print_endline( t ^ " = " ^ (string_of_value (Rmap.find t !regmap))) ; print_outputs q regmap

 let read_file file = (*.csv file*)
   let lines = ref [] in
   let read = open_in file in
   try
     while true; do
     lines := input_line read :: !lines
     done; []
   with End_of_file -> close_in read ;
                         List.rev !lines

let make_romram file =
 let l = read_file file in
 match List.length l with
  | 0 -> (0,[||])
  | _ -> let n = String.length (List.hd l) in
         if not (List.for_all (fun x -> String.length x = n) l) then raise RomError ;
         let t = Array.make (n * List.length l) (VBit false) in
         let k = ref 0 in
         List.iter (fun s -> String.iter (fun c -> (t.(!k) <- VBit (bool_of_int (int_of_char_new c))) ; incr k )s ) l ;
         (n,t)


let rec write_file l file =
  let f = open_out file in
  let aux liste fichier = output_string fichier (String.concat "\n" liste) ; flush fichier in
  aux l f ;
  close_out f

let write_ram (n,r) file =
  let length = (Array.length r) / n in
  let l = ref [] in
  for k = 0 to (length - 1) do
    let c = ref "" in
    for i = 0 to (n-1) do
      c := !c ^ ( (fun (VBit x) -> string_of_int (int_of_bool x)) r.(k*n + i) )
    done;
    l := !c :: !l
  done;
  write_file (List.rev !l) file

(* Evaluation *)
let eval_arg a regmap = match a with
  |Avar(i) -> Rmap.find i !regmap
  |Aconst(x) -> x

let vnot v = match v with
  | VBit(x) -> VBit(not x)
  | VBitArray(t) -> VBitArray( Array.map (fun x -> not x) t)

let vor v1 v2 = match v1 , v2 with
  |VBit(x) , VBit(y) -> VBit( x || y)
  |VBit(x) , VBitArray(y) -> if Array.length y > 1 then failwith "142"
                             else VBit (x || y.(0));
  |VBitArray(y) , VBit(x) -> if Array.length y > 1 then failwith "144"
                             else VBit (x || y.(0));
  |VBitArray(x) , VBitArray(y) -> if (Array.length x) <> (Array.length y) then failwith "146"
                                  else begin
                                    let k = ref (-1) in
                                    VBitArray( Array.map (fun a -> incr k ; (a || y.(!k)) ) x)
                                  end

let vand v1 v2 =match v1 , v2 with
  |VBit(x) , VBit(y) -> VBit( x && y)
  |VBit(x) , VBitArray(y) -> if Array.length y > 1 then failwith "154"
                             else VBit (x && y.(0));
  |VBitArray(y) , VBit(x) -> if Array.length y > 1 then failwith "156"
                             else VBit (x && y.(0));
  |VBitArray(x) , VBitArray(y) -> if (Array.length x) <> (Array.length y) then failwith "158"
                                  else begin
                                    let k = ref (-1) in
                                    VBitArray( Array.map (fun a -> incr k ; (a && y.(!k)) ) x)
                                  end

let vxor v1 v2 = match v1 , v2 with
  |VBit(x) , VBit(y) -> VBit( (x || y) && not (x && y))
  |VBit(x) , VBitArray(y) -> if Array.length y > 1 then failwith "166"
                             else VBit ( (x || y.(0)) && not (x && y.(0)));
  |VBitArray(y) , VBit(x) -> if Array.length y > 1 then failwith "168"
                             else VBit ( (x || y.(0)) && not (x && y.(0)));
  |VBitArray(x) , VBitArray(y) -> if (Array.length x) <> (Array.length y) then failwith "170"
                                  else begin
                                    let k = ref (-1) in
                                    VBitArray( Array.map (fun a -> incr k ; ((a || y.(!k)) && not (a && y.(!k))) ) x)
                                  end

let vnand v1 v2 =match v1 , v2 with
  |VBit(x) , VBit(y) -> VBit( not(x && y))
  |VBit(x) , VBitArray(y) -> if Array.length y > 1 then failwith "178"
                             else VBit (not (x && y.(0)));
  |VBitArray(y) , VBit(x) -> if Array.length y > 1 then failwith "180"
                             else VBit (not(x && y.(0)));
  |VBitArray(x) , VBitArray(y) -> if (Array.length x) <> (Array.length y) then failwith "182"
                                  else begin
                                    let k = ref (-1) in
                                    VBitArray( Array.map (fun a -> incr k ; not (a && y.(!k)) ) x)
                                  end

let vconcat v1 v2 = match v1 , v2 with
  |VBit(x) , VBit(y) -> VBitArray ( [|x;y|])
  |VBit(x) , VBitArray(y) -> VBitArray (Array.append [|x|] y)
  |VBitArray(x) , VBit(y) -> VBitArray (Array.append x [|y|])
  |VBitArray(x) , VBitArray(y) -> VBitArray(Array.append x y)

let vslice n1 n2 v = match v with
  |VBit(x) -> if n1 = n2 && n2 = 0 then VBit (x)
              else failwith "196"
  |VBitArray(t) -> try VBitArray(Array.sub t n1 (n2-n1+1))
                   with _ -> failwith "198"

let vselect n v = match v with
  |VBit(x) -> if n = 0 then VBit(x)
              else failwith "202"
  |VBitArray(t) -> VBit(t.(n))

let vrom ads ws va rom rom_name =
  match va with
    |VBit(x) -> begin try VBitArray (Array.sub (Array.map (fun (VBit(b)) -> b) (snd rom)) (8*(int_of_signed_array [|int_of_bool x|])) ws)
                with _ -> failwith "Rom err line 198" end
    |VBitArray(t) -> try VBitArray (Array.sub (Array.map (fun (VBit(b)) -> b) (snd rom)) (8*(int_of_signed_array (Array.map int_of_bool t))) ws)
                    with _ -> failwith "Rom err line 200"

let to_VBitArray v = match v with
  | VBit(x) -> VBitArray([|x|])
  | _ -> v

let vram ads ws vra  vwe vwa vd ram ram_name =
  match vra with
    |VBit(x) ->begin
               let size = 8 *(int_of_signed_array [|int_of_bool x|]) in
               let ret =  try VBitArray (Array.sub (Array.map (fun (VBit(b)) -> b) (snd ram)) size ws)
                          with _ -> VBitArray (Array.make size false) in
               let x = (match vwe with
                          |VBit(v) -> [|v|]
                          |VBitArray(t) -> t) in
               (match (bool_of_int (int_of_signed_array (Array.map int_of_bool x))) with
                 | true -> begin
                    let VBitArray(t) = to_VBitArray vd in
                    let n = Array.length t in
                    let i = int_of_signed_array ((fun (VBitArray t) -> Array.map int_of_bool t) (to_VBitArray vwa)) in
                    for k = 0 to (n-1) do
                      (snd ram).(8*i+k) <- (VBit t.(k))
                    done;
                    ret
                    end
                |_ -> ret
                                          )
               end
    |VBitArray(t) ->
               begin
               let size = 8 * (int_of_signed_array (Array.map int_of_bool t)) in
               let ret = try VBitArray (Array.sub (Array.map (fun (VBit(b)) -> b) (snd ram)) size ws)
                         with _ -> VBitArray (Array.make size false) in
               let x = (match vwe with
                          |VBit(v) -> [|v|]
                          |VBitArray(t) -> t) in
               (match (bool_of_int (int_of_signed_array (Array.map int_of_bool x))) with
                 | true -> begin
                    let VBitArray(t) = to_VBitArray vd in
                    let n = Array.length t in
                    let i = int_of_signed_array ((fun (VBitArray t) -> Array.map int_of_bool t) (to_VBitArray vwa)) in
                    for k = 0 to (n-1) do
                      (snd ram).(8*i+k) <- (VBit t.(k))
                    done;
                    ret
                    end
                |_ -> ret
                                          )
               end

let vmux v1 v2 v3 = match v1 with
  | VBit(x) -> if x then v2 else v3
  | VBitArray(x) -> (match int_of_signed_array (Array.map int_of_bool x) with
    |1 -> v2
    |0 -> v3
    |_ -> failwith "une conditionnelle d'un mux ne vaut ni 0 ni 1")

let ident_of_arg a = match a with
  | Avar id -> id
  | Aconst v -> "constante"

let evaluation e  rom ram rom_name ram_name regmap = match e with
    | Earg (a : arg)   -> eval_arg a regmap
    | Ereg (x : ident) -> Rmap.find x !regmap
    | Enot (a : arg )  -> vnot (eval_arg a regmap)
    | Ebinop ((op : binop) , (a1 : arg) ,(a2 : arg) )->
       begin match op with
         |Or   -> vor (eval_arg a1 regmap) (eval_arg a2 regmap)
         |Xor  -> vxor (eval_arg a1 regmap) (eval_arg a2 regmap)
         |And  -> vand (eval_arg a1 regmap) (eval_arg a2 regmap)
         |Nand -> vnand (eval_arg a1 regmap) (eval_arg a2 regmap) end
    | Emux ((a1 :arg) , (a2 : arg ) , (a3 : arg ) )-> vmux (eval_arg a1 regmap) (eval_arg a2 regmap) (eval_arg a3 regmap)
    | Erom ( (ads : int)  , (ws : int) , (a : arg) )-> vrom ads ws (eval_arg a regmap) rom rom_name
    | Eram  ((ads : int)  , (ws : int)   , (ra : arg)  , (we : arg)  , (wa : arg)  , (d : arg)) -> vram ads ws (eval_arg ra regmap) (eval_arg we regmap) (eval_arg wa regmap) (eval_arg d regmap) ram ram_name
    | Econcat ( (a1 : arg) , (a2 : arg)) -> vconcat (eval_arg a1 regmap) (eval_arg a2 regmap)
    | Eslice ( (n1 : int) , (n2 : int) , (a : arg) ) -> vslice n1 n2 (eval_arg a regmap)
    | Eselect  ((n : int)  , (a : arg)) -> vselect n (eval_arg a regmap)

(*Simulator *)

let one_step p rom ram rom_name ram_name regmap =
  ask_inputs p.p_inputs regmap;
  List.iter (fun (a,e) ->regmap := Rmap.add a (evaluation e rom ram rom_name ram_name regmap) (Rmap.remove a !regmap)) p.p_eqs


let j_of_int i = match i with
  |0 -> "Lundi"
  |1 -> "Mardi"
  |2 -> "Mercredi"
  |3 -> "Jeudi"
  |4 -> "Vendredi"
  |5 -> "Samedi"
  |6 -> "Dimanche"
  | _ -> assert false

let affiche_date aaaa mm jds jj h m s =
  Format.printf "%d/%d/%d \n%dh%dm%ds \n%s@." aaaa (mm+1) (jj+1) h m s jds

let rec get_l_k l k = match k with
  | 0 -> List.hd l
  | _ -> get_l_k (List.tl l) (k-1)

let rec replace_l_k l k r= match (k,l) with
  | 0,t::q -> r :: q
  | _, t::q -> t :: (replace_l_k q (k-1) r)
  |_ -> failwith "n'arrive pas"

let array_of_string s =
  let n = String.length s in
  let t = Array.make n 0 in
  for k = 0 to (n-1) do
    t.(k) <- int_of_char_new (s.[k])
  done;
  t


let rec simulator netlist rom_name ram_name ini regmap mult =
  let p = Scheduler.schedule (Netlist.read_file netlist) in
  if ini then
    initialise_regmap regmap p ;
  let rec aux rom_name ram_name time =
    let ram = make_romram ram_name in
    let rom = make_romram rom_name in
    one_step p rom ram rom_name ram_name regmap ;
    write_ram ram ram_name ;
    if ((Sys.time () -. !time) > (1./.mult)) then
      begin
        time := Sys.time () ;
        let l = read_file "emptyram" in
        let lp = List.map array_of_string l in
        ignore (Sys.command "clear") ;
        affiche_date (int_of_signed_array (get_l_k lp 9))
                     (int_of_signed_array (get_l_k lp 8))
                     (j_of_int (int_of_signed_array (get_l_k lp 7)))
                     (int_of_signed_array (get_l_k lp 6))
                     (int_of_signed_array (get_l_k lp 5))
                     (int_of_signed_array (get_l_k lp 4))
                     (int_of_signed_array (get_l_k lp 3)) ;

        write_file (replace_l_k l 30 "00000000000000000000000000000000") "emptyram"
      end ;
    aux rom_name ram_name time in
  let time = ref (Sys.time ()) in
  aux rom_name ram_name time




let montre mult =
  write_file (["00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000"; (* 32*)
              "00000000000000000000000000000000";
              "00000000000000000000000000000001";
              "00000000000000000000000000111011"; (* minutes *)
              "00000000000000000000000000010111"; (*heures*)
              "00000000000000000000000000000101"; (*jour de la semaine*)
              "00000000000000000000000000011110"; (* jour du mois*)
              "00000000000000000000000000001011"; (*mois*)
              "00000000000000000000011111100000"; (* année*)
              "00000000000000000000000000000000"; (*mod 4 de l'annee*)
              "00000000000000000000000000010000"; (* mod 100 de l'annee*)
              "00000000000000000000000000010000"; (*mod 400 de l'année*)
              "00000000000000000000000000110100"; (*secondes *)
              "00000000000000000000000000011111";
              "00000000000000000000000000011100";
              "00000000000000000000000000011111";
              "00000000000000000000000000011110";
              "00000000000000000000000000011111";
              "00000000000000000000000000011110";
              "00000000000000000000000000011111";
              "00000000000000000000000000011111";
              "00000000000000000000000000011110";
              "00000000000000000000000000011111";
              "00000000000000000000000000011110";
              "00000000000000000000000000011111";
              "00000000000000000000000000011111";
              "00000000000000000000000000011101";
              "00000000000000000000000000011111";
              "00000000000000000000000000011110";
              "00000000000000000000000000011111";
              "00000000000000000000000000011110";
              "00000000000000000000000000011111";
              "00000000000000000000000000011111";
              "00000000000000000000000000011110";
              "00000000000000000000000000011111";
              "00000000000000000000000000011110";
              "00000000000000000000000000011111";
              "00000000000000000000000000000000";
              "00000000000000000000000000000000"]) "emptyram" ;
  let regmap = ref Rmap.empty in
  simulator "micro_processeur.net" "clock.byte" "emptyram"  true regmap mult;
  while true do
    simulator "micro_processeur.net" "clock.byte" "emptyram" false regmap mult;
   done

let () = montre (float_of_string Sys.argv.(1))
