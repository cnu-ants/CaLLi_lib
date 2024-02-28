module F = Format

module Partition = struct

  type 'a t = 
    | Vertex of {vertex:'a}
    | Component of {head:'a t; rest:'a t list}


  let rec pp fmt wto = 
    match wto with
    | Vertex {vertex} -> Format.fprintf fmt "[%s]" vertex
    | Component {head; rest} ->  
        let _ = Format.fprintf fmt "(%a " pp head in
        let _ = List.iter
          (fun v -> 
            Format.fprintf fmt "%a " pp v
          )
          rest
        in
        let _ = Format.fprintf fmt ")" in ()
  

end

module type S = sig

  type t
  module G : Graph.S
  val wto_scc : G.t -> Basicblock.t -> string Partition.t list
  val pp : Format.formatter -> string Partition.t list -> unit
end

module Make (G : Graph.S) : (S with module G = G) = struct 

  module DFN = Map.Make(String)
  module G = G
  type t = Basicblock.t Partition.t

  let pp _ partition = 
    List.iter
    (fun v -> Format.printf "%a " Partition.pp v 
    )
    partition

  let wto_scc (cfg : G.t) (entry : Basicblock.t) = 
    
    let dfn = ref DFN.empty in
    let _ = dfn := (G.fold
      (fun k _ dfn -> 
        DFN.add k 0 dfn
      )
      cfg
      DFN.empty)
    in
    let stack = Stack.create () in
    let num = ref 0 in

    (* function Visit *)
    let rec visit (vertex : string) (partition : string Partition.t list ref) : string Partition.t list * int = 
      let head = ref 0 in 
      let loop = ref false in
      let _ = Stack.push vertex stack in
      let _ = num := !num + 1 in
      let _ = dfn := DFN.add vertex !num !dfn in
      let _ = head := !num in
      let _ = loop := false in

      let _ = List.iter
      (fun (succ : Basicblock.t) -> 
        let dfn_val = DFN.find (succ.bb_name) !dfn in
        let min = 
          if dfn_val = 0 then
            let (p, h) = visit succ.bb_name partition in
            let _ = partition := p in
            h
          else dfn_val
        in
        if min <= !head then
          let _ = head := min in
          loop := true
      )
      (G.succ (Bbpool.find_bb vertex) cfg)
      in
      let _ = if !head = (DFN.find vertex !dfn) then
        (let _ = dfn := DFN.add vertex Int.max_int !dfn in
        let element = ref (Stack.pop stack) in
        if !loop then
          (let _ = while !element != vertex do
              let _ = dfn := DFN.add !element 0 !dfn in
              element := Stack.pop stack
            done
          in
          partition := (component vertex)::!partition)
        else 
          partition := Partition.Vertex {vertex=vertex;}::!partition
        )
      in
      (!partition, !head)
        

    (* function Component *)
    and component vertex = 
      let partition = ref [] in
      let _ = List.iter
      (fun (succ : Basicblock.t) -> 
        if DFN.find (succ.bb_name) !dfn = 0 then
          partition := fst (visit succ.bb_name partition)
      )
      (G.succ (Bbpool.find_bb vertex) cfg)
      in
      Partition.Component {head=Partition.Vertex {vertex=vertex;}; rest=(!partition);}

    in

    let partition = ref [] in
    let _ = partition := fst (visit entry.bb_name partition) in
    !partition

end


