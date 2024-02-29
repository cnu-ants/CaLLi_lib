(**

The Stmt module is a module that adds additional information to Inst.
It includes bb_name (the name of the basicblock to which the instruction
belongs), and index (the index of the instruction within the basicblock)

 **)

module Loc = struct 
  type t = {line:int; col:int;}
  
  let pp fmt (l : t) = 
    Format.fprintf fmt "loc:(line:%d; col:%d)\n" l.line l.col
end

type t = {bb_name:string; index:int; inst:Inst.t; loc:Loc.t option}

let pp fmt (s : t) = 
  match s.loc with
  | None -> Format.fprintf fmt "%a\n" Inst.pp s.inst
  | Some loc -> Format.fprintf fmt "%a; %a\n" Inst.pp s.inst Loc.pp loc
