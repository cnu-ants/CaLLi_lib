
  module M = Map.Make(String)
  type t = {function_map : (Function.t) M.t; globals : Global.t list}
  
  let empty = M.empty

  let find s m : Function.t = M.find s m
  let add = M.add
  let fold = M.fold
  let iter = M.iter

  let main m = 
    match M.find_first_opt (fun key -> String.starts_with ~prefix:"main" key) m with
    | Some (_,f) -> f
    | None -> failwith "No main function exists"
  
  let next (bb_name : String.t) m : Basicblock.t list = 
    let bb = Bbpool.find_bb bb_name in
    let func = bb.func_name in
    let cfg = (find func m).cfg in
    Cfg.next bb cfg

  let pp ppf m =
    M.iter (fun k v -> 
      let f : Function.t = v in
      Format.fprintf ppf "%s -> %s@\n" k f.function_name) m
    
