module F = Format
open Apron
open Calli

let _ = 
  let _ = Init.init () in 
  let _ = Init.loop_unroll 3 in
  let _ = Init.transform_call () in
  let _ = Init.transform_select () in
  let _ = Init.transform_prune () in
  let _ = Init.make_llm () in
  let _ = Init.make_call_graph () in ()

module MyContext = CallSiteContext.Make(
  struct 
    type t = String.t
    type memty = AbsMemory.t
    let size = ref 3

    let llmodule = Init.llmodule ()
    let call_graph = Init.call_graph ()
    let pp fmt (s : t) = F.fprintf fmt "%s" s
  end
)

module TF = 
  struct 
    
    type memty = AbsMemory.t

    let llmodule = Init.llmodule ()

    let abs_eval (e : Expr.t) (mem: AbsMemory.t) =
      match e with
      | ConstInt {value; _} -> AbsValue1.alpha (IntLiteral value) ""
      | Name {name;_} -> 
        (try (match Env.find name !Env.env with 
        | a -> AbsMemory.find a mem
        ) with _ -> AbsValue1.top)
      | Void _ -> AbsValue1.bot
      | _ -> AbsValue1.top


    let abs_interp_stmt (stmt : Stmt.t) (mem: AbsMemory.t) : AbsMemory.t =
      let instr = stmt.inst in
      if mem = AbsMemory.bot then mem else
      match instr with
      | ICmp {name; cond; operand0; operand1; _} ->
        let v1 = abs_eval operand0 mem in
        let v2 = abs_eval operand1 mem in
        let res = AbsValue1.compop cond v1 v2 name in
        let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
        let _ = Env.env := Env.add name addr !Env.env in
        AbsMemory.update addr res mem
      | Select {name; operand0; operand1; _;} ->
        let v1 = abs_eval operand0 mem in
        let v2 = abs_eval operand1 mem in
        let res = AbsValue1.join v1 v2 in
        let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
        let _ = Env.env := Env.add name addr !Env.env in
        AbsMemory.update addr res mem
      | BinaryOp {name; op; operand0; operand1; _} ->
        let v1 = abs_eval operand0 mem in
        let v2 = abs_eval operand1 mem in
        let res : AbsValue1.t = AbsValue1.binop op v1 v2 name in
        let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
        let _ = Env.env := Env.add name addr !Env.env in
        AbsMemory.update addr res mem
      | Alloc {name; _} -> 
          let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
          let a = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 1) in
          let addr' = AbsValue1.alpha (AddrLiteral a) name in
          let mem' = AbsMemory.update addr addr' mem in
          let _ = Env.env := Env.add name addr !Env.env in
          mem'
      | Store {operand; name; _} -> 
        let v = abs_eval operand mem in
        let _ = (match v with
          | AbsInt v -> 
            let _ = Calli.Pp.printf ~color:Red "%a -> %a\n@." 
              Var.print v.var Interval.print (Abstract1.bound_variable (Oct.manager_alloc ()) v.abs v.var) in ()
          | _ -> ()
        ) in
        let a = Env.find name !Env.env in
        let a' = AbsMemory.find a mem in
        (match a' with
        | AbsAddr a'' ->
          let mem' = AbsValue1.AbsAddr.fold
            (fun a mem ->  AbsMemory.update a v mem ) a'' mem
          in mem'
        | _ ->  mem)
        
      | Load {name; operand; _} -> 
          let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
          let res = abs_eval operand mem in
          let res' = 
            (match res with
            | AbsAddr a -> 
              AbsValue1.AbsAddr.fold
              (fun a' v -> AbsValue1.join v (AbsMemory.find a' mem)) a AbsValue1.bot
            | AbsTop -> AbsValue1.top
            | AbsBot -> AbsValue1.bot 
            | AbsInt _ -> failwith "load error"
            ) in
          let mem' = AbsMemory.update addr res' mem in
          let _ = Env.env := Env.add name addr !Env.env in
          let _ = (match res' with
          | AbsInt v -> 
            let _ = Calli.Pp.printf ~color:Red "%a -> %a\n@." 
              Var.print v.var Interval.print (Abstract1.bound_variable (Oct.manager_alloc ()) v.abs v.var) in ()
          | _ -> ()
          ) in  
          mem'
      | ReturnSite {name; ty} ->
        let res = abs_eval (Expr.Name {ty=ty; name="ret"}) mem in
        let _ = (match res with
            | AbsInt v -> 
              let _ = Calli.Pp.printf ~color:Red "%a -> %a\n@." 
                Var.print v.var Interval.print (Abstract1.bound_variable (Oct.manager_alloc ()) v.abs v.var) in ()
            | _ -> ()
        ) in
        let addr = stmt.bb_name^"return" in
        let _ = Env.env := Env.add name addr !Env.env in
        let mem' = AbsMemory.update addr res mem in
        mem'      
      | _ -> mem



    let abs_interp_term' (term : Term.t) (mem : AbsMemory.t) = 
      if mem = AbsMemory.bot then mem else
      match term with
      | Br _ -> mem
      | CondBr _ -> mem
      | Ret {ret; bb_name} -> 
        let res = abs_eval ret mem in
        let addr = bb_name^(string_of_int (-1))^(string_of_int 0) in
        let _ = Env.env := Env.add "ret" addr !Env.env in
        let mem' = AbsMemory.update addr res mem in
        mem'
      | Exit _ -> mem
      | CallSite {callee; args; bb_name;_} -> 
        let call_func : Function.t = Module.find callee llmodule in
        let mem', _ = 
          List.fold_left
          (fun (mem, index) arg ->
            let param = List.nth call_func.params index in
            let res = abs_eval arg mem in
            let addr = bb_name^"param"^(string_of_int index) in
            let name = (Expr.typed_var_of_expr param).name in
            let _ = Env.env := Env.add name addr !Env.env in
            let mem' = AbsMemory.update addr res mem in
            (mem', index+1)
          )
          (mem, 0)
          args
        in
        mem'
      | Switch _ -> mem
      | _ -> mem

    let abs_interp_global (v : Global.t) mem = 
      let res = abs_eval v.value mem in
      let addr = "global"^v.name^(string_of_int 0) in
      let a = "global"^v.name^(string_of_int 1) in
      let addr' = AbsValue1.alpha (AddrLiteral a) v.name in      
      let mem' = AbsMemory.update addr addr' mem in
      let mem' = AbsMemory.update a res mem' in
      let _ = Env.env := Env.add v.name addr !Env.env in
      mem'


   let transfer (bb : Basicblock.t) (mem : AbsMemory.t)  =
     let mem' = List.fold_left
        (fun mem stmt ->
          let mem'' = abs_interp_stmt stmt mem in
          mem''
        )
        mem bb.stmts 
      in
      abs_interp_term' bb.term mem'

  end



module States = States.Make (MyContext) (AbsMemory)
module Analyzer = WtoAnalyzer.Make (AbsValue1) (AbsMemory) (MyContext) (States) (TF)

module Dot = To_dot.Make(States)

let _ = 
  let _ = Dot.make (Init.m ()) "main()" in 
  let _ = Dot.make (Init.m ()) "test()" in 
  let init_mem = Analyzer.init (Init.m ())  in 
  let main = Module.main (Init.llmodule ()) in
  let entry = Bbpool.find (main.function_name^"#"^"entry") !Bbpool.pool in
  let init_states = States.update (entry, MyContext.empty ()) init_mem States.empty in
  let _ = Analyzer.LoopCounter.set_max_count 5 in
  let _ = Analyzer.analyze init_states in
  let _ = F.printf "\n\nFin\nStates\n%a\n" States.pp !Analyzer.summary in
  let _ = F.printf "Env\n%a\n" Env.pp !Env.env in
  ()
