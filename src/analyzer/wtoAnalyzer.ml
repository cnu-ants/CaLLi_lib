module F = Format
module WTO = WeakTopologicalOrder

(** 
  Functor building an implementation of the Analyzer
given abstract domain, abstract memory, analysis context, abstract states, abstract semantics.
 *)
module Make 
(AbsVal : AbstractDomain.S)
(AbsMem : AbstractMemory.S with type valty = AbsVal.t) (Ctxt : Context.S with type memty = AbsMem.t) 
(States : States.S with type ctxtty = Ctxt.t and type memty = AbsMem.t) 
(TF : AbstractSemantics.S with type memty = AbsMem.t)
=
  struct
  
  (** Interprocedural Control Flow Graph module tailored to the user-provided 
  context and abstract memory.*)
  (* module Icfg = Icfg.Make(AbsMem)(Ctxt) *)

  module Icfg = Icfg.Make(AbsMem)(Ctxt)
  module WTO = WeakTopologicalOrder.Make (Icfg)


  (** 
  A loop_counter for widening operations in the analysis loop, visiting the same 
  basicblock for a maximum count of loop_counter before performing widening.
  *)
  module LoopCounter = struct 
  
    module M = Map.Make(struct 
      type t = string
      let compare = compare
    end)

    let empty : int M.t = M.empty

    let max_count : int ref = ref 0

    let set_max_count i : unit =
      max_count := i

    let lc = ref empty

    let mem = M.mem
    let find = M.find

    let update bb_ctxt = 
      let _ = lc := 
        if mem bb_ctxt !lc
        then M.add bb_ctxt ((find bb_ctxt !lc) + 1) !lc
        else M.add bb_ctxt 1 !lc
      in ()

    let widen bb_ctxt = 
      find bb_ctxt !lc > !max_count
  end


  let llmodule = ref Module.empty
  let icfg = ref Icfg.empty
  let summary = ref States.empty


  (** analysis loop *)
  let analyze (states) = 
    let stable prev post =
      if AbsMem.(post <= prev) then
        true
      else false
    in
    let rec analyze_v (v : string WeakTopologicalOrder.Partition.t) s head = 
      match v with
      | Vertex {vertex} -> 
        (* let _ = Format.printf "%s\n@." vertex in *)
        let bb = Bbpool.find_bb vertex in
        let _ = LoopCounter.update (bb.bb_name) in
        let states = 
          if States.mem' bb s then States.find bb s else States.CtxtM.empty in
        States.fold'
        (fun ctxt _ s ->
          (* let _ = Format.printf "   %a\n@." Ctxt.pp ctxt in *)
          let prev_mem = 
            if States.mem (bb, ctxt) s then
              States.find_mem (bb, ctxt) s
            else
              (AbsMem.empty)
          in
          let prev = 
            if States.mem (bb, ctxt) !summary then
              States.find_mem (bb, ctxt) !summary
          else 
              AbsMem.empty
          in
          let post = TF.transfer bb prev_mem in
          let mem = 
            if (stable prev post) || (head = false) then
              post
            else 
              if LoopCounter.widen (bb.bb_name) then 
                (* let _ = Format.printf "widen\n@." in *)
                AbsMem.widen prev post
              else AbsMem.(join prev post)
          in
          let _ = summary := States.update (bb, ctxt) mem !summary in
          List.fold_left 
          (fun s' ((bb:Basicblock.t), ctxt) -> 
            (* let _ = Format.printf " => update to %s, %a\n@." bb.bb_name Ctxt.pp ctxt in *)
            States.update (bb, ctxt) mem s') 
          s (Icfg.next bb ctxt mem !icfg !llmodule)
        )
        (states)
        s
      | Component {head; rest;} ->
        let bb = 
          (match head with 
          | Vertex {vertex} -> Bbpool.find_bb vertex
          | Component _ -> failwith "head cannot be component"
          )
        in
        States.fold'
        (fun ctxt _ s ->

          (* nth iteration of a partition 
            A LoopCounter counts number of visiting 'head'
            if lc < max_count, widening at the 'head' until stablization (fixpoint).
          *)
          let prev = 
            if States.mem (bb, ctxt) !summary then
              States.find_mem (bb, ctxt) !summary
            else 
              AbsMem.empty
          in
          let states = analyze_v head s true in
          let post = States.find_mem (bb, ctxt) !summary in
          if stable prev post then
            analyze_partition rest states
          else
            let states = analyze_partition rest states in
            analyze_v v states false
        )
        (States.find bb s)
        s
   
    and analyze_partition p s = 
      match p with
      | [] -> s
      | hd :: tl -> analyze_partition tl (analyze_v hd s false)
    in
    let main = Module.main !llmodule in
    let entry = Bbpool.find (main.function_name^"#"^"entry") !Bbpool.pool in
    let wto = WTO.wto_scc !icfg entry in 
    analyze_partition wto states


    (** initilizing llmodule and icfg. must be called before ```analyze``` function called *)
    let init (llm : Module.t) = 
      let _ = llmodule := llm.function_map in
      let _ = icfg := Icfg.make llm.function_map in

      let mem = 
        List.fold_left
        (fun mem (v : Global.t) ->
          TF.abs_interp_global v mem
        )
        AbsMem.empty llm.globals in
      mem

      
      
 end


