
(** 
  After 18.0.1
**)
(* 
let loop_unroll m = 
  let pass_option = Llvm_passbuilder.create_passbuilder_options () in
  let target_triple_str = Llvm.target_triple m in
  let target = Llvm_target.TargetMachine.create target_triple_str (Llvm.target.Target.by_triple target_triple_str) in
  let _ = Llvm_passbuilder.passbuilder_options_set_loop_unrolling pass_option true in
  let (unit, str) = Llvm_passbuilder.run_passes m "-loop-unroll -unroll-count=3" in
  Format.printf "%s\n@." str *)

(* let get_triple m =
  let _ = Format.printf "triple : %s\n@." (Llvm.target_triple m) in
  match Llvm.target_triple m with
  | "" -> Llvm_target.Target.default_triple ()
  | triple -> triple

let get_target ~triple =
  let target = Llvm_target.Target.by_triple triple in
  Llvm_target.TargetMachine.create ~triple target

let loop_unroll m = 
  let _ = Format.printf "%s\n\n" (Llvm.string_of_llmodule m) in
  let _ = Llvm_all_backends.initialize () in
  let pm = Llvm.PassManager.create () in
  let b = Llvm_passmgr_builder.create () in
  let triple = get_triple m in
  let target = get_target ~triple:(triple) in
  let _ = Llvm.set_target_triple triple m in
  let _ = Llvm_target.TargetMachine.add_analysis_passes pm target in
  let _ = Llvm.PassManager.run_module m pm in
  let _ = Llvm_passmgr_builder.set_opt_level 0 b in
  let _ = Llvm_passmgr_builder.populate_module_pass_manager pm b in
  let res = Llvm.PassManager.run_module m pm in
  let _ = Format.printf "%b\n" res in 
  let _ = Format.printf "%s\n\n" (Llvm.string_of_llmodule m) in ()

   *)

let loop_unroll unroll_count bc_name =
  let output = "unrolled_"^bc_name in
  let unroll_count = "-unroll-count="^(string_of_int unroll_count) in
  let input = "target_to_unroll.ll" in
  let _ = Sys.command ("llvm-dis "^bc_name^" -o "^input) in
  let b = Sys.command ("sed s/optnone// "^input^" | opt -loop-unroll "^unroll_count^" -o "^output) in
  let _ = Format.printf "%d\n" b in
  output