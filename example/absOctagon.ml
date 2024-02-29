open Apron;;
open Mpqf;;
open Format;;

module F = Format

type t = {var: Var.t; abs:Oct.t Apron.Abstract1.t; env:Environment.t}
type elt = Z.t

(* Oct.t Apron.Abstract1.t *)
  let man = Oct.manager_alloc ();;
  let index = ref 0
  let new_var () = 
    let _ = index := !index + 1 in 
    Var.of_string ("x"^(string_of_int !index))

  let var s = 
    Var.of_string s

  let top = 
    let var = new_var () in
    let env = Environment.make [|var;|] [||] in
    let abs = Abstract1.of_box man env [|var;|] [|Interval.top;|] in
    {var=var; abs=abs; env=env}
    
  let bot = 
    let var = new_var () in
    let env = Environment.make [|var;|] [||] in
    let abs = Abstract1.of_box man env [|var;|] [|Interval.bottom;|] in
    {var=var; abs=abs; env=env}

  let (<=) v1 v2 = 

    let abs_v1 = Abstract1.bound_variable man v1.abs v1.var in
    let abs_v2 = Abstract1.bound_variable man v2.abs v2.var in
    let _ = Calli.Pp.printf ~color:Blue "%a " Interval.print abs_v1 in
    let _ = Calli.Pp.printf ~color:Blue "<= " in
    let _ = Calli.Pp.printf ~color:Blue "%a %b \n@." Interval.print abs_v2 (Interval.is_leq abs_v1 abs_v2) in
    (* let abs = Abstract1.unify man v1.abs v2.abs in *)
    (* let env = Abstract1.env abs in *)
    (* let abs_v1 = Abstract1.change_environment man v1.abs env true in *)
    (* let abs_v2 = Abstract1.change_environment man v2.abs env true in *)
    Interval.is_leq abs_v1 abs_v2

  let join v1 v2 = 
    let abs = Abstract1.unify man v1.abs v2.abs in
    let env = Abstract1.env abs in
    let abs_v1 = Abstract1.change_environment man v1.abs env true in
    let abs_v2 = Abstract1.change_environment man v2.abs env true in
    let abs = Abstract1.join man abs_v1 abs_v2 in
    {var=v2.var; abs = abs; env=env}

  let meet v1 v2 =
    let abs = Abstract1.unify man v1.abs v2.abs in
    let env = Abstract1.env abs in
    let abs_v1 = Abstract1.change_environment man v1.abs env false in
    let abs_v2 = Abstract1.change_environment man v2.abs env false in
    let abs = Abstract1.meet man v1.abs v2.abs in
    {var=v1.var; abs = abs; env=env}

  let alpha z s = 
    let i = Z.to_int z in
    let var = var s in
    let env = Environment.make [|var;|] [||] in
    let abs = Abstract1.of_box man env [|var;|] [|Interval.of_int i i;|] in
    let _ = index := !index + 1 in
    {var=var; abs=abs; env=env}

  let pp fmt v = 
    Format.printf "%a -> %a" Var.print v.var Interval.print (Abstract1.bound_variable man v.abs v.var)
    (* Format.printf "var : %a, abs : %a@." Var.print v.var Abstract1.print v.abs *)

  let widen v1 v2 = 
    let _ = Calli.Pp.printf ~color:Blue "Widen\n@." in
    let abs = Abstract1.unify man v1.abs v2.abs in
    let env = Abstract1.env abs in
    let abs_v1 = Abstract1.change_environment man v1.abs env false in
    let abs_v2 = Abstract1.change_environment man v2.abs env false in
    let threshold = Lincons1.array_make env 5 in
    let abs = Abstract1.widening_threshold man v1.abs v2.abs threshold in
    {var=v1.var; abs = abs; env=env}


  module Op = struct

    let f v1 v2 ty s = 
      let var = var s in
      let abs = Abstract1.unify man v1.abs v2.abs in
      let env = Abstract1.env abs in
      let t1 = Texpr1.var env v1.var in
      let t2 = Texpr1.var env v2.var in
      let texpr = Texpr1.binop ty t1 t2 Texpr1.Int Texpr1.Rnd in
      let env = 
        if Environment.mem_var env var 
          then env
        else 
          Environment.add env [|var|] [||] 
      in
      let abs = Abstract1.change_environment man abs env false in
      (* let abs = Abstract1.bottom man env in *)
      let abs = Abstract1.assign_texpr man abs var texpr None in
      let res = {var=var; abs = abs; env=env} in 
      let _ = Calli.Pp.printf ~color:Red "%a -> %a\n@." 
        Var.print v1.var Interval.print (Abstract1.bound_variable man abs v1.var) in
      let _ = Calli.Pp.printf ~color:Red "%a -> %a\n@." 
        Var.print v2.var Interval.print (Abstract1.bound_variable man abs v2.var) in
      let _ = Calli.Pp.printf ~color:Red "%a -> %a\n@." 
        Var.print var Interval.print (Abstract1.bound_variable man abs var) in
      (* let _ = Calli.Pp.printf ~color:Red "%a -> %a\n@." 
        Var.print var Abstract1.print abs in *)
      res

    let (+) v1 v2 s = 
      f v1 v2 Texpr1.Add s

    let (-) v1 v2 s = 
      f v1 v2 Texpr1.Sub s

    let ( * ) v1 v2 s =
      f v1 v2 Texpr1.Mul s

    let (/) v1 v2 s = 
      f v1 v2 Texpr1.Div s
    
    let (%) v1 v2 s = 
      f v1 v2 Texpr1.Mod s


    let (==) n1 n2 = n1
        
    let (!=) n1 n2 = n1

    let (<) n1 n2 = n1

    let (<=) n1 n2 = n1

  end