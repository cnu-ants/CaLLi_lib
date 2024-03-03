module F = Format
type elt = I of Z.t | MinInf | MaxInf
type t = IntInterval of {min:elt; max:elt} | IntBot

let bot :t = IntBot
let top :t = IntInterval {min=MinInf; max=MaxInf}

module Elt = struct

  let zero = 
    I (Z.of_int 0)

  let one = 
    I (Z.of_int 1)

  let minus_one = 
    I (Z.of_int (-1))

  let (<=) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, _ | _, MaxInf -> true
    | I i1, I i2 -> Z.leq i1 i2
    | _ -> false

  let (==) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MinInf | MaxInf, MaxInf -> true
    | I i1, I i2 -> Z.equal i1 i2
    | _ -> false
  
  let (+) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(+)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.add i1 i2)

  let (-) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(-)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.sub i1 i2)

  let ( * ) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(*)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.mul i1 i2)

  let (/) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(/)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.div i1 i2)
  
  let (%) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(%)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.rem i1 i2)
  
  let (&) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(and)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(logand) i1 i2)
  
  let (or) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(or)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(logor) i1 i2)

  let xor (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(xor)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(logxor) i1 i2)

  let (>>) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(>>)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(shift_right) i1 (Z.to_int i2))
  
  let (<<) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(<<)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(shift_left) i1 (Z.to_int i2))

  let min (elts : elt list) : elt = 
    List.fold_left
    (fun min e -> if e <= min then e else min)
    (List.hd elts)
    elts

  let max (elts : elt list) : elt = 
    List.fold_left
    (fun max e -> if max <= e then e else max)
    (List.hd elts)
    elts


  let next_pow (e : elt) : elt = 
    let res = ref zero in
    while !res <= e do 
      res := !res * I (Z.of_int 2)
    done;
    !res 

end

let alpha (n: Z.t) : t = 
    IntInterval {min=I n; max=I n}

(** Partial order *)
let (<=) (n1:t) (n2:t) : bool =
  match n1, n2 with
  | IntBot, _ -> true
  | _, IntBot -> false
  | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    if (Elt.(min1 <= min0) && Elt.(max0 <= max1)) then true 
    else false

let join n1 n2 = 
  match n1, n2 with
  | IntBot, _ -> n2
  | _, IntBot -> n1
  | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    IntInterval {min=Elt.min [min0; min1]; max=Elt.max [max0; max1]}

let meet n1 n2 = 
  match n1, n2 with
  | IntBot, _ | _, IntBot -> IntBot
  | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    IntInterval {min=Elt.max [min0; min1]; max=Elt.min [max0; max1]}


module BinOp = struct

  let (+) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      IntInterval {min=Elt.(min0+min1); max=Elt.(max0+max1)}

  let (-) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      IntInterval {min=Elt.(min0-min1); max=Elt.(max0-max1)} 

  let ( * ) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      let mul = [Elt.(min0*min1); Elt.(min0*max1); 
        Elt.(max0*min1); Elt.(max0*max1)] in
      IntInterval {min=Elt.min mul; max=Elt.max mul}

  let (/) n1 n2 : t =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if Elt.(min1 == Elt.zero) && Elt.(max1 == Elt.zero)
        then IntInterval {min=MinInf; max=MaxInf}
      else if Elt.(min1 == Elt.zero)
        then IntInterval {min=Elt.(min0 / max1); max=MaxInf}
      else if Elt.(max1 == Elt.zero)
        then IntInterval {min=MinInf; max=Elt.(/) max0 min1}
      else if (alpha (Z.zero)) <= n2 
        then IntInterval {min=MinInf; max=MaxInf}
      else 
        let div = [Elt.(min0/min1); Elt.(min0/max1); 
        Elt.(max0/min1); Elt.(max0/max1)] in
        IntInterval {min=Elt.min div; max=Elt.max div}

  let (%) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if Elt.(min1 == Elt.zero) && Elt.(max1 == Elt.zero)
        then IntInterval {min=MinInf; max=MaxInf}
      else if Elt.(min1 == Elt.zero)
        then IntInterval {min=Elt.(%) min0 max1; max=MaxInf}
      else if Elt.(max1 == Elt.zero)      
        then IntInterval {min=MinInf; max=Elt.(%) max0 min1}
      else if IntInterval {min=Elt.zero; max=Elt.zero} <= n2 
        then IntInterval {min=MinInf; max=MaxInf}
      else 
        let m = [Elt.(min0%min1); Elt.(min0%max1); 
        Elt.(max0%min1); Elt.(max0%max1)] in
        IntInterval {min=Elt.min m; max=Elt.max m}
  
  let (&) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if (alpha (Z.zero)) <= n1 || (alpha (Z.zero)) <= n2
        then alpha (Z.zero)
      else if (alpha (Z.minus_one)) <= n1
        then n2
      else if (alpha (Z.minus_one)) <= n2
        then n1
      else if Elt.(min0==max0) && Elt.(min1==max1)
        then IntInterval {min=Elt.(min0 & min1); max=Elt.(max0 & max1)}
      else if Elt.(Elt.zero < min0) && Elt.(Elt.zero < min1)
        then IntInterval {min=Elt.zero; max=Elt.min [max0; max1]}
      else top


  let (or) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if (alpha (Z.minus_one)) <= n1 || (alpha (Z.minus_one)) <= n2
        then alpha (Z.minus_one)
      else if (alpha (Z.zero)) <= n1
        then n2
      else if (alpha (Z.zero)) <= n2
        then n1
      else if Elt.(min0==max0) && Elt.(min1==max1)
        then IntInterval {min=Elt.(or) min0 min1; max=Elt.(or) max0 max1}
      else if Elt.(Elt.zero < min0) && Elt.(Elt.zero < min1) && max0 != MaxInf && max1 != MaxInf
        then IntInterval {min=Elt.zero; max=Elt.(Elt.next_pow(Elt.((Elt.max [max0; max1]) + Elt.one)) - Elt.one)}
      else top

  let xor n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if (alpha (Z.zero)) <= n1
        then n2
      else if (alpha (Z.zero)) <= n2
        then n1
      else if Elt.(min0==max0) && Elt.(min1==max1)
        then IntInterval {min=Elt.(xor) min0 min1; max=Elt.(xor) max0 max1}
      else if Elt.(Elt.zero < min0) && Elt.(Elt.zero < min1) && max0 != MaxInf && max1 != MaxInf
        then IntInterval {min=Elt.zero; max=Elt.(Elt.next_pow(Elt.((Elt.max [max0; max1]) + Elt.one)) - Elt.one)}
      else top
  

  let (<<) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if Elt.(max1 < Elt.zero)
        then IntBot
      else 
        n1 * IntInterval {min=Elt.(Elt.one << min1); max=Elt.(Elt.one << max1)}

  let (>>) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if Elt.(max1 < Elt.zero)
        then IntBot
      else 
        let m = [Elt.(min0 >> min1); Elt.(min0 >> max1); 
        Elt.(max0 >> min1); Elt.(max0 >> max1)] in
        IntInterval {min=Elt.min m; max=Elt.max m}

end

module CompOp = struct
  let (==) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> alpha (Z.zero)
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if (Elt.(min0 == max0) && Elt.(min1 == max1) && Elt.(min0 == min1)) 
        then alpha (Z.one)
      else if Elt.(min1 > max0) || Elt.(min0 > max1) 
        then alpha (Z.zero)
      else IntInterval {min=Elt.zero; max=Elt.one}

  let (!=) n1 n2 = 
    match (n1 == n2) with
    | IntInterval {min; max} ->
      if Elt.(==) min Elt.zero
        then alpha (Z.one)
      else alpha (Z.zero)
    | _ -> failwith "unreachable"

  let (<=) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> alpha (Z.zero)
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        if (Elt.(min0 == max0) && Elt.(min1 == max1) && Elt.(min0 == min1)) 
          then alpha (Z.one)
        else if Elt.(min1 > max0) || Elt.(min0 > max1) 
          then alpha (Z.zero)
        else IntInterval {min=Elt.zero; max=Elt.one}

  let (<) n1 n2 = 
    match n1, n2 with
    | IntBot, _ | _, IntBot -> alpha (Z.zero)
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        if (Elt.(min0 == max0) && Elt.(min1 == max1) && Elt.(min0 == min1)) 
          then alpha (Z.zero)
        else if Elt.(min1 > max0) || Elt.(min0 > max1) 
          then alpha (Z.one)
        else IntInterval {min=Elt.zero; max=Elt.one}

end


let pp_elt _ elt = 
    match elt with
    | I i -> F.printf "%s" (Z.to_string i)
    | MinInf -> F.printf "-∞"
    | MaxInf -> F.printf "+∞"

let pp fmt n =
    match n with
    | IntBot -> F.fprintf fmt "IntBot"
    | IntInterval {min; max} -> F.fprintf fmt "[%a, %a]" pp_elt min pp_elt max