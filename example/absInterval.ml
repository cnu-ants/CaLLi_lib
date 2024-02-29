module F = Format
type elt = I of Z.t | MinInfInt | MaxInfInt
type t = 
    | IntTop (* = [MinIntInf, MaxInfInt] *)
    | IntInterval of {min:elt; max:elt}
    | IntBot

let bot :t = IntBot

let top :t = IntTop

module EltOp = 
    struct
    
    let (+) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | I i1, I i2 -> I (Z.(+) i1 i2)
    | MinInfInt , MaxInfInt -> failwith "elt + error"
    | MaxInfInt , MinInfInt -> failwith "elt + error"
    | MinInfInt, _ -> MinInfInt
    | _, MinInfInt -> MinInfInt
    | MaxInfInt, _ -> MaxInfInt
    | _, MaxInfInt -> MaxInfInt
    
    let (-) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInfInt , MaxInfInt -> failwith "elt - error"
    | MaxInfInt , MinInfInt -> failwith "elt - error"
    | I i1, I i2 -> I (Z.(-) i1 i2)
    | MinInfInt, _ -> MinInfInt
    | _, MinInfInt -> MinInfInt
    | MaxInfInt, _ -> MaxInfInt
    | _, MaxInfInt -> MaxInfInt
    
    let ( * ) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | I i1, I i2 -> I (Z.( * ) i1 i2)
    | MinInfInt , MaxInfInt -> failwith "elt * error"
    | MaxInfInt , MinInfInt -> failwith "elt * error"
    | MinInfInt, _ -> MinInfInt
    | _, MinInfInt -> MinInfInt
    | MaxInfInt, _ -> MaxInfInt
    | _, MaxInfInt -> MaxInfInt
    
    let (/) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInfInt , MaxInfInt -> failwith "elt / error"
    | MaxInfInt , MinInfInt -> failwith "elt / error"
    | I i1, I i2 -> I (Z.(/) i1 i2)
    | MinInfInt, _ -> MinInfInt
    | _, MinInfInt -> MinInfInt
    | MaxInfInt, _ -> MaxInfInt
    | _, MaxInfInt -> MaxInfInt
    
    let (<) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | I i1, I i2 -> Z.lt i1 i2
    | MinInfInt, MinInfInt -> false
    | _, MinInfInt -> false
    | MaxInfInt, MaxInfInt -> false
    | MaxInfInt, _ -> false
    | MinInfInt, _ -> true
    | _, MaxInfInt-> true
    
    let (<=) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | I i1, I i2 -> Z.leq i1 i2
    | MinInfInt, MinInfInt -> true
    | _, MinInfInt -> false
    | MaxInfInt, MaxInfInt -> true
    | MaxInfInt, _ -> false
    | MinInfInt, _ -> true
    | _, MaxInfInt-> true
    
    let (>) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | I i1, I i2 -> Z.gt i1 i2
    | MinInfInt, MinInfInt -> false
    | _, MinInfInt -> true
    | MaxInfInt, MaxInfInt -> false
    | MaxInfInt, _ -> true
    | MinInfInt, _ -> false
    | _, MaxInfInt-> false
    
    let (>=) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | I i1, I i2 -> Z.geq i1 i2
    | MinInfInt, MinInfInt -> true
    | _, MinInfInt -> true
    | MaxInfInt, MaxInfInt -> true
    | MaxInfInt, _ -> true
    | MinInfInt, _ -> false
    | _, MaxInfInt-> false
    
    let (=) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | I i1, I i2 -> Z.equal i1 i2
    | MinInfInt, MinInfInt -> true
    | MaxInfInt, MaxInfInt -> true
    | _, _ -> false
    
    let (!=) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | I i1, I i2 -> not (Z.equal i1 i2)
    | MinInfInt, MinInfInt -> false
    | MaxInfInt, MaxInfInt -> false
    | _, _ -> true
    end

(** Partial order *)
let (<=) n1 n2 =
    match n1, n2 with
    | _, IntTop | IntBot, _ -> true
    | IntTop, _ | _, IntBot -> false
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    if (EltOp.(min0 >= min1) && EltOp.(max0 <= max1)) then true else false

let join n1 n2 = 
    match n1, n2 with
    | IntTop, _ | _, IntTop -> IntTop
    | IntBot, _ -> n2
    | _, IntBot -> n1
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    if n1 <= n2 then n2 
    else if n2 <= n1 then n1
    else if EltOp.(min1 > max0) then IntInterval {min=min0; max=max1}
    else if EltOp.(min0 > max1) then IntInterval {min=min1; max=max0}
    else if EltOp.(min0 >= min1) then IntInterval {min=min1; max=max0}
    else IntInterval {min=min0; max=max1}

let meet n1 n2 = 
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntTop, _ -> n2
    | _, IntTop -> n1
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    if n1 <= n2 then n1
    else if n2 <= n1 then n2
    else if EltOp.(min1 > max0) then IntBot
    else if EltOp.(min0 > max1) then IntBot
    else if EltOp.(min0 >= min1) then IntInterval {min=min0; max=max1}
    else IntInterval {min=min1; max=max0}

let alpha (n) = 
    IntInterval {min=I n; max=I n}

module BinOp = struct
    let (+) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntTop, _ | _, IntTop -> IntTop
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        IntInterval {min=EltOp.(min0+min1); max=EltOp.(max0+max1)}
    let (-) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntTop, _ | _, IntTop -> IntTop
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        IntInterval {min=EltOp.(min0-min1); max=EltOp.(max0-max1)} 
    let ( * ) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntTop, _ | _, IntTop -> IntTop
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        IntInterval {min=EltOp.(min0*min1); max=EltOp.(max0*max1)}
    let (/) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntTop, _ | _, IntTop -> IntTop
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} -> 
        if (min1 = I (Z.of_int 0)) || (max1 = I (Z.of_int 0)) then IntBot else
        IntInterval {min=EltOp.(min0/max1); max=EltOp.(max0/min1)} 
end

module CompOp = struct
    let (==) n1 n2 =
    match n1, n2 with
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        if (EltOp.(min0 = max0) && EltOp.(min1 = max1) && EltOp.(min0 = min1)) 
        then IntInterval {min=I (Z.of_int 1); max=I (Z.of_int 1)}
        else if EltOp.(min1 > max0) then IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 0)}
        else if EltOp.(min0 > max1) then IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 0)}
        else IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 1)}
    | _ -> IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 1)}
    let (!=) n1 n2 = 
    match n1, n2 with
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        if (EltOp.(min0 = max0) && EltOp.(min1 = max1) && EltOp.(min0 = min1)) 
            then IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 0)}
        else if EltOp.(min1 > max0) 
            then IntInterval {min=I (Z.of_int 1); max=I (Z.of_int 1)}
        else if EltOp.(min0 > max1) 
            then IntInterval {min=I (Z.of_int 1); max=I (Z.of_int 1)}
        else IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 1)}
    | _ -> IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 1)}
    let (<=) n1 n2 =
    match n1, n2 with
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        if (EltOp.(min0 = max0) && EltOp.(min1 = max1) && EltOp.(min0 = min1)) 
        then IntInterval {min=I (Z.of_int 1); max=I (Z.of_int 1)}
        else if EltOp.(min1 > max0) then IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 0)}
        else if EltOp.(min0 > max1) then IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 0)}
        else IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 1)}
    | _ -> IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 1)}
    let (<) n1 n2 = 
    match n1, n2 with
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        if (EltOp.(min0 = max0) && EltOp.(min1 = max1) && EltOp.(min0 = min1)) then IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 0)}
        else if EltOp.(min1 > max0) then IntInterval {min=I (Z.of_int 1); max=I (Z.of_int 1)}
        else if EltOp.(min0 > max1) then IntInterval {min=I (Z.of_int 1); max=I (Z.of_int 1)}
        else IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 1)}
    | _ -> IntInterval {min=I (Z.of_int 0); max=I (Z.of_int 1)}
end
let pp_elt _ elt = 
    match elt with
    | I i -> F.printf "%s" (Z.to_string i)
    | MinInfInt -> F.printf "-∞"
    | MaxInfInt -> F.printf "+∞"

let pp fmt n =
    match n with
    | IntTop -> F.fprintf fmt "IntTop"
    | IntBot -> F.fprintf fmt "IntBot"
    | IntInterval {min; max} -> F.fprintf fmt "[%a, %a]" pp_elt min pp_elt max