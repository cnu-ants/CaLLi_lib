module F = Format
module AbsInt = AbsOctagon
module AbsAddr = AbsAddr

type elt = IntLiteral of AbsInt.elt | AddrLiteral of AbsAddr.elt
type t = | AbsTop | AbsAddr of  AbsAddr.t | AbsInt of AbsInt.t | AbsBot

let bot = AbsBot
let top = AbsTop

let pp fmt v = 
  match v with
  | AbsTop -> F.fprintf fmt "AbsTop"
  | AbsBot -> F.fprintf fmt "AbsBot"
  | AbsInt (v) -> F.fprintf fmt "AbsInt : %a" AbsInt.pp v 
  | AbsAddr v -> F.fprintf fmt "%a" AbsAddr.pp v 

let (<=) v1 v2 = 
  match v1, v2 with
  | _, AbsTop | AbsBot, _ -> true
  | AbsTop, _ | _, AbsBot -> false
  | AbsAddr a1 , AbsAddr a2 -> AbsAddr.(a1 <= a2)
  | AbsInt n1, AbsInt n2 -> AbsInt.(n1 <= n2) 
  | _ -> failwith "<= error"

let join v1 v2 = 
    match v1, v2 with
    | AbsTop, _ | _, AbsTop -> AbsTop
    | AbsBot, _ -> v2
    | _, AbsBot -> v1
    | AbsAddr a1 , AbsAddr a2 -> AbsAddr (AbsAddr.join a1 a2)
    | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.join n1 n2)
    | _ -> failwith "join error"

let meet v1 v2 = 
  match v1, v2 with
  | AbsBot, _ | _, AbsBot -> AbsBot
  | AbsTop, _ -> v2
  | _, AbsTop -> v1
  | AbsAddr a1 , AbsAddr a2 -> AbsAddr (AbsAddr.meet a1 a2)
  | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.meet n1 n2)
  | _ -> failwith "meet error"

let alpha_int (n:AbsInt.elt ) s = AbsInt (AbsInt.alpha n s)

let alpha_addr (a : AbsAddr.elt) = AbsAddr (AbsAddr.alpha a)

let alpha literal s = 
  match literal with
  | IntLiteral n -> alpha_int n s
  | AddrLiteral a -> alpha_addr a

let widen v1 v2 = 
  if (v1 <= v2) && v1 <> v2
    then 
      AbsInt (AbsInt.top)
    else join v1 v2

let binop (op : Calli.Op.t) n1 n2 s =  
  match n1, n2 with
  | AbsInt v1, AbsInt v2 -> 
    (match op with
    | Add -> AbsInt (AbsInt.Op.(v1 + v2) s)
    | Sub -> AbsInt (AbsInt.Op.(v1 - v2) s)
    | Mul -> AbsInt (AbsInt.Op.(v1 * v2) s)
    (* | SDiv -> 
    | SRem -> 
    | Shl -> 
    | LShr -> 
    | Ashr -> 
    | And -> 
    | Or -> 
    | Xor ->  *)
    | _ -> AbsBot
    )
  | AbsBot, _ | _, AbsBot -> AbsBot
  | AbsTop, _ | _, AbsTop -> AbsTop
  | _ -> AbsBot

    
let compop (op : Calli.Cond.t) n1 n2 str = 
  match n1, n2 with
  | AbsInt v1, AbsInt v2 -> 
    (match op with
    | Eq -> AbsInt (AbsInt.Op.(v1 == v2))
    | Ne -> AbsInt (AbsInt.Op.(v1 != v2))
    | Sgt -> AbsInt (AbsInt.Op.(v1 < v2))
    | Sge -> AbsInt (AbsInt.Op.(v1 <= v2))
    | _ -> AbsBot
    )
  | AbsBot, _ | _, AbsBot -> AbsBot
  | AbsTop, _ | _, AbsTop -> AbsTop
  | _ -> AbsBot