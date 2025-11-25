open Ast

(* local environment: 
    maps local variables, plus sender and value
    this is a transient storage, not preserved between calls
*)
type env = ide -> exprval

(* contract state: persistent contract storage, preserved between calls *)
type account_state = {
  balance : int;
  storage : ide -> exprval;
  code : contract option;
}

type sysstate = {
  accounts: addr -> account_state;
  stackenv: env list;
  blocknum: int;
  active: addr list; (* set of all active addresses (for debugging)*)
}

(* execution state *)
type exec_state = 
  | St of sysstate 
  | Cmd of cmd * sysstate * addr
  | Reverted

let rec last_sysstate = function
    [] -> failwith "last on empty list"
  | [St st] -> st
  | _::l -> last_sysstate l


(* Functions to access and manipulate the state *)

let topenv (st: sysstate) : env = match st.stackenv with
  [] -> failwith "empty stack"
| e::_ -> e

let popenv (st: sysstate) : sysstate = match st.stackenv with
    [] -> failwith "empty stack"
  | _::el -> { st with stackenv = el } 

(* initial (empty) environment *)
let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
    
let bind x v f = fun y -> if y=x then v else f y

(* lookup for variable x in state st (tries first in storage of address a) *)
    
let lookup_var (a : addr) (x : ide) (st : sysstate) : exprval =
  try 
    (* look up for x in environment *)
    let e = topenv st in
    e x
  with _ -> 
    (* look up for x in storage of a *)
    let cs = st.accounts a in
    cs.storage x

let lookup_balance (a : addr) (st : sysstate) : int =
  try (st.accounts a).balance
  with _ -> 0

let exists_account (st : sysstate) (a : addr) : bool =
  try let _ = st.accounts a in true
  with _ -> false

let exists_ide_in_storage (cs : account_state) (x : ide) : bool = 
  try let _ = cs.storage x in true
  with _ -> false

let update_var (st : sysstate) (a:addr) (x:ide) (v:exprval) : sysstate = 
  let cs = st.accounts a in
    if exists_ide_in_storage cs x then 
      let cs' = { cs with storage = bind x v cs.storage } in 
      { st with accounts = bind a cs' st.accounts }
    else failwith (x ^ " not bound in storage of " ^ a)   

let update_map (st : sysstate) (a:addr) (x:ide) (k:exprval) (v:exprval) : sysstate = 
  let cs = st.accounts a in
    if exists_ide_in_storage cs x then 
      match cs.storage x with
      | Map m ->
        let m' = bind k v m in
        let cs' = { cs with storage = bind x (Map m') cs.storage } in 
        { st with accounts = bind a cs' st.accounts }
      | _ -> failwith ("update_map: " ^ x ^ " is not a mapping")
      else failwith ("mapping " ^ x ^ " not bound in storage of " ^ a)   

let update_env (st : sysstate) (x:ide) (v:exprval) : sysstate =
  let el = st.stackenv in match el with
  | [] -> failwith "Empty stack"
  | e::el' -> 
    try let _ = e x in (* checks if ide x is bound in e *)
      let e' = bind x v e in 
      { st with stackenv = e'::el' }
    with _ -> failwith (x ^ " not bound in env")