open Ast
open Utils
open Prettyprint

(* Types for expressions are a refinement of variable declaration types, 
 * since we want to give more specific types to integer constants in order
 * to have a smoother treatment of int and uint types 
 *)
type exprtype = 
  | BoolConstET of bool
  | BoolET
  | IntConstET of int
  | IntET
  | UintET
  | AddrET of bool
  | EnumET of ide
  | ContractET of ide
  | MapET of exprtype * exprtype

let rec string_of_exprtype = function
  | BoolConstET b   -> "bool " ^ (if b then "true" else "false")
  | BoolET          -> "bool"
  | IntConstET n    -> "int " ^ string_of_int n
  | IntET           -> "int"
  | UintET          -> "uint"
  | AddrET p        -> "address" ^ (if p then " payable" else "")
  | EnumET x        -> x
  | ContractET x    -> x
  | MapET(t1,t2)    -> string_of_exprtype t1 ^ " => " ^ string_of_exprtype t2

(* the result of the contract typechecker is either:
  - Ok():       if all static checks passed
  - Error log:  if some checks did not pass; the log collects all the errors found 
 *)
type typecheck_result = (unit,exn list) result

(* >> merges two contract typechecker results *)
let (>>)  (out1 : typecheck_result) (out2 : typecheck_result) : typecheck_result =
  match out1 with
  | Ok () -> out2
  | Error log1 -> match out2 with 
    | Ok () -> Error log1 
    | Error log2 -> Error (log1 @ log2)

(* the result of the expression typechecker is either:
  - Ok(t):      if all static checks passed, and t is the inferred type of the expression
  - Error log:  if some checks did not pass; the log collects all the errors found 
*)

type typecheck_expr_result = (exprtype,exn list) result

(* >>+ merges two expression typechecker results *)
let (>>+)  (out1 : typecheck_expr_result) (out2 : typecheck_expr_result) : typecheck_expr_result =
  match out1,out2 with
  | Ok _, Ok _ -> assert(false) (* should not happen *)
  | Ok _, Error log2 -> Error log2
  | Error log1, Ok _ -> Error log1 
  | Error log1,Error log2 -> Error (log1 @ log2)

(* boring cast from expression typechecker result to contract typechecker result*)
let typecheck_result_from_expr_result (out : typecheck_expr_result) : typecheck_result =
  match out with
  | Error log -> Error log
  | Ok(_) -> Ok()

(* The following exceptions represent the all possible errors detected by the typechecker *)
exception TypeError of ide * expr * exprtype * exprtype
exception NotMapError of ide * expr
exception ImmutabilityError of ide * ide
exception UndeclaredVar of ide * ide
exception MultipleDecl of ide
exception MultipleLocalDecl of ide * ide
exception EnumNameNotFound of ide * ide
exception EnumOptionNotFound of ide * ide * ide
exception EnumDupName of ide
exception EnumDupOption of ide * ide
exception MapInLocalDecl of ide * ide

(* Our exceptions *)
exception MissingReturnsDecl of ide
exception MissingReturnStat of ide
exception FuncNotFound of ide * ide
exception ArityArgsProblem of ide * ide * int * int
exception ProcCallException of ide * expr * exprtype * exprtype * exprtype
exception CannotBeVoid of ide * ide

let logfun f s = "(" ^ f ^ ")\t" ^ s 

(* Prettyprinting of typechecker errors *)
let string_of_typecheck_error = function
| TypeError (f,e,t1,t2) -> 
    logfun f
    "expression " ^ (string_of_expr e) ^ 
    " has type " ^ string_of_exprtype t1 ^
    " but is expected to have type " ^ string_of_exprtype t2
| NotMapError (f,e) -> logfun f (string_of_expr e) ^ " is not a mapping"
| ImmutabilityError (f,x) -> logfun f "variable " ^ x ^ " was declared as immutable, but is used as mutable"
| UndeclaredVar (f,x) -> logfun f "variable " ^ x ^ " is not declared"
| MultipleDecl x -> "variable " ^ x ^ " is declared multiple times"
| MultipleLocalDecl (f,x) -> logfun f "variable " ^ x ^ " is declared multiple times"
| EnumNameNotFound (f,x) -> logfun f "enum " ^ x ^ " is not declared"
| EnumOptionNotFound (f,x,o) -> logfun f "enum option " ^ o ^ " is not found in enum " ^ x
| EnumDupName x -> "enum " ^ x ^ " is declared multiple times"
| EnumDupOption (x,o) -> "enum option " ^ o ^ " is declared multiple times in enum " ^ x
| MapInLocalDecl (f,x) -> logfun f "mapping " ^ x ^ " not admitted in local declaration"

(* Prettyprinting of our typechecker errors *)
| MissingReturnsDecl f -> logfun f "function declared with 'return' statement but empty or no 'returns' keyword found"
| MissingReturnStat f -> logfun f "function have not declared 'return' statement but there is a 'returns' keyword found"
| FuncNotFound (f, g) -> logfun f "function '" ^ g ^ "' is not declared"
| ArityArgsProblem (f, g, n1, n2) -> logfun f "function '" ^ g ^ "' expects " ^ string_of_int n1 ^ " arguments but " ^ string_of_int n2 ^ " were provided"
| ProcCallException (f,v,t1,t2,t3) -> 
  logfun f
  "variable " ^ (string_of_expr v) ^ 
  " has type " ^ string_of_exprtype t1 ^
  " but is expected to have type " ^ string_of_exprtype t2 ^
  " or " ^ string_of_exprtype t3
| CannotBeVoid (f, g) -> logfun f "function '" ^ g ^ "' is declared void"

| ex -> Printexc.to_string ex

let exprtype_of_decltype = function
  | IntBT         -> IntET
  | UintBT        -> UintET
  | BoolBT        -> BoolET
  | AddrBT(b)     -> AddrET(b)
  | EnumBT _      -> UintET
  | ContractBT x  -> ContractET x 
  | UnknownBT _   -> assert(false) (* should not happen after preprocessing *)

(* typechecker functions take as input the list of variable declarations:
  - var_decl:       state variables 
  - local_var_decl: local variables
  The type all_var_decls encapsulates the list of these variables.
*)

type all_var_decls = (var_decl list) * (local_var_decl list)

let get_state_var_decls (avdl : all_var_decls) : var_decl list = fst avdl 
let get_local_var_decls (avdl : all_var_decls) : local_var_decl list = snd avdl 

(* merges a list of state variable decls and a list of local variable decls *)
let merge_var_decls (vdl : var_decl list) (lvdl : local_var_decl list) : all_var_decls = vdl , lvdl  

(* adds a list of local variables to all_var_decls *)
let push_local_decls ((vdl: var_decl list),(old_lvdl : local_var_decl list)) new_lvdl = 
  (vdl , new_lvdl @ old_lvdl)  

let lookup_type (x : ide) (avdl : all_var_decls) : exprtype option =
  if x="msg.sender" then Some (AddrET false)
  else if x="msg.value" then Some UintET else
  (* first lookup the local variables *)
  avdl 
  |> get_local_var_decls 
  |> List.map (fun (vd : local_var_decl) -> match vd.ty with
    | VarT(t)   -> (exprtype_of_decltype t),vd.name 
    | MapT(tk,tv) -> MapET(exprtype_of_decltype tk, exprtype_of_decltype tv),vd.name)
  |> List.fold_left
  (fun acc (t,y) -> if acc=None && x=y then Some t else acc)
  None
  |>
  fun res -> match res with
    | Some t -> Some t
    | None -> (* if not found, lookup the state variables *)
      avdl 
      |> get_state_var_decls  
      |> List.map (fun (vd : var_decl) -> match vd.ty with
        | VarT(t)   -> (exprtype_of_decltype t),vd.name 
        | MapT(tk,tv) -> MapET(exprtype_of_decltype tk, exprtype_of_decltype tv),vd.name)
      |> List.fold_left
      (fun acc (t,y) -> if acc=None && x=y then Some t else acc)
      None

let rec dup = function 
  | [] -> None
  | x::l -> if List.mem x l then Some x else dup l

(* no_dup_var_decls:
    checks that no variables are declared multiple times
 *)
let no_dup_var_decls vdl = 
  vdl 
  |> List.map (fun (vd : var_decl) -> vd.name) 
  |> dup
  |> fun res -> match res with None -> Ok () | Some x -> Error ([MultipleDecl x])  

let no_dup_local_var_decls f vdl = 
  vdl 
  |> List.map (fun (vd : local_var_decl) -> vd.name) 
  |> dup
  |> fun res -> match res with None -> Ok () | Some x -> Error ([MultipleLocalDecl (f,x)])  

let no_dup_fun_decls vdl = 
  vdl 
  |> List.map (fun fd -> match fd with 
    | Constr(_) -> "constructor"
    | Proc(f,_,_,_,_,_) -> f) 
  |> dup
  |> fun res -> match res with None -> Ok () | Some x -> Error ([MultipleDecl x])  

let subtype t0 t1 = match t1 with (* converto t0 in t1 *)
  | BoolConstET _ -> (match t0 with BoolConstET _ -> true | _ -> false) 
  | BoolET -> (match t0 with BoolConstET _ | BoolET -> true | _ -> false) 
  | IntConstET _ -> (match t0 with IntConstET _ -> true | _ -> false)
  | UintET -> (match t0 with IntConstET n when n>=0 -> true | UintET -> true | _ -> false)
  | IntET -> (match t0 with IntConstET _ | IntET -> true | _ -> false) (* uint is not convertible to int *)
  | AddrET _ -> (match t0 with AddrET _ -> true | _ -> false)
  | _ -> t0 = t1


let rec find_fun_in_decl_list (fdl : fun_decl list) (f : ide) : (local_var_decl list * base_type list) option =
    match fdl with
    | h :: t -> (
        match h with
        | Proc(name, vdl, _, _, _, ret) when name = f -> Some (vdl, ret)
        | _ -> find_fun_in_decl_list t f
        )
    | _ -> None
;;

let rec typecheck_expr (f : ide) (edl : enum_decl list) vdl (fdl : fun_decl list) = function
  | BoolConst b -> Ok (BoolConstET b)

  | IntConst n -> Ok (IntConstET n)

  | IntVal _ | UintVal _ -> assert(false) (* these expressions only occur at runtime *)

  | AddrConst _ -> Ok (AddrET false)

  | BlockNum -> Ok(UintET)

  | This -> Ok(AddrET false) (* TODO: check coherence with Solidity *)

  | Var x -> (match lookup_type x vdl with
    | Some t -> Ok(t)
    | None -> Error [UndeclaredVar (f,x)])

  | MapR(e1,e2) -> (match (typecheck_expr f edl vdl fdl e1, typecheck_expr f edl vdl fdl e2) with
    | Ok(MapET(t1k,t1v)),Ok(t2) when t2 = t1k -> Ok(t1v) 
    | Ok(MapET(t1k,_)),Ok(t2) -> Error [TypeError (f,e2,t2,t1k)]
    | _ -> Error [NotMapError(f,e1)]
    )

  | BalanceOf(e) -> (match typecheck_expr f edl vdl fdl e with
        Ok(AddrET(_)) -> Ok(UintET)
      | Ok(t) -> Error [TypeError (f,e,t,AddrET(false))]
      | _ as err -> err)

  | Not(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(BoolConstET b) -> Ok(BoolConstET (not b))
      | Ok(BoolET) -> Ok(BoolET)
      | Ok(t) -> Error [TypeError (f,e,t,BoolET)]
      | _ as err -> err)

  | And(e1,e2) -> 
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(BoolConstET false),Ok(t2) when subtype t2 BoolET -> Ok(BoolConstET false)
     | Ok(t1),Ok(BoolConstET false) when subtype t1 BoolET -> Ok(BoolConstET false)
     | Ok(t1),Ok(t2) when subtype t1 BoolET && subtype t2 BoolET -> Ok(BoolET)
     | Ok(t1),_ when not (subtype t1 BoolET) -> Error [TypeError (f,e1,t1,BoolET)]
     | _,Ok(t) -> Error [TypeError (f,e2,t,BoolET)]
     | err1,err2 -> err1 >>+ err2)

  | Or(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(BoolConstET true),Ok(t2) when subtype t2 BoolET -> Ok(BoolConstET true)
     | Ok(t1),Ok(BoolConstET true) when subtype t1 BoolET -> Ok(BoolConstET true)
     | Ok(t1),Ok(t2) when subtype t1 BoolET && subtype t2 BoolET -> Ok(BoolET)
     | Ok(t1),_ when not (subtype t1 BoolET) -> Error [TypeError (f,e1,t1,BoolET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,BoolET)]
     | err1,err2 -> err1 >>+ err2)

  | Add(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(IntConstET (n1+n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Sub(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(IntConstET (n1-n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Mul(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(IntConstET (n1*n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Div(_) -> failwith "Div: TODO"

  | Eq(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 = n2))
     | Ok(t1),Ok(t2) when t1=t2-> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 t2 && subtype t2 t1 -> Ok(BoolET) (* AddrET _ *)
     | Ok(t1),Ok(t2) -> Error [TypeError (f,e2,t2,t1)]
     | err1,err2 -> err1 >>+ err2)

  | Neq(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 <> n2))
     | Ok(t1),Ok(t2) when t1=t2-> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 t2 && subtype t2 t1 -> Ok(BoolET) (* AddrET _ *)
     | Ok(t1),Ok(t2) -> Error [TypeError (f,e2,t2,t1)]
     | err1,err2 -> err1 >>+ err2)

  | Leq(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 <= n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Lt(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 < n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)
    
  | Geq(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 >= n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Gt(e1,e2) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 > n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | IfE(e1,e2,e3) ->
    (match (typecheck_expr f edl vdl fdl e1,typecheck_expr f edl vdl fdl e2, typecheck_expr f edl vdl fdl e3) with
     | Ok(BoolConstET true),Ok(t2),_ -> Ok(t2)
     | Ok(BoolConstET false),_,Ok(t3) -> Ok(t3)
     | Ok(BoolET),Ok(t2),Ok(t3) when subtype t2 t3 -> Ok(t3)
     | Ok(BoolET),Ok(t2),Ok(t3) when subtype t3 t2 -> Ok(t2)
     | Ok(BoolET),Ok(t2),Ok(t3) -> Error [TypeError (f,e3,t3,t2)]
     | Ok(t1),_,_ -> Error [TypeError (f,e1,t1,BoolET)]
     | err1,err2,err3 -> err1 >>+ err2 >>+ err3)

  | IntCast(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(IntConstET _) | Ok(IntET) | Ok(UintET) -> Ok(IntET)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | UintCast(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(IntConstET n) when n>=0 -> Ok(IntConstET n) 
      | Ok(IntET) | Ok(UintET) -> Ok(UintET)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | AddrCast(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(AddrET(b))     -> Ok(AddrET b)
      | Ok(IntConstET _)  -> Ok(AddrET false) 
      | Ok(UintET)        -> Ok(AddrET false)
      | Ok(IntET)         -> Ok(AddrET false)
      | Ok(t)             -> Error [TypeError (f,e,t,IntET)] 
      | err               -> err)

  | PayableCast(e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(AddrET _)      -> Ok(AddrET true)
      | Ok(IntConstET 0)  -> Ok(AddrET false)
      | Ok(t)             -> Error [TypeError (f,e,t,IntET)]
      | err               -> err)

  | EnumOpt(enum_name,option_name) -> 
      edl
      |> List.filter (fun (Enum(y,_)) -> y=enum_name)
      |> fun edl -> (match edl with [Enum(_,ol)] -> Some ol | _ -> None)  
      |> fun l_opt -> (match l_opt with 
        | None -> Error [EnumNameNotFound (f,enum_name)]
        | Some ol -> (match find_index (fun o -> o=option_name) ol with
          None -> Error [EnumOptionNotFound(f,enum_name,option_name)]
          | Some i -> Ok(IntConstET i)))

  | EnumCast(x,e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(IntConstET _) | Ok(UintET) | Ok(IntET) -> Ok(EnumET x)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | ContractCast(x,e) -> (match typecheck_expr f edl vdl fdl e with
      | Ok(AddrET _) -> Ok(ContractET x)
      | Ok(t) -> Error [TypeError (f,e,t,AddrET(false))]
      | err -> err)

  | UnknownCast(_) -> assert(false) (* should not happen after preprocessing *)

  (* FunCall of expr * ide * expr * expr list => IndirizzoMittente, NomeFunzione, ValoreInWei, ListaArgomenti *)
  | FunCall(e_to, g, e_val, e_args) -> (
    
    let castTMP (a : typecheck_result) : typecheck_expr_result = 
      match a with
      | Error t -> Error t
      | Ok() -> failwith "should not happen : castTMP"
    in
  
    let accErr = castTMP(typecheck_proc e_to g e_val e_args f edl vdl fdl) in
    if e_to <> This then
      accErr
    else (
      match find_fun_in_decl_list fdl g with
      | None -> accErr
      | Some(_, ret) -> (
        match ret with
        | [] -> Error [CannotBeVoid(f, g)]
        | [single_return] -> Ok(exprtype_of_decltype single_return)
        | _ -> failwith "Issue #12: function declared with multiple return values"
      )
    )
  )
    
    
        (* e_to è o un indirizzo/contratto o This
        match e_to with
        | This -> failwith "poba, ti coddo con una gamba sola, la mia minca ti bastona"
        | _ -> Error [FuncNotFound (f, g)]
      ) in Ok(IntET)
    ) *)

    (* let ret = (
      match find_fun_in_decl_list fdl f with
      | Some(_, r) -> r
      | _ -> []
    )
    in 
    if ret = [] && e_to <> This then
      failwith "la funzione non esiste in questo contratto"
    else
      let returnET = (
        match ret with
        | [] -> failwith "void"
        | [single_return] -> exprtype_of_decltype single_return
        | _ -> failwith "Issue #12: function declared with multiple return values"
      )
      in (
        match typecheck_proc e_to g e_val e_args f edl vdl fdl with
        | Error err -> Error err
        | Ok() -> Ok(returnET)
      ) *)

  | ExecFunCall(_) -> assert(false) (* this should not happen at static time *)

  
and typecheck_proc (e_to : expr) (g : ide) (e_val : expr) (e_args : expr list) (f : ide) (edl : enum_decl list) (vdl : all_var_decls) (fdl : fun_decl list) : (typecheck_result) = 
  ( (* Controllo wei *)
    match typecheck_expr f edl vdl fdl e_val with
    | Error err -> Error err
    | Ok(weiT) -> (
      if (not (subtype weiT UintET)) then
        Error [TypeError(f, e_val, weiT, UintET)]
      else
        Ok()
    )
  (* Controllo della validità degli argomenti singoli, dell'arità e del type match *)
  ) >> (
    let args_res = List.map (fun expr -> (expr, typecheck_expr f edl vdl fdl expr)) e_args in
    let args_errors = List.fold_left (fun acc (_, res) -> 
      match res with 
      | Error log -> acc >> Error log 
      | Ok _ -> acc
    ) (Ok()) args_res in

    args_errors >> (
      match e_to with
      | This -> (
        match find_fun_in_decl_list fdl g with
        | None -> Error [FuncNotFound (f, g)]
        | Some (formal_args, _) -> (
          if List.length e_args <> List.length formal_args then
            Error [ArityArgsProblem (f, g, List.length formal_args, List.length e_args)]
          else (
            List.fold_left2 (fun acc (expr, expr_res) arg ->
              match expr_res with
              | Error(_) -> acc
              | Ok(exprET) ->
                let argET = (
                  match arg.ty with
                    | VarT t -> exprtype_of_decltype t
                    | MapT _ -> failwith "should not happen: map formal arg not supported") in
                if subtype exprET argET then
                  acc
                else
                  acc >> Error [TypeError(f, expr, exprET, argET)]
            ) (Ok()) args_res formal_args
          )
        )
      )
    
      | t -> (
        match typecheck_expr f edl vdl fdl t with
        | Ok(AddrET _) | Ok(ContractET _) -> Error [FuncNotFound (f, g)]
        | Ok(v) -> Error [ProcCallException(f, t, v, (AddrET false), (ContractET "contract")); FuncNotFound (f, g)]
        | Error err -> Error err
      )
    )
  )
;;



let is_immutable (x : ide) (vdl : var_decl list) = 
  List.fold_left (fun acc (vd : var_decl) -> acc || (vd.name=x && vd.mutability<>Mutable)) false vdl

let typecheck_local_decls (f : ide) (vdl : local_var_decl list) = List.fold_left
  (fun acc vd -> match vd.ty with 
    | MapT(_) -> acc >> Error [MapInLocalDecl (f,vd.name)]
    | _ -> acc)
  (Ok ())
  vdl

(* f: Identificatore della funzione / edl: lista delle dichiarazioni delle enum / vdl lista delle dichiarazioni delle variabili *)
let rec typecheck_cmd (f : ide) (edl : enum_decl list) (vdl : all_var_decls) (fdl : fun_decl list) = function 
    | Skip -> Ok()
    
    | Assign(x,e) -> 
        (* the immutable modifier is not checked for the constructor *)
        if f <> "constructor" && is_immutable x (get_state_var_decls vdl) then Error [ImmutabilityError (f,x)]
        else (
          match typecheck_expr f edl vdl fdl e,typecheck_expr f edl vdl fdl (Var x) with
          | Ok(te),Ok(tx) -> if subtype te tx then Ok() else Error [TypeError (f,e,te,tx)]
          | res1,res2 -> typecheck_result_from_expr_result (res1 >>+ res2)
        )

    | Decons(_) -> failwith "TODO: multiple return values"  (* Issue: 12 *)

    | MapW(x,ek,ev) -> 
        (match typecheck_expr f edl vdl fdl (Var x),
               typecheck_expr f edl vdl fdl ek,
               typecheck_expr f edl vdl fdl ev with
          | Ok(tx),Ok(tk),Ok(tv) -> (match tx with
              | MapET(txk,_) when not (subtype tk txk) -> Error [TypeError (f,ek,tk,txk)] 
              | MapET(_,txv) when not (subtype tv txv) -> Error [TypeError (f,ev,tv,txv)] 
              | MapET(_,_) -> Ok()
              | _ -> Error [NotMapError (f,Var x)])
          | res1,res2,res3 -> typecheck_result_from_expr_result (res1 >>+ res2 >>+ res3))

    | Seq(c1,c2) -> 
        typecheck_cmd f edl vdl fdl c1
        >>
        typecheck_cmd f edl vdl fdl c2

    | If(e,c1,c2) -> (match typecheck_expr f edl vdl fdl e with
          | Ok(BoolConstET true)  -> typecheck_cmd f edl vdl fdl c1
          | Ok(BoolConstET false) -> typecheck_cmd f edl vdl fdl c2
          | Ok(BoolET) -> 
              typecheck_cmd f edl vdl fdl c1
              >>
              typecheck_cmd f edl vdl fdl c2
          | Ok(te) -> Error [TypeError (f,e,te,BoolET)]
          | res -> typecheck_result_from_expr_result res)

    | Send(ercv,eamt) -> (match typecheck_expr f edl vdl fdl ercv with
          | Ok(AddrET(true)) -> Ok() (* can only send to payable addresses *)
          | Ok(t_ercv) -> Error [TypeError(f,ercv,t_ercv,AddrET(true))]
          | res -> typecheck_result_from_expr_result res) 
          >>
          (match typecheck_expr f edl vdl fdl eamt with
          | Ok(t_eamt) when subtype t_eamt UintET -> Ok()
          | Ok(t_eamt) -> Error [TypeError(f,eamt,t_eamt,UintET)]
          | res -> typecheck_result_from_expr_result res)

    | Req(e) -> (match typecheck_expr f edl vdl fdl e with
          | Ok(BoolET) -> Ok() 
          | Ok(te) -> Error [TypeError (f,e,te,BoolET)]
          | res -> typecheck_result_from_expr_result res)

    | Block(lvdl,c) ->
        typecheck_local_decls f lvdl
        >>
        let vdl' = push_local_decls vdl lvdl in
        typecheck_cmd f edl vdl' fdl c

    | ExecBlock(_) -> assert(false) (* should not happen at static time *)

    | Decl(_) -> assert(false) (* should not happen after blockify *)

    (* (destinatario, nome funzione chiamata, valore in wei, argomenti alla chiamata) *)
    | ProcCall(e_to, g, e_val, e_args) -> typecheck_proc e_to g e_val e_args f edl vdl fdl
    
    (* let rec useInfixOp (a : typecheck_expr_result list) =
    match a with
    | [] -> Ok()
    | h::t -> (
      match h with
      | Ok(_) -> useInfixOp t
      | Error(l) -> Error(l) >> (useInfixOp t)
    )
    ;; *)
    
    
      (*| ProcCall(e_to, g, e_val, e_args) -> let gay = "gay" in failwith gay(
      let accErr = Ok() in (
      match typecheck_expr f edl vdl e_val with
      | Error err -> accErr >> Error err
      | Ok(weiT) -> accErr >> (
        if (not (subtype weiT UintET)) then
          Error [TypeError(f, e_val, weiT, UintET)]
        else
          Ok()
        )
      ) >> (
        (* Controlliamo il proprietario della funzione *)
        match e_to with
        | This -> (
          (* Controlliamo che la firma della funzione esista all'interno di questo contratto *)
          match find_fun_in_decl_list fdl g with
          | None -> Error [FuncNotFound (f, g)]
          | Some (formal_args, _) -> (
            (* Controlliamo l'arità *)
            if List.length e_args <> List.length formal_args then
              Error [ArityArgsProblem (f, g, List.length formal_args, List.length e_args)]
            else (
              (* Convertiamo i parametri passati e i parametri formali della funzione in ExpressionType *)                
              List.fold_left2 (fun acc expr arg ->
                let expr_res = typecheck_expr f edl vdl expr in
                match expr_res with
                | Error log -> acc >> Error log
                | Ok exprET ->
                  let argET = (
                    match arg.ty with
                      | VarT t -> exprtype_of_decltype t
                      | MapT _ -> failwith "should not happen") in
                  if subtype exprET argET then
                    acc
                  else
                    acc >> Error [TypeError(f, expr, exprET, argET)]
              ) (accErr) e_args formal_args
            )
          )
        )
        | t -> Error [MissingReturnsDecl("oOoOoOoOoOo")] >> (
          (
            match typecheck_expr f edl vdl t with
            | Ok(AddrET _) | Ok(ContractET _) -> Error [MissingReturnsDecl("bagassa 3 euro")]
            | _ -> failwith "Pyrex Baby negro di merda! ok"
          ) 

        )
      )
    )
    *)
    (*
    let argsET = e_args 
      |> List.map (typecheck_expr f edl vdl) (* Ok() | Error() (>>)  *)
      (* |> List.map (fun x -> match x with | Ok(bt) -> bt | _ -> failwith "should not happen") *)
    in
    let formal_argsET = formal_args 
      |> List.map (fun x -> x.ty) 
      |> List.map (fun x -> match x with | VarT b -> b | _ -> failwith "passing a map (should not happen)")
      |> List.map (exprtype_of_decltype) in (* argsET deve essere compatibile con formal_arg_list *)

    let errors = useInfixOp argsET in
    match errors with
    | Error(_) -> errors
    | _ ->
      if (List.for_all2 subtype (argsET |> List.map (fun x -> match x with | Ok(bt) -> bt | _ -> failwith "should not happen")) formal_argsET) then
        Ok()
      else 
        failwith "pouh"
        (* Error([TypeError (f, e)]) Type mismatch *)
    (* failwith ("THEN, formal_args: " ^ string_of_int (List.length formal_args) ^ ", arg_list: " ^ (string_of_int (List.length (arg_list)))) *)
  *)
  

    | ExecProcCall(_) -> assert(false) (* should not happen at static time *)

    | Return(el) ->
      let ret = match find_fun_in_decl_list fdl f with
        | Some(_, r) -> r
        | _ -> []
      in
      match ret with
        | [] -> Error [MissingReturnsDecl f]
        | [single_returns] -> let returnsET = (exprtype_of_decltype single_returns) in (
          match el with
            | [] -> failwith "should not happen: Function declared with 'returns' but no 'return' statement found"
            | [expr_return] -> (
                  match (typecheck_expr f edl vdl fdl expr_return) with
                  | Ok(et) ->
                    if (subtype et returnsET) then 
                      Ok() 
                    else 
                      Error [TypeError (f, expr_return, et, returnsET)]
                  | Error errL -> Error errL
                )
            | _ -> failwith "Issue #12: functions returning multiple values"
          )
        | _ -> failwith "Issue #12: function declared with multiple return values"



let typecheck_return c ret f = 
  let rec exists_return (c : cmd) : bool =
    match c with
    | Return (_) -> true
    | Seq (c1, c2) -> exists_return c1 || exists_return c2
    | If (_, c1, c2) -> exists_return c1 && exists_return c2
    | Block (_, c1) -> exists_return c1
    | _ -> false
  in
  let check = exists_return c in
  if (ret <> [] && not check) then        (*(ret <> [] && check || ret = [] && not check)*)
    Error [MissingReturnStat (f)]
  else
    Ok()
;;


let typecheck_fun (edl : enum_decl list) (vdl : var_decl list) (fdl : fun_decl list) = function
  | Constr (al,c,_) ->
      no_dup_local_var_decls "constructor" al
      >>
      typecheck_local_decls "constructor" al
      >> 
      typecheck_cmd "constructor" edl (merge_var_decls vdl al) fdl c
  | Proc (f,al,c,_,__,ret) ->
      no_dup_local_var_decls f al
      >> 
      typecheck_local_decls f al
      >>
      typecheck_return c ret f
      >>
      typecheck_cmd f edl (merge_var_decls vdl al) fdl c

(* dup_first: finds the first duplicate in a list *)
let rec dup_first (l : 'a list) : 'a option = match l with 
  | [] -> None
  | h::tl -> if List.mem h tl then Some h else dup_first tl

let typecheck_enums (edl : enum_decl list) = 
  match dup_first (List.map (fun (Enum(x,_)) -> x) edl) with
  | Some x -> Error [EnumDupName x] (* there are two enums with the same name *)
  | None -> List.fold_left (fun acc (Enum(x,ol)) -> 
      match dup_first ol with 
      | Some o -> acc >> (Error [EnumDupOption (x,o)])
      | None -> acc
    )
    (Ok ()) 
    edl

(* typecheck_contract : contract -> (unit,string) result 
    Perform several static checks on a given contract. The result is:
    - Ok () if all checks succeed 
    - Error log otherwise, where log explains the reasons of the failed checks     
    *)

let rm_dup_err (err : typecheck_result) : typecheck_result =
  let rec rm_rec = function
    | [] -> []
    | h::t ->
      if List.mem h t then
        rm_rec t
      else
        h::(rm_rec t)
  in
  match err with
  | Error l -> Error(rm_rec l)
  | Ok() -> Ok()
;;

let typecheck_contract (Contract(_,edl,vdl,fdl)) : typecheck_result =
  (* no multiply declared enums *)
  typecheck_enums edl 
  >>
  (* no multiply declared state variables *)
  no_dup_var_decls vdl
  >>
  (* no multiply declared functions *)
  no_dup_fun_decls fdl
  >>
  List.fold_left (fun acc fd -> acc >> typecheck_fun edl vdl fdl fd) (Ok ()) fdl
  (* |>
  rm_dup_err  *)

let string_of_typecheck_result = function
  Ok() -> "Typecheck ok"
| Error log -> List.fold_left 
  (fun acc ex -> acc ^ (if acc="" then "" else "\n") ^ string_of_typecheck_error ex) "" log
