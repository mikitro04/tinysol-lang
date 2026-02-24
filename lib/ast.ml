(******************************************************************************)
(*                                     Contracts                              *)
(******************************************************************************)

(* variable/function/contract identifier *)
type ide = string

(* address identifier *)
type addr = string

(* Abstract syntax of expressions *)

type expr =
  (* COSTANTI: Quello che scrivi nel codice *)
  | BoolConst of bool
  | IntConst of int
  | AddrConst of addr

  (* VALORI RUNTIME: Questi non li scrivi nel codice .sol, ma servono al programma mentre gira per ricordarsi quanto vale un calcolo fatto *)
  | IntVal of int       (* runtime only: integer expressions *)             (* valore computato di un'espressione e.g.(3+(-5)) *)
  | UintVal of int      (* runtime only: unsigned integer expressions *)    (* valore computato di un'espressione e.g.(3+5) *)
  
  (* VARIABILI E AMBIENTE *)
  | BlockNum            (* es: block.number (a che blocco siamo?) *)
  | This                (* rappresenta il contratto attuale *)
  | Var of ide          (* stringa che rappresenta il nome della variabile *)
  | BalanceOf of expr   (* es Solidity: x.balance (quanti soldi ha x?) *) (* es OCaml: balance(x) *)

  | MapR of expr * expr (* Map Reader: tipo che richiede due valori tipo expr: NomeMappa * Chiave *) (* MapR(m, k) => m[k] *)
                        (* Più flessibile, computa expr fino a farlo diventare una stringa, cioè l'identificativo della variabile a cui accedere *)

  (* OPERAZIONI MATEMATICHE E LOGICHE *)
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr           (* equal == *)
  | Neq of expr * expr          (* not equal != *)
  | Leq of expr * expr          (* less equal <= *)
  | Lt of expr * expr           (* less then < *)
  | Geq of expr * expr          (* greater equal >= *)
  | Gt of expr * expr           (* greater then > *)
  | IfE of expr * expr * expr   (* If then Else: if(expr) then expr else expr *)
  
  (* Expression casting *)
  | IntCast of expr             (*  *)
  | UintCast of expr            (*  *)
  | AddrCast of expr            (*  *)
  | PayableCast of expr         (*  *)
  | EnumOpt of ide * ide        (*  *)
  | UnknownCast of ide * expr   (*  *)
  | EnumCast of ide * expr      (*  *)
  | ContractCast of ide * expr  (*  *)

  (* Function calls *)
  | FunCall of expr * ide * expr * expr list        (* IndirizzoMittente, NomeFunzione, ValoreInWei, ListaArgomenti *)
  | ExecFunCall of cmd  (* Runtime only: c is the cmd being reduced *)
    
(* Abstract syntax of commands *)
          
and cmd =
  | Skip
  | Assign of ide * expr                        (* Variable assignment *)
  | Decons of ide option list * expr            (* Deconstruction of multiple return values *)
  | MapW of ide * expr * expr                   (* Map assignment *)
  | Seq of cmd * cmd                            (* Sequencing *)
  | If of expr * cmd * cmd                      (* Conditional command *)
  | Send of expr * expr                         (* send(e1,e2) transfers e2 wei to e1 *)
  | Req of expr                                 (* require(e) reverts if e is false *) 
  | Block of local_var_decl list * cmd          (* block with declarations *)
  | ExecBlock of cmd                            (* Runtime only: c is the cmd being reduced *)
  | Decl of local_var_decl                      (* Static-time only: Decl is converted into block*)
  | ProcCall of expr * ide * expr * expr list   (* ProcCall(to_expr, func_name, value_expr, arg_exprs):
                                                    chiamata procedurale esterna -
                                                    (destinatario, nome funzione, valore in wei, argomenti).
                                                    Trasferisce valore, crea frame con `msg.sender`/`msg.value`
                                                    e avvia l'esecuzione della funzione. *)
  | ExecProcCall of cmd                         (* Runtime only: c is the cmd being reduced *)
  | Return of expr list

(* Base types *)

and base_type = 
  | IntBT             (* int *)
  | UintBT            (* uint *) 
  | BoolBT            (* bool *)
  | AddrBT of bool    (* address (the bool b in AddrBT(b) tells if the address is payable (b=1) or not (b=0) *)
  | UnknownBT of ide  (* unknown type: specialized by preprocess_contract in EnumBT of ContractBT *)
  | EnumBT of ide     (* enum *)
  | ContractBT of ide (* contract *)

(* Variable types, consisting of:
  - a base type, or
  - a mapping from a base type to another base type
*)

(* Exprval: values associated to (contract and local) variables *)

and var_type = VarT of base_type | MapT of base_type * base_type

and visibility_t = 
  | Public 
  | Private
  | Internal
  | External

and fun_mutability_t = 
  | Payable
  | NonPayable
  | View
  | Pure

and var_mutability_t = 
  | Constant
  | Immutable
  | Mutable

and exprval = 
  | Bool of bool 
  | Int of int
  | Uint of int
  | Addr of string
  | Map of (exprval -> exprval)

(* a variable declaration (t,x) consists of:
   - a type t
   - an identifier x of the variable
   - a variable visibility modifier (default is internal)
   - a variable mutability modifier (default is muitable)
 *)

(* Visibility modifiers *)

and var_decl = {                (* (IntBT/..) (x/IDE) (public/private/external) (view/pure) (Some(bool/int/..)/None)*) (* --> *) (*int g public view; / int g public view = ..; *)
  ty: var_type;                 (* Tipo della variabile *)
  name: ide;                    (* Identificativo della variabile *)
  visibility: visibility_t;     (* Visibilità della variabile *)
  mutability: var_mutability_t; (*  *)
  init_value: exprval option    
}

and local_var_decl = { ty: var_type; name: ide; }

(* Function declarations
  - the constructor is always public, and it can be payable
  - functions can be either public or private, and they can be payable
 *)

type fun_decl =
  | Constr of local_var_decl list * cmd * fun_mutability_t      (* Costruttore = (dichiarazioneDiVariabiliLocali, Comando, (payable/..)) *)
  | Proc of ide * local_var_decl list * cmd * visibility_t * fun_mutability_t * (base_type list) (* Procedura = come costruttore ma con nome identificativo e tipo di ritorno *)

type enum_decl = Enum of (ide * ide list)

(* Contracts consist of:
 - a name (ide)
 - a list of enum declarations 
 - a list of variable declarations (contract state variables)
 - a list of function declarations 
 *)
type contract = Contract of ide * enum_decl list * var_decl list * fun_decl list


(******************************************************************************)
(*                                   Transactions                             *)
(******************************************************************************)

(* Transactions contain:
 - txsender: the address of the transaction sender (either an EOA or a contract address)
 - txto: the address of the called contract
 - txfun: the name of the called function. In a deploy transaction, the txfun is "constructor" and the first argument is the contract code
 - txvars: the list of actual parameters
 - txvalue: the amount of wei transferred from the sender to the caller along with the transaction 
 *)

type transaction = {
  txsender : addr;          (* Mittente*)
  txto : addr;              (* Destinatario *)
  txfun : ide;              (* Nome della funzione chiamata *)
  txargs : exprval list;    (* Argomenti della funzione chiamata *)
  txvalue : int;            (* Valore in wei trasferito con la transazione *)
}
