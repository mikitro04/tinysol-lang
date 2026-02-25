//SPDX-License-Identifier: UNLICENSED
pragma solidity <= 0.8;


contract C3 {

    /* NON TOCCARE */
    int x;

    int pisello;
    C c;
    merda m;
    address pistacchio;

    mapping (address => uint) mapDecl;
    

    // Prof tests NON TOCCARE!!!                                                    // (fallisce in Return(_) / fallisce in typecheck return)
    // function f0() public { }                                                     // DEVE PASSARE
    // function f1() public { x = c.g(); }                                          // FunCall
    // function f2() public { x = c.g{value:1+2}(1,true); }                         // FunCall
    // function f3() public { x = c.g{value:x}(1+1); }                              // FunCall
    // function f4() public { return x; }                                           // fail: c'è return in ogni branch ma non returns, (fallisce in Return(_))
    // function f5() public { if (1+1>1) return x; else return 2; }                 // fail: c'è return in ogni branch ma non returns, (fallisce in Return(_))
    // function f6() public { if (1+1>1) { return x; return 1; } else return 2; }   // fail: c'è return in ogni branch ma non returns, (fallisce in Return(_))
    // function f7() public { return c.g(); }                                       // fail: c'è return in ogni branch ma non returns, (fallisce in Return(_))
    // function f8() public { c.g{value:1}(); }                                     // Proc(): 
    // function f9() public { if (c.g()) c.g{value:1}(); else c.g(); }              // FunCall(?)
    // function f10() public returns(uint) { return 1; }                            // DEVE PASSARE
    // function f11() public returns(int) { }                                       // fail: c'è returns ma non return, fallisce in typecheck return
    // function f12() public returns(address) { skip; }                             // fail: c'è returns ma non return, fallisce in typecheck return
    // function f13() public returns(address payable) { skip; }                     // fail: c'è returns ma non return, fallisce in typecheck return
    /* NON TOCCARE */

    // VARIABILI NOSTRE
    // bool y = true;okkkkkkkkkkkk
    // address a = "ciao";
    // mapping (address => uint) mapAAAAAAA;
    // function f() public returns(uint) { int y; return x; }
    // function g(int b) public { skip; }
    // function h() public returns(int) { return 1; }

    // NOSTRI TEST

    // function f0() public returns(int) { return f.g(); }
    // function f1() public { x = c.g(); }         // exception Failure("TODO: FunCall")
    // function f1v2() public { this.g(mapAAAAAAA); }           // exception Failure("TODO: ProcCall")
    // function f2() public { c.g{value:2-4}(1,true); }   
    // function f3() public { x = c.g{value:x}(1+1); }   
    // function f4() public { return x; }   
    // function f5() public { if (1+1>1) return x; else return 2; }   
    // function f52() public returns(uint) { if (y) return a; else return x; }   
    // function f53() public returns(bool) { if (1+1>1) { return x; return 1; } else return 2; }   
    // function f6() public returns() { skip; }      
    // function f7() public { c.g{value:1}(); }
    // function f8() public { if (c.g()) c.g{value:1}(); else c.g(); }
    // function f9() public returns (uint) { return 1; }   
    // function f10() public returns(int) { }
    // function f11() public returns(address) { skip; }
    // function f12() public returns(address payable) { skip; }

    function target(uint a, bool b) public {
        skip;
    }

    // function test_shadowing() public {
    //     // BUG 1: Shadowing.
    //     // L'errore viene riportato in (target) invece che in (test_shadowing).
    //     this.target(true, true, true);
    // }


    // function test_short_circuit() public {
    //     // BUG 3: Short-circuiting.
    //     // L'errore sul 'value' blocca il controllo degli argomenti.
    //     // Vedremo solo l'errore su 'true' (value), ma non quello su '20' (che dovrebbe essere bool).
    //     this.target{value: y}(10, 20);
    // }
    
    function test_external_args_ignored() public {
        // BUG 2: Argomenti esterni ignorati.
        // Poiché non è 'This', il typechecker non controlla l'arità (3 invece di 2)
        // né il tipo degli argomenti. Questo passa senza errori!
        "0xC".target{value: false}(false*false, 1);
    }
}