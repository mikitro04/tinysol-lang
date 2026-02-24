//SPDX-License-Identifier: UNLICENSED
pragma solidity <= 0.8;

contract C3 {

    /* NON TOCCARE */
    uint x;
    C c;

    // Prof tests NON TOCCARE!!!                                                    // (fallisce in Return(_) / fallisce in typecheck return)
    function f0() public { }                                                        // DEVE PASSARE
    // function f1() public { x = c.g(); }                                          // FunCall
    // function f2() public { x = c.g{value:1+2}(1,true); }                         // FunCall
    // function f3() public { x = c.g{value:x}(1+1); }                              // FunCall
    // function f4() public { return x; }                                           // fail: c'è return in ogni branch ma non returns (fallisce in Return(_))
    // function f5() public { if (1+1>1) return x; else return 2; }                 // fail: c'è return in ogni branch ma non returns, (fallisce in Return(_))
    // function f5() public { if (1+1>1) { return x; return 1; } else return 2; }   // fail: c'è return in ogni branch ma non returns, (fallisce in Return(_))
    // function f6() public { return c.g(); }                                       // fail: c'è return in ogni branch ma non returns
    // function f7() public { c.g{value:1}(); }                                     // Proc()
    // function f8() public { if (c.g()) c.g{value:1}(); else c.g(); }              // FunCall(?)
    // function f9() public returns (uint) { return 1; }                            // DEVE PASSARE
    // function f10() public returns(int) { }                                       // fail: 
    // function f11() public returns(address) { skip; }                             // fail: 
    // function f12() public returns(address payable) { skip; }                     // fail: 
    /* NON TOCCARE */

    // VARIABILI NOSTRE
    // bool y = true;
    // address a = "ciao";
    
    // function f() public returns(uint) { int y; return x; }
    // function g() public { skip; }
    // function h() public returns(int) { return 1; }

    // NOSTRI TEST

    // function f0() public returns(int) { return f.g(); }
    // function f1() public { x = c.g(); }         // exception Failure("TOD0: FunCall")
    // function f1v2() public { c.g(); }           // exception Failure("TOD0: ProcCall")
    // function f2() public { x = c.g{value:1+2}(1,true); }   
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
}