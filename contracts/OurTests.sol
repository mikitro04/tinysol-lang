// SPDX-License-Identipier: MIT
pragma solidity >= 0.8.0;

contract C {
/* Dichiarazione Variabili */
    bool flag;
    C1 c1;
    int returnInt;
    bool returnBool;

    
/* RETURN */
    // Void
    // function r0() public { }                                                                         // MUST PASS
    // function r1() public returns() { }                                                               // MUST PASS
    // function r2() public returns() { this.r0(); }                                                    // MUST PASS   
    // function r3() public returns(int) { }

    // Return singoli
    // function r4() public returns(int) { return 1; }                                                  // MUST PASS
    // function r5() public returns(int) { return this.r0(); }
    // function r6() public returns() { return 1; }

    // Return multipli (Issue #12: failwith)
    // function r7()  public returns(int, int) { return (1, 1); }
    // function r8()  public returns(int, int) { return 1; }
    // function r9()  public returns(int) { return (1, 1); }
    // function r10() public returns(int, int) { }
    // function r11() public returns() { return (1, 1); }

    // TypeError
    // function r12() public returns(uint) { return -1; }
    // function r13() public returns(int) { return false; }
    // function r14() public returns(address) { return true * true; }
    // function r15() public returns(int) { return 1 && true; }

    // Gestione blocchi
    // function r16() public returns(int) { if (flag) { return 1; } else { return 2; } }                // MUST PASS
    // function r17() public returns(int) { if (flag) { return 1; } }
    // function r18() public returns(int) { if (flag) { return 1; } else { skip; } }


/* PROCCALL */
    // Funzioni target per i test
    function targetProc() public { }
    function targetProcArgs(int a, bool b) public { }


    // TypeError Wei
    // function p0() public { this.targetProc{value: 1}(); }                                            // MUST PASS
    // function p1() public { this.targetProc{value: -1}(); }
    // function p2() public { this.targetProc{value: true}(); }
    
    // Chiamata a funzione inesistente + Validità args
    // function p3() public { this.missing(true*true, 2+5-8*3); }
    // function p4() public { this.missing(true, 7 && false); }

    // Controlli Arità
    // function p5() public { this.targetProc(); }                                                      // MUST PASS
    // function p6() public { this.targetProc(1); }
    // function p7() public { this.targetProcArgs(1, true); }                                           // MUST PASS
    // function p8() public { this.targetProcArgs(1, true, 3); }

    // Type Mismatch
    // function p9()  public { this.targetProcArgs(true, true); }
    // function p10() public { this.targetProcArgs(1, 2); }
    // function p11() public { this.targetProcArgs("0xC", 1); }

    // Errori Espressione Chiamante
    // function p12() public { "0xC".targetProcArgs(); }
    // function p13() public { c1.targetProcArgs(); }
    // function p14() public { c.targetProcArgs(); }
    // function p15() public { flag.targetProcArgs(); }
    // function p16() public { 1.targetProcArgs(); }
    

    // Combinazioni di errori concatenati
    // function p17() public { flag.targetProcArgs{value: false}(1*false); }
    // function p18() public { c1.targetProcArgs{value: false}(1 && 2); }
    // function p19() public { c.targetProcArgs{value: -1}(true-false); }
    // function p20() public { "0xC".targetProcArgs{value: -10}(1, true*true); }
    

/* FUNCALL */
    // Funzioni target per i test FunCall
    function targetFun() public returns (int) { return 1; }
    function targetFunArgs(int a, bool b) public returns (bool) { return b; }

    // Variabili target per provare i TypeMismatch nell'assegnamento
    

    // TypeError Wei
    // function f0() public { returnInt = this.targetFun{value: 1}(); }                                         // MUST PASS
    // function f1() public { returnInt = this.targetFun{value: -1}(); }
    // function f2() public { returnInt = this.targetFun{value: true}(); }

    // Chiamata a funzione inesistente + Validità args
    // function f3() public { returnInt = this.missing(true*true, 2+5-8*3); }
    // function f4() public { returnInt = this.missing(true, 7 && false); }

    // Controlli Arità
    // function f5() public { returnBool = this.targetFunArgs(1, true); }                                       // MUST PASS
    // function f6() public { returnInt = this.targetFun(1); }
    // function f7() public { returnBool = this.targetFunArgs(1); }
    // function f8() public { returnBool = this.targetFunArgs(1, true, 3); }

    // Type Mismatch Argomenti (errori su passaggio parametri, bloccano il check dell'Assign)
    // function f9()  public { returnBool = this.targetFunArgs(false, true); }
    // function f10() public { returnBool = this.targetFunArgs(1, 2); }
    // function f11() public { returnBool = this.targetFunArgs("0xC", 1); }

    // Type Mismatch Ritorno (Valid Params ma Assegnamento ad una var di tipo errato)
    // function f12() public { returnBool = this.targetFun(); }
    // function f13() public { returnInt = this.targetFunArgs(1, true); }
    // function f14() public { returnInt = this.targetProc(); }

    // Errori Espressione Chiamante + NotInThisContract
    // function f15() public { returnInt = "0xC".targetFun(); }
    // function f16() public { returnBool = c1.targetFunArgs(1, true); }
    // function f17() public { returnBool = c.targetFunArgs(1, true); }
    // function f18() public { returnBool = flag.targetFunArgs(1, true); }
    // function f19() public { returnInt = 1.targetFun(); }

    // Combinazioni di errori concatenati
    // function f20() public { returnBool = flag.targetFunArgs{value: false}(1*false); }
    // function f21() public { returnBool = c1.targetFunArgs{value: "0xD"}(1 && 2); }
    // function f22() public { returnBool = c.targetFunArgs{value: -1}(true-false); }
    // function f23() public { returnInt = "0xC".targetFun{value: -10}(1); }

    /*  */
}