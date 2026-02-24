// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

contract C {
    int x;
    function f() public returns() { skip; return 1; }
    //function g() public { this.f(); }       // => f = Proc(_,_,_,[])
    //function g() public { bool b; b = this.f(); }
}