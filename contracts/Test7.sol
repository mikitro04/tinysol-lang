// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

contract C {
    uint x;
    function f() public pure returns(int) { return(1); }
    function g() public { bool b; b = this.f(); }
}