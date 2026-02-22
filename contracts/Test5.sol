// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

contract C {
    int x;
    function f() public view returns (uint) { return(x>0); }
}