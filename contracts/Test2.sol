// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract C { 
    D d;
    constructor(D add) { d = add; }
    function foo(address add) public payable { payable(add).transfer(10); }
    receive() external payable { d.g(); }
}

contract D { 
    uint x;
    constructor() payable { } 
    function f(address a) public payable { payable(a).transfer(100); }
    function g() public { x = 1; }
    receive() external payable { x += 1; }
}