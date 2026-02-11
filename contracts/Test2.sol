// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

contract C { 
    address d;
    constructor(address payable add_) { d = add_; }
    function foo(address add_) public { payable(add_).transfer(10); }
    receive() external payable {
        // transfer a D => x = 1 += 1 = 2
        d.g();
        // payable(d).transfer(10);
    }
}

contract D { 
    uint x;
    constructor() payable { }
    function f(address a_) public { payable(a_).transfer(100); }
    function g() public { x = 1; }
    receive() external payable { x += 1; }
}