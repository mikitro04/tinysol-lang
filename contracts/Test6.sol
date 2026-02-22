// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

contract C {
    int x;
    function f(int y) public { x = y; }
    function g() public { this.f(); }
}