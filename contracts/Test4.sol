// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.2;

contract C {
    address d;

    constructor(address payable add_) { d = add_; }

    receive() external payable { payable(d).transfer(25); }
}

contract D {
    address c;
    int x;

    constructor(address payable add_) payable { c = add_; }

    function foo() public { payable(c).transfer(25); }

    receive() external payable {
        // gestione prima di overflow del loop
        if(x < 70) {        // Cap di iterazioni = n70, oltre 70 va in OverFlow/Revert
            x += 1;
            payable(c).transfer(25);
        } else {
            x = x;
        }
        
    }
}