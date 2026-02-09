// SPDX-License-Identifier: SEE LICENSE IN LICENSE
pragma solidity >= 0.8;

contract Test1 {
    uint x;

    constructor() {
        x = 0;
    }

    receive() external payable {
        x += 10;
    }

    function azzeraX() public {
        x = 0;
    }
}

contract D {
    uint y;
    constructor() { y = 1; }

    function tr(address payable des) public payable {
        des.transfer(1);
    }
}