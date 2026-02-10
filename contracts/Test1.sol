// SPDX-License-Identifier: MIT
pragma solidity >= 0.8;

contract C {
    uint x;

    // Di default il valore intero viene inizializzato a 0
    // constructor() {
    //     x = 0;
    // }

    receive() external payable {
        x += 10;
    }

    function azzeraX() public {
        x = 0;
    }
}

contract D {
    address cAdd;

    constructor(address payable cAddress_) {
        cAdd = cAddress_;
    }

    function trasferisciDenaro() public payable {
        payable(cAdd).transfer(1);
    }
}