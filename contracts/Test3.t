// unit testing Test3.sol
faucet 0xA 200    // creo il contratto A con 200wei
faucet 0xB 100    // creo il contratto B con 100wei

deploy 0xA:0xD{value:100}("0xC") "D" "contracts/Test3.sol" // A deploya D pagando 100wei
assert 0xD this.balance==100                          // controllo che D abbia 0+100wei
assert 0xA this.balance==100                          // controllo che A abbia 200-100wei
deploy 0xA:0xC("0xD") "C" "contracts/Test3.sol"       // A deploya C GRATIS

0xA:0xD.foo()
assert 0xA this.balance==100
assert 0xB this.balance==100
assert 0xC this.balance==0
assert 0xD this.balance==100
assert 0xD x==0
