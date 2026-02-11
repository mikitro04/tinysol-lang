// unit testing Test2.sol
faucet 0xA 200    // creo il contratto A con 200wei
faucet 0xB 100    // creo il contratto B con 100wei

deploy 0xA:0xD{value:100}() "D" "contracts/Test2.sol" // A deploya D pagando 100wei
assert 0xD this.balance==100                          // controllo che D abbia 0+100wei
assert 0xA this.balance==100                          // controllo che A abbia 200-100wei
deploy 0xA:0xC("0xD") "C" "contracts/Test2.sol"       // A deploya C GRATIS

assert 0xB this.balance==100    // check di B.balance = inizio = 100
assert 0xC this.balance==0      // check di C.balance = inizio = 0
assert 0xD this.balance==100    // check di D.balance = inizio = 100

// A usa f di D e paga 100 a C
0xA:0xD.f("0xC")              // D paga C 100wei


assert 0xC this.balance==100
assert 0xD this.balance==0


0xB:0xC.foo("0xA")


assert 0xA this.balance==110
assert 0xB this.balance==100
assert 0xC this.balance==90
assert 0xD this.balance==0
assert 0xD x==1