// unit testing Test2.sol
faucet 0xA 200
faucet 0xB 100

deploy 0xA:0xD{value:100}() "D" "contracts/Test2.sol"
deploy 0xA:0xC("0xD") "C" "contracts/Test2.sol"

assert 0xA.balance==100
assert 0xB.balance==100
assert 0xC.balance==0
assert 0xD.balance==100


