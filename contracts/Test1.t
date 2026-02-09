// unit testing Test1.sol
faucet 0xA 100
faucet 0xB 100

deploy 0xA:0xC() "Test1" "contracts/Test1.sol"
assert 0xC x==0

deploy 0xB:0XD() "D" "contracts/Test1.sol"
assert 0xD y==1

0xB:0xD.tr{value : 1}("0xC")
assert 0xC x==10