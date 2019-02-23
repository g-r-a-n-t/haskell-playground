![](./header.png)

# Elliptic Curves

Implementation of modular elliptic curve arithmetic and cryptographic functions.

## Supported operations
### ModularArithmetic
- `pointAdd (2,3,97) (3,6) (12,3) --> (39,6)`
- `pointScale (2,3,97) 347 (3,6) --> (80,10)`

### ECDSA
- `sign ...`
- `verify ...`


## Running Tests
```bash
$ ghci modular-arithmetic
> Gec.ModularArithmetic.runTests

$ ghci ecdsa types modular-arithmetic
> Gec.ECDSA.runTests
```
