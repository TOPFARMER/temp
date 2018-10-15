# Toy C++ Crypto

This repository contains all personal cryptographic implementations. For educational use only, the code is not cryptographically secure, nor has it been optimised efficiently.

* To run, download the repository and open the solution in MSVC.
* Refer to the documentation for relative primers.

## RSA
### Overview
Variable length RSA key generation and message encryption / decryption.

### Dependencies
Utilises a modified version of: https://github.com/panks/BigInteger (bundled)
  * bigint.h
  * bigint.cpp

### Notes
Heavily underoptimised - key generation is extremely slow. For increased speeds, use small key sizes w/ low primailty iterations.

## Future
* Asymmetric
  * ECC
* Symmetric
  * AES
