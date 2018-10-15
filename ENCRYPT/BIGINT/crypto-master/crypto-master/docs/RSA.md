# Programmatically Computing RSA
## Steps

1. Compute 2 primes (p & q):
  1. Generate large random number.
  2. Apply primality test.
  3. Return probably prime.
2. Compute (n = p * q).
3. Compute (q(n) = (p - 1) * (q - 1))
4. Choose e -> (1 < e < q(n)) - e & n are coprime.
5. Compute a value for d such that ((d * e) % q(n) = 1).
6. Public key == (e, n).
7. Private key == (d, n).

The encryption of m = 2 is c = 27 % 33 = 29
The decryption of c = 29 is m = 293 % 33 = 2
s
Euler's Totient Function and Euler's Theorem
Fermat test
Miller - Rabin
Primality test
Greatest common divisor
Chinese remainder theorem
Euler's totient function
Mersenne Twister

http://crypto.stackexchange.com/questions/71/how-can-i-generate-large-prime-numbers-for-rsa
http://www.di-mgt.com.au/rsa_alg.html
