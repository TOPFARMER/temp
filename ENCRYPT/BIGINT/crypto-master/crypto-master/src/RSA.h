#include "bigint.h"

class RSA {
public:

	/**
	* Create RSA public / private key pair,
	* return struct.
	*/
	void createKeyPair();

	int encrypt(int m, int e, int n);

	void decrypt(int c, int d, int n);

	BigInteger genPrime();

	bool fermat(BigInteger n, int k);

	/*
	*	Modular Exponentiation
	*/
	BigInteger modulo(int b, BigInteger e, BigInteger m);

	bool gcd(BigInteger n1, BigInteger n2);

private:
	struct keypair {
		BigInteger e; // (e, n)
		BigInteger d; // (d, n)
		BigInteger n;
	};
};