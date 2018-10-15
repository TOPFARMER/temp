
#include "stdafx.h"
#include "RSA.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include <math.h>
#include <ctime>
#include <sstream>
#include <ctime>
#include <cstdlib>
#include "bigint.h"
#include <iostream> 
#include <iomanip>

#define MODULUS_SIZE 32					// bits
#define KEY_SIZE (MODULUS_SIZE / 8)		// bytes

using std::ostringstream;

void RSA::createKeyPair() {

	// Get p & q
	// -------------------------------------------------------

	srand(time(NULL));
	cout << "Computing P..." << endl;
	BigInteger p = genPrime();
	std::cout << "P: " << p.getNumber() << "\n";
	cout << "Computing Q..." << endl;
	BigInteger q = genPrime();
	std::cout << "Q: " << q.getNumber() << "\n";

	// -------------------------------------------------------

	BigInteger n = p * q;
	std::cout << "N: " << n.getNumber() << "\n";

	// -------------------------------------------------------

	BigInteger qn = (p - 1) * (q - 1);
	std::cout << "QN: " << qn.getNumber() << "\n";

	// -------------------------------------------------------

	BigInteger e = 997;
	if (RSA::gcd(e, qn) == true) {
		std::cout << "PHI: " << e.getNumber() << "\n";
	}

	// -------------------------------------------------------

	BigInteger d = 0;
	BigInteger temp = 0;
	while (d == 0) {
		++temp;
		if (((temp * e) % qn) == 1) {
			d = temp;
		}
	}
	std::cout << "D: " << d.getNumber() << "\n";

	// -------------------------------------------------------

	std::cout << "Public Key = (" << e.getNumber() << ", " << n.getNumber() << ")" << "\n";
	std::cout << "Private Key = (" << d.getNumber() << ", " << n.getNumber() << ")" << "\n";

	//keypair kp = { e, d, n };
	//int c = encrypt(2, e, n);
	//decrypt(c, d, n);
}

int RSA::encrypt(int m, int e, int n) {
	std::cout << "M: " << m << "\n";
	int c1 = pow(m, e);
	int c = c1 % n;
	std::cout << "C: " << c << "\n";
	return c;
}

void RSA::decrypt(int c, int d, int n) {
	int m1 = pow(c, d);
	int m2 = m1 % n;
	std::cout << "M: " << m2 << "\n";
}

BigInteger RSA::genPrime() {
	unsigned char random_bytes[KEY_SIZE];
	for (size_t i = 0; i < KEY_SIZE; i++)
		random_bytes[i] = rand() % 0xFF;

	// Top two bits to one so number is relatively large.
	random_bytes[0] |= 0xC0;
	// Bottom bit to 1 to ensure number is odd.
	random_bytes[KEY_SIZE - 1] |= 0x01;

	// Byte array to hex.
	std::stringstream ss;
	ss << std::hex << std::setfill('0');
	for (int i = 0; i < KEY_SIZE; ++i)
	{
		ss << std::setw(2) << static_cast<unsigned>(random_bytes[i]);
	}

	BigInteger p; p.toDecimal(ss.str());

	while (true) {		
		if (RSA::fermat(p.getNumber(), 5) == true)
			return p.getNumber();
		else
			p += 2;
	}
}

bool RSA::fermat(BigInteger n, int k) {
	if (n % 2 == 0) {
		return false;
	}

	// a ^ n-1 = 1 (mod n)
	for (int i = 0; i < k; ++i) {
		int a = rand() % (1000 - 2) + 2;
		if (modulo(a, n - 1, n) != 1) {
			return false;
		}
	}
	return true;
}

BigInteger RSA::modulo(int base, BigInteger exponent, BigInteger mod) {
	// (base ^ exponent) / mod
	BigInteger x = 1, y = base;
	while (exponent > 0) {
		if (exponent % 2 == 1) {
			x = (x * y) % mod;
		}
		y = (y * y) % mod;
		exponent /= 2;
	}
	return x % mod;
}

bool RSA::gcd(BigInteger n1, BigInteger n2) {
	BigInteger n3 = n1;
	BigInteger n4 = n2;
	while (n3 != n4) {
		if (n3 > n4)
			n3 -= n4;
		else
			n4 -= n3;
	}
	if (n3 == 1) {
		return true;
	}
	else {
		return false;
	}
}

