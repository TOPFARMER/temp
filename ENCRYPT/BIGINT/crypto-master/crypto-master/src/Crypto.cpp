// Crypto.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "RSA.h"
#include <iostream>


int main()
{
	RSA rsa_;
	rsa_.createKeyPair();
	printf("Press enter to exit...\n");
	cin.get();

    return 0;
}

