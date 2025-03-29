# Classical cryptography in Haskell

## Introduction
This project implements popular cryptographic ciphers in **Haskell**. Currently the following ciphers are implemented: **Caesar Cipher**, **Vigenère Cipher**, and **One-Time Pad**.

The project also demonstrates possible attacks on those ciphers: **Many-Time Pad Attack**, **Frequency Analysis**, **Kasiski Examination**. The Many Time Pad Attack exploits the reuse of a one-time pad key across multiple ciphertexts.

## Features
- Key generation
- Message Encryption/Decryption with a key
- Decryption of multiple ciphertexts encrypted with the same one-time pad key using the Many-Time Pad attack
- Brute force attacl on Caesar Cipher ciphertexts with frequency analysis.
- Decryption of Vigenère ciphertexts using Kasiski examination and frequency analysis

## Usage
For running the program:
```sh
$ stack run
```

The program will start and ask user for further input.

## Tests
Tests are included in the test folder. To run the tests:
```sh
$ stack test
```

## How It Works
### Many Time Pad Attack
1. **XOR Analysis**: The program XORs different ciphertexts together to determine the locations of spaces in the corresponding plaintexts.
2. **Partial Key**: Using the space locations in the different ciphertexts, a partial key is constructed.

The folder ciphertexts contains text files with ciphertexts to be dectrypted. The "mtp.txt" file has the ciphertexts that are used to demonstrate the Many-Time Pad attack. They are in hex format, separated by a comma. The source of the challenge is: https://homepages.cwi.nl/~schaffne/courses/crypto/2012/.


### Caesar Cipher Attack
**Brute Force**: Tries all possible shifts (0-25), compares them using chi-squared statistic, and displays the option that is most likely to be the original plaintext.

