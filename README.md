# Classical cryptography in Haskell

## Introduction
This project implements popular cryptographic ciphers in **Haskell**. Currently two ciphers are implemented: **Caesar Cipher** and **One-Time Pad**.

The project also demonstrates possible attacks on those ciphers: **Many-Time Pad Attack** and **Frequency Analysis**. The Many Time Pad Attack exploits the reuse of a one-time pad key across multiple ciphertexts, while the Frequency Analysis can decrypt messages encrypted using a simple shift-based encryption like Caesar Cipher.

## Features
- Key generation
- Message Encryption/Decryption
- Decryption of multiple ciphertexts encrypted with the same one-time pad key using the Many-Time Pad attack
- Frequency analysis for breaking mono-alphabetic substitutions such as the Caesar Cipher
- Implementation of **Caesar Cipher Attack** using brute force

## Usage
For running a demonstration of the **Many-Time Pad attack** (ciphertexts in /ciphertexts/mtp.txt):
```sh
$ stack run
```
For running the **Caesar Cipher attack**:
```sh
$ stack run
```
This will attempt brute-force decryption of a message encrypted with the Caesar cipher.

## How It Works
### Many Time Pad Attack
1. **XOR Analysis**: The program XORs different ciphertexts together to determine the locations of spaces in the corresponding plaintexts.
2. **Partial Key**: Using the space locations in the different ciphertexts, a partial key is constructed.
3. **Interactive Guessing (probably coming)**: The user can provide manual inputs to refine decryption results.

The folder ciphertexts contains text files with ciphertexts to be dectrypted. The "mtp.txt" file has the ciphertexts that are used to demonstrate the Many-Time Pad attack. They are in hex format, separated by a comma. The source of the challenge is: https://homepages.cwi.nl/~schaffne/courses/crypto/2012/.


### Caesar Cipher Attack
1. **Brute Force** (WIP): Tries all possible shifts (0-25) and displays possible plaintexts
2. **Frequency Analysis**: Uses letter frequency statistics to guess the most likely shift
3. **User Interaction** (WIP): Allows users to refine results manually

