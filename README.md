# Classical cryptography in Haskell

## Introduction
This project demonstrates a **Many-Time Pad Attack** and a **Caesar Cipher Attack** implemented in **Haskell**. The Many Time Pad Attack exploits the reuse of a one-time pad key across multiple ciphertexts, while the Caesar Cipher Attack decrypts messages encrypted using simple shift-based encryption.

## Features
- Decryption of multiple ciphertexts encrypted with the same one-time pad key
- Frequency analysis for efficient decryption
- Implementation of **Caesar Cipher Attack** using brute force

## Usage
For running the **Many Time Pad attack**:
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
1. **XOR Analysis**: The program XORs different ciphertexts together to reveal probable spaces
2. **Frequency Analysis**: Based on common English letter frequencies, the program determines likely characters
3. **Interactive Guessing (probably coming)**: The user can provide manual inputs to refine decryption results

The folder ciphertexts contains text files with ciphertexts to be dectrypted. The "mtp.txt" file has the ciphertexts that will be used for the Many Time Pad attack. They are in hex format, separated by a comma.


### Caesar Cipher Attack
1. **Brute Force**: Tries all possible shifts (0-25) and displays possible plaintexts
2. **Frequency Analysis**: Uses letter frequency statistics to guess the most likely shift
3. **User Interaction**: Allows users to refine results manually

