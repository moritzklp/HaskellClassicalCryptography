# Report Example

See [report.pdf](report.pdf) for documentation.

Note: To rename the project you need to replace all occurrences of "report" with your own library name in the following files:

- `hie.yaml`
- `package.yaml`

{Building}
To build the project run "stack build" or make. Then run the program with "stack run".

{Usage}
The folder ciphertexts contains text files with ciphertexts to be dectrypted. The "mtp.txt" file has the ciphertexts that will be used for the Many Time Pad attack. They are in hex format, separated by a comma.

