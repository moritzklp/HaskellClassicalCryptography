def caesar_cipher(text, shift):
    result = ""
    for char in text:
        if char.isalpha():
            shift_base = ord('A') if char.isupper() else ord('a')
            result += chr((ord(char) - shift_base + shift) % 26 + shift_base)
        else:
            result += char
    return result

# Original text
original_text = "The quick brown fox jumps over the lazy dog. This is an example of a Caesar Cipher shift."
print(f"[")

# Print shifted versions
for shift in range(1, 26):
    shifted_text = caesar_cipher(original_text, shift)
    print(f"\"{shifted_text}\", ")

print(f"\"{original_text}\"")
print(f"]")
