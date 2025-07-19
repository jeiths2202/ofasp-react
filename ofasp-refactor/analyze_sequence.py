#!/usr/bin/env python3
"""
Analyze the problematic EBCDIC sequence to understand SOSI positioning
"""

# The problematic sequence from the real file
sequence = "0E60FA4660FA8D61428660FA8D667C457F488D60F9805573657260F9802041757468656E7469636174696F6E272E2020202020202020202020202020"

print("Analyzing EBCDIC sequence:")
print(f"Hex: {sequence}")
print()

# Convert to bytes
bytes_data = bytes.fromhex(sequence)
print(f"Bytes: {' '.join(f'{b:02X}' for b in bytes_data)}")
print()

# Look for SOSI codes
for i, byte in enumerate(bytes_data):
    if byte == 0x0E:
        print(f"SO (0x0E) found at position {i}")
    elif byte == 0x0F:
        print(f"SI (0x0F) found at position {i}")

print()

# Show the ASCII portion
ascii_start = None
for i, byte in enumerate(bytes_data):
    if byte == ord('U'):  # Look for 'U' in "User"
        ascii_start = i
        break

if ascii_start:
    print(f"ASCII 'User' starts at position {ascii_start}")
    ascii_portion = bytes_data[ascii_start:ascii_start+30]  # Show next 30 bytes
    print(f"ASCII portion: {' '.join(f'{b:02X}' for b in ascii_portion)}")
    print(f"ASCII text: {ascii_portion.decode('ascii', errors='replace')}")
    
    # Check if SI is in the ASCII portion
    for i, byte in enumerate(ascii_portion):
        if byte == 0x0F:
            print(f"SI (0x0F) found within ASCII text at relative position {i}")
            print(f"Absolute position: {ascii_start + i}")