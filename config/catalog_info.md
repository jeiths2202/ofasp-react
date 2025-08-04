# Catalog.json Record Type Reference

## Record Types

### FB (Fixed Block)
- Fixed-length records without line terminators
- Each record has exactly the same length (RECLEN)
- Traditional mainframe format
- No newline characters between records

### VB (Variable Block)
- Variable-length records with Record Descriptor Word (RDW)
- Each record starts with 4-byte RDW containing record length
- RDW format: 2 bytes length (big-endian) + 2 bytes reserved
- No newline characters between records
- Used for files with varying record lengths

### LB (Line Block)
- Open system format not in original Fujitsu ASP
- Each record terminated with newline character (\n)
- Standard text file format
- Variable length records with line breaks

## Usage in catalog.json

```json
{
  "DISK01": {
    "EMPLOYEE.DAT": {
      "RECTYPE": "FB",
      "RECLEN": 100
    },
    "REPORT.TXT": {
      "RECTYPE": "LB",
      "RECLEN": 80
    },
    "VARIABLE.DAT": {
      "RECTYPE": "VB",
      "RECLEN": 256
    }
  }
}
```

## Default Values
- RECTYPE: "FB" (if not specified)
- RECLEN: 80 (if not specified)