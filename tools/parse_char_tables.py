#!/usr/bin/env python3
"""Parse the 5 large char hash tables in goldfish/scheme/char.scm.

Usage:
  python3 tools/parse_char_tables.py dump     # entry counts per table
  python3 tools/parse_char_tables.py cover    # diff table keys vs tested codepoints
  python3 tools/parse_char_tables.py ranges   # print range-built structure
"""
import re
import sys

CHAR_SCM = "goldfish/scheme/char.scm"

TABLES = {
    "*char-upcase-ht*": "tests/scheme/char/char-upcase-test.scm",
    "*char-downcase-ht*": "tests/scheme/char/char-downcase-test.scm",
    "*char-non-alphabetic-ht*": "tests/scheme/char/char-alphabetic-p-test.scm",
    "*char-upper-ht*": "tests/scheme/char/char-upper-case-p-test.scm",
    "*char-lower-ht*": "tests/scheme/char/char-lower-case-p-test.scm",
}

# (s7-hash-table-set! *TABLE* CP VAL)  VAL := NUM | #t | (+ NUM NUM)
RE_SET = re.compile(
    r"\(s7-hash-table-set!\s+(\*char-[a-z-]+-ht\*)\s+(\d+)\s+"
    r"(\(\+\s+\d+\s+-?\d+\)|\d+|#t)\)")
# do-loop range with body (s7-hash-table-set! *TABLE* i EXPR)
RE_DO = re.compile(
    r"\(do \(\(i (\d+) \(\+ i 1\)\)\)\s*\n\s*\(\(> i (\d+)\)\)\s*\n"
    r"\s*\(s7-hash-table-set!\s+(\*char-[a-z-]+-ht\*)\s+i\s+"
    r"(\(\+\s+i\s+-?\d+\)|#t)\)\s*\n\s*\) ;do")
# (char-ht-set-range! *TABLE* START END)
RE_RANGE = re.compile(
    r"\(char-ht-set-range!\s+(\*char-[a-z-]+-ht\*)\s+(\d+)\s+(\d+)\)")


def parse_tables(path):
    """Return {table: {codepoint: value}} with value int or True."""
    src = open(path, encoding="utf-8").read()
    tables = {t: {} for t in TABLES}
    for m in RE_SET.finditer(src):
        tbl, cp, val = m.group(1), int(m.group(2)), m.group(3)
        if tbl not in tables:
            continue
        if val == "#t":
            tables[tbl][cp] = True
        elif val.startswith("("):
            nums = re.findall(r"-?\d+", val)
            tables[tbl][cp] = int(nums[0]) + int(nums[1])
        else:
            tables[tbl][cp] = int(val)
    for m in RE_DO.finditer(src):
        start, end, tbl, expr = int(m.group(1)), int(m.group(2)), m.group(3), m.group(4)
        if tbl not in tables:
            continue
        if expr == "#t":
            for i in range(start, end + 1):
                tables[tbl][i] = True
        else:
            delta = int(re.search(r"-?\d+", expr.split("i", 1)[1]).group(0))
            for i in range(start, end + 1):
                tables[tbl][i] = i + delta
    for m in RE_RANGE.finditer(src):
        tbl, start, end = m.group(1), int(m.group(2)), int(m.group(3))
        if tbl in tables:
            for i in range(start, end + 1):
                tables[tbl][i] = True
    return tables


def tested_codepoints(path):
    src = open(path, encoding="utf-8").read()
    cps = set()
    for m in re.finditer(r"#\\x([0-9A-Fa-f]+)", src):
        cps.add(int(m.group(1), 16))
    for m in re.finditer(r"#\\(.)", src):
        cps.add(ord(m.group(1)))
    return cps


def main():
    tables = parse_tables(CHAR_SCM)
    cmd = sys.argv[1] if len(sys.argv) > 1 else "dump"
    if cmd == "dump":
        for tbl, entries in tables.items():
            print(f"{tbl}: {len(entries)} entries")
    elif cmd == "cover":
        for tbl, test_file in TABLES.items():
            tested = tested_codepoints(test_file)
            keys = set(tables[tbl])
            missing = sorted(keys - tested)
            print(f"{tbl}: {len(keys)} entries, tested {len(keys & tested)}, "
                  f"missing {len(missing)}")
            if missing and len(missing) <= 100:
                print("  missing:", " ".join(hex(c) for c in missing))


if __name__ == "__main__":
    main()
