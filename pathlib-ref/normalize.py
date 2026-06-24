#!/usr/bin/env python3
# 规范化 diff:对比 pathlib 真值(posix_ref.py / windows_ref.py)与 goldfish 审计
# (posix_audit.scm / windows_audit.scm)的输出。
#
# 用法: python3 pathlib-ref/normalize.py posix
#       python3 pathlib-ref/normalize.py windows
#
# 按 label 对齐两边,把 value 归一化后逐行比较,只输出不一致的行(并标注差异)。
# value 归一化:去引号、统一 list 括号为 (...)、布尔 True/False -> #t/#f。

import subprocess
import sys
import re

ROOT = subprocess.DEVNULL  # placeholder
# resolve repo root from this file's location
import os
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GF = os.path.join(REPO, "bin", "gf")


def norm(v):
    s = v.strip()
    # python bool
    if s in ("True", "False"):
        return "#t" if s == "True" else "#f"
    # list:[a,b](python) 或 ()(scheme) -> 统一为 (a b)
    if (s.startswith("[") and s.endswith("]")) or (s.startswith("(") and s.endswith(")")):
        inner = s[1:-1].strip()
        if not inner:
            return "()"
        items = [norm(x.strip()) for x in split_top(inner)]
        return "(" + " ".join(items) + ")"
    # python str '...' / "..." -> 去引号
    if (s.startswith("'") and s.endswith("'")) or (s.startswith('"') and s.endswith('"')):
        return python_unescape(s[1:-1])
    # scheme str "..." -> 去引号
    if s.startswith('"') and s.endswith('"'):
        return s[1:-1]
    return s


def split_top(s):
    """按逗号或空格切分顶层(忽略括号/引号内)。python 用逗号,scheme 用空格分隔 list 元素。"""
    out, cur, depth, in_str, q = [], "", 0, False, None
    for ch in s:
        if in_str:
            cur += ch
            if ch == q:
                in_str = False
        elif ch in "'\"":
            in_str = True
            q = ch
            cur += ch
        elif ch in "[(":
            depth += 1
            cur += ch
        elif ch in "])":
            depth -= 1
            cur += ch
        elif ch == "," and depth == 0:
            out.append(cur)
            cur = ""
        elif ch == " " and depth == 0:
            if cur:
                out.append(cur)
                cur = ""
        else:
            cur += ch
    if cur.strip():
        out.append(cur)
    return out


def python_unescape(s):
    # 把 python repr 的转义还原(\', \\, \n ...)
    return s.encode().decode("unicode_escape") if "\\" in s else s


def parse(path):
    """解析 `label => value` 行为 (label, raw_value)。"""
    rows = {}
    with open(path) as f:
        for line in f:
            line = line.rstrip("\n")
            if " => " not in line:
                continue
            label, val = line.split(" => ", 1)
            rows[label.strip()] = val
    return rows


def run(cmd):
    return subprocess.check_output(cmd, cwd=REPO, text=True)


def main(flavor):
    py_file = os.path.join(REPO, "pathlib-ref", f"{flavor}_ref.py")
    scm_file = os.path.join(REPO, "pathlib-ref", f"{flavor}_audit.scm")
    py_out = run(["python3", py_file])
    scm_out = run([GF, scm_file])

    py_rows = {}
    scm_rows = {}
    for line in py_out.splitlines():
        if " => " in line:
            lab, val = line.split(" => ", 1)
            py_rows[lab.strip()] = val
    for line in scm_out.splitlines():
        if " => " in line:
            lab, val = line.split(" => ", 1)
            scm_rows[lab.strip()] = val

    labels = list(dict.fromkeys(list(py_rows.keys()) + list(scm_rows.keys())))
    diffs = 0
    only_py, only_scm = 0, 0
    for lab in labels:
        if lab not in scm_rows:
            print(f"[仅 python] {lab} => {py_rows[lab]}")
            only_py += 1
            diffs += 1
            continue
        if lab not in py_rows:
            print(f"[仅 scheme] {lab} => {scm_rows[lab]}")
            only_scm += 1
            diffs += 1
            continue
        a = norm(py_rows[lab])
        b = norm(scm_rows[lab])
        if a != b:
            print(f"[差异] {lab}\n  python = {a}\n  scheme = {b}")
            diffs += 1
    print(f"\n=== {flavor}: 共 {len(labels)} 项,差异 {diffs} (仅python {only_py}, 仅scheme {only_scm}) ===")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "posix")
