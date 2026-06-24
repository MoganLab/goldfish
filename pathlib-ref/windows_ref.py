#!/usr/bin/env python3
# pathlib Windows 真值参考脚本
#
# 用途:为 goldfish (liii path) 库的对齐工作提供 PureWindowsPath 的权威行为基准。
# 输出格式 `label => value`,可与 goldfish 审计探针的输出逐行 diff。
#
# 运行: python3 pathlib-ref/windows_ref.py
# 对照: 见 pathlib-ref/README.md
#
# 注意:这些是 PureWindowsPath 在 Windows 平台的行为;goldfish 在 (os-windows?)=>
# #t 时应复刻。macOS 上字符串 "C:\\a" 不解析为 windows(走 PurePosixPath 语义),
# 故 goldfish 的 windows 行为需用 path-of-drive/path-from-parts 构造 windows record
# 验证,或直接在 Windows CI 上跑。
#
# 字符串全部用普通转义(\\ 表示一个反斜杠),避免 raw string 末尾反斜杠的语法陷阱。

from pathlib import PureWindowsPath as W

BS = "\\"  # 单个反斜杠


def row(label, val):
    print(f"{label} => {val!r}")


# ---- str (path->string) ----
cases_str = [
    "C:\\a", "C:\\a\\b", "C:\\foo", "C:foo", "C:foo\\bar", "C:" + BS,
    "\\foo", BS, "\\\\srv", "\\\\srv\\sh", "\\\\srv\\sh\\a", "\\\\srv\\sh\\a\\b",
    "\\\\srv\\share\\a\\b", "\\\\srv\\share", "foo\\bar", "C:/Users/foo", "C:/a/b",
]
for s in cases_str:
    row(f"str {s}", str(W(s)))

# ---- drive / root ----
for s in cases_str:
    row(f"drive {s}", W(s).drive)
    row(f"root {s}", W(s).root)

# ---- name / stem / suffix / suffixes ----
for s in ["C:\\tmp\\demo.txt", "C:\\a\\b.tar.gz", "C:\\Users\\foo\\bar.tar.gz", "\\\\srv\\sh\\a.txt"]:
    row(f"name {s}", W(s).name)
    row(f"stem {s}", W(s).stem)
    row(f"suffix {s}", W(s).suffix)
    row(f"suffixes {s}", list(W(s).suffixes))

# ---- parent ----
for s in ["C:\\Users", "\\foo", "C:foo\\bar", "C:foo", "C:" + BS,
          "\\\\srv\\sh\\a", "\\\\srv\\sh\\a\\b", "\\\\srv\\sh", "C:\\a\\b\\c"]:
    row(f"parent {s}", str(W(s).parent))

# ---- parents ----
for s in ["C:\\a\\b", "C:\\a", "C:" + BS, "C:foo\\bar", "\\\\srv\\sh\\a\\b",
          "\\\\srv\\sh\\a", "\\\\srv\\sh", "\\foo"]:
    row(f"parents {s}", [str(x) for x in W(s).parents])

# ---- parts ----
for s in ["C:\\tmp\\demo.txt", "\\\\srv\\sh\\a\\b", "C:foo", "\\foo", "C:" + BS, "\\\\srv\\sh"]:
    row(f"parts {s}", list(W(s).parts))

# ---- is_absolute ----
for s in ["\\foo", "C:\\foo", "C:foo", "\\\\srv\\sh\\foo", BS, "C:" + BS, "\\\\srv\\sh"]:
    row(f"abs? {s}", W(s).is_absolute())

# ---- joinpath ----
joins = [
    ("C:\\a", ["\\b"]),                 # => C:\b (drive 继承)
    ("C:\\base", ["D:rel", "x.txt"]),   # => D:rel\x.txt (不同 drive 重置)
    ("C:\\base", ["C:rel"]),            # => C:\base\rel (同 drive 合并)
    ("C:\\a", ["D:\\b"]),               # => D:\b (完整绝对重置)
    ("\\\\srv\\sh", ["\\a"]),           # => \\srv\sh\a
    ("C:\\Users", ["foo", "bar.scm"]),  # => C:\Users\foo\bar.scm
]
for base, segs in joins:
    row(f"join {base} {' '.join(segs)}", str(W(base).joinpath(*segs)))

# ---- relative_to ----
rel_cases = [
    ("C:\\a\\b\\c", "C:\\a"), ("C:\\a\\b", "C:\\a\\b"), ("C:\\a\\b", "C:" + BS),
    ("\\\\srv\\sh\\a\\b", "\\\\srv\\sh"),
]
for a, b in rel_cases:
    row(f"rel-to {a} {b}", str(W(a).relative_to(W(b))))

# ---- with_name / with_stem / with_suffix ----
for s, arg in [("C:\\a\\b.txt", "c.md"), ("\\\\srv\\sh\\a.txt", "b.md")]:
    row(f"with-name {s} {arg}", str(W(s).with_name(arg)))
for s, arg in [("C:\\tmp\\a.tar.gz", "new")]:
    row(f"with-stem {s} {arg}", str(W(s).with_stem(arg)))
for s, arg in [("C:\\tmp\\a.txt", ".md"), ("C:\\tmp\\a.tar.gz", ""), ("\\\\srv\\sh\\a.txt", ".md")]:
    row(f"with-suffix {s} {arg}", str(W(s).with_suffix(arg)))

# ---- as_posix ----
for s in ["C:\\a\\b", "\\\\srv\\sh\\a", "C:foo\\bar", "C:" + BS]:
    row(f"as-posix {s}", W(s).as_posix())

# ---- match (windows, 大小写不敏感) ----
for path, pat in [("Foo.TXT", "*.txt"), ("C:\\a\\b\\foo.txt", "*.txt")]:
    row(f"match {path} {pat}", W(path).match(pat))
