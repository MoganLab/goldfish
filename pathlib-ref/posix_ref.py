#!/usr/bin/env python3
# pathlib POSIX 真值参考脚本
#
# 用途:为 goldfish (liii path) 库的对齐工作提供 PurePosixPath 的权威行为基准。
# 输出格式 `label => value`,可与 goldfish 审计探针的输出逐行 diff。
#
# 运行: python3 pathlib-ref/posix_ref.py
# 对照: 见 pathlib-ref/README.md

from pathlib import PurePosixPath as P


def row(label, val):
    print(f"{label} => {val!r}")


# ---- str (path->string) ----
for s in ['.', '', '..', 'a/b', 'a//b', 'a/./b', './a', 'a/', '/a/', 'a/../b',
          '/..', '/.', '/', '/a/b/c', 'tmp/demo.txt', 'a.b.c', '.hidden', 'foo.']:
    row(f"str {s}", str(P(s)))

# ---- name ----
for s in ['.', '..', '', 'a/', '/a/b/', 'a..b', 'foo.', 'a...b', 'file.txt', '.hidden']:
    row(f"name {s}", P(s).name)

# ---- stem / suffix / suffixes ----
for s in ['file.txt', 'archive.tar.gz', '.hidden', 'noext', '', '.', '..', 'foo.',
          'a..b', 'a...b', 'a.b.c', 'config.yaml.bak']:
    row(f"stem {s}", P(s).stem)
    row(f"suffix {s}", P(s).suffix)
    row(f"suffixes {s}", list(P(s).suffixes))

# ---- parent ----
for s in ['.', '..', '', '/', '/a', '/a/b', '/a/b/c', 'a', 'a/b', 'a/', '/a/', 'a/../b']:
    row(f"parent {s}", str(P(s).parent))

# ---- parents ----
for s in ['/a/b/c', '/a/b', '/a', '/', 'a/b/c', 'a', '.', '..']:
    row(f"parents {s}", [str(x) for x in P(s).parents])

# ---- parts ----
for s in ['.', '', '..', '/', '/a/b', 'a/b', '/a/', 'a/', '/..', 'a//b']:
    row(f"parts {s}", list(P(s).parts))

# ---- joinpath ----
joins = [
    ('/a', ['/b']), ('/a', ['b']), ('a', ['/b']), ('/a', ['b', '/c', 'd']),
    ('/a', ['']), ('a', ['']), ('', ['b']), ('', ['']), ('a', []),
    ('/', ['tmp', 'demo.txt']), ('/a/', ['b']),
]
for base, segs in joins:
    row(f"join {base} {' '.join(segs)}", str(P(base).joinpath(*segs)))

# ---- match ----
matches = [
    ('a/b/c', 'b/c'), ('a/b/c', 'c'), ('a/b/c', 'b'), ('a/b/c', 'a/b/c'),
    ('a/b/c', '*/c'), ('/a/b/c', '/a/b/c'), ('/a/b/c', 'a/b/c'),
    ('/x/a/b/c', '/a/b/c'), ('/a/b', 'a/b'), ('a/b/c', 'a/c'),
    ('foo.txt', '*.txt'), ('foo.TXT', '*.txt'), ('foo.txt', 'foo.???'),
]
for path, pat in matches:
    row(f"match {path} {pat}", P(path).match(pat))

# ---- is_absolute / relative ----
for s in ['/a', 'a', '.', '/', '/a/b', 'a/b']:
    row(f"abs? {s}", P(s).is_absolute())

# ---- relative_to ----
for a, b in [('/a/b', '/a'), ('/a', '/a'), ('/a/b/c', '/a'), ('/a/b', '/'),
             ('a/b/c', 'a/b'), ('a/b', 'a/b')]:
    row(f"rel-to {a} {b}", str(P(a).relative_to(b)))

# ---- with_name / with_stem / with_suffix ----
for s, arg in [('a.txt', 'c.md'), ('/a/b.txt', 'c.md'), ('x.txt', 'y')]:
    row(f"with-name {s} {arg}", str(P(s).with_name(arg)))
for s, arg in [('a.tar.gz', 'new'), ('a.b.c', 'new'), ('a.txt', 'b'),
               ('README', 'new'), ('.bashrc', 'new'), ('foo.', 'new')]:
    row(f"with-stem {s} {arg}", str(P(s).with_stem(arg)))
for s, arg in [('a.txt', '.md'), ('README', '.md'), ('a.tar.gz', '.md'),
               ('a.txt', ''), ('a.tar.gz', ''), ('.bashrc', '.bak')]:
    row(f"with-suffix {s} {arg}", str(P(s).with_suffix(arg)))

# ---- as_posix ----
for s in ['/a/b/c', 'a/b', '/', '.']:
    row(f"as-posix {s}", P(s).as_posix())
