# Goldfish Scheme / [金鱼 Scheme](README_ZH.md)
> Make Scheme as easy to use and practical as Python!

Goldfish Scheme is a Scheme interpreter with the following features:
+ R7RS-small compatible
+ Python-like versatile standard library
+ AI Coding friendly
+ Small and fast

<img src="GoldfishScheme-logo.png" alt="示例图片" style="width: 360pt;">

## Simplicity is Beauty
Goldfish Scheme still follows the same principle of simplicity as S7 Scheme. Currently, Goldfish Scheme only depends on [S7 Scheme](https://ccrma.stanford.edu/software/s7/), [tbox](https://gitee.com/tboox/tbox) and C++ standard library defined in C++ 98.

Just like S7 Scheme, [src/goldfish.hpp](src/goldfish.hpp) and [src/goldfish.cpp](src/goldfish.cpp) are the only key source code needed to build the goldfish interpreter binary.


## Standard Library
### Python-like standard library

| Library                                           | Description                          | Example functions                                                |
| ------------------------------------------------- | ------------------------------------ | ---------------------------------------------------------------- |
| [(liii base)](goldfish/liii/base.scm)             | Basic routines                       | `==`, `!=`, `display*`                                           |
| [(liii error)](goldfish/liii/error.scm)           | Python like Errors                   | `os-error` to raise `'os-error` just like OSError in Python      |
| [(liii check)](goldfish/liii/check.scm)           | Test framework based on SRFI-78      | `check`, `check-catch`                                           |
| [(liii case)](goldfish/liii/case.scm)             | Pattern matching                     | `case*`                                                          |
| [(liii list)](goldfish/liii/list.scm)             | List Library                         | `list-view`, `fold`                                              |
| [(liii bitwise)](goldfish/liii/bitwise.scm)       | Bitwise Library                      | `bitwise-and`, `bitwise-or`                                      |
| [(liii string)](goldfish/liii/string.scm)         | String Library                       | `string-join`                                                    |
| [(liii vector)](goldfish/liii/vector.scm)         | Vector Library                       | `vector-index`                                                   |
| [(liii hash-table)](goldfish/liii/hash-table.scm) | Hash Table Library                   | `hash-table-empty?`, `hash-table-contains?`                      |
| [(liii sys)](goldfish/liii/sys.scm)               | Library looks like Python sys module | `argv`                                                           |
| [(liii os)](goldfish/liii/os.scm)                 | Library looks like Python os module  | `getenv`, `mkdir`                                                |
| [(liii path)](goldfish/liii/path.scm)             | Path Library                         | `path-dir?`, `path-file?`                                        |
| [(liii range)](goldfish/liii/range.scm)           | Range Library                        | `numeric-range`, `iota`                                          |
| [(liii option)](goldfish/liii/option.scm)         | Option Type Library                  | `option?`, `option-map`, `option-flatten`                        |
| [(liii either)](goldfish/liii/either.scm)         | Either Type Library                  | `left?`, `right?`, `either-map`                                  |
| [(liii uuid)](goldfish/liii/uuid.scm)             | UUID generation                      | `uuid4`                                                          |
| [(liii http)](goldfish/liii/http.scm)             | HTTP client library                  | `http-get`, `http-post`, `http-head`                             |
| [(liii json)](goldfish/liii/json.scm)             | JSON parsing and manipulation        | `string->json`, `json->string`                                   |


### SRFI

| Library           | Status   | Description                  |
| ----------------- | -------- | ---------------------------- |
| `(srfi srfi-1)`   | Part     | List Library                 |
| `(srfi srfi-8)`   | Complete | Provide `receive`            |
| `(srfi srfi-9)`   | Complete | Provide `define-record-type` |
| `(srfi srfi-13)`  | Complete | String Library               |
| `(srfi srfi-16)`  | Complete | Provide `case-lambda`        |
| `(srfi srfi-39)`  | Complete | Parameter Objects            |
| `(srfi srfi-78)`  | Part     | Lightweigted Test Framework  |
| `(srfi srfi-125)` | Part     | Hash Table                   |
| `(srfi srfi-133)` | Part     | Vector                       |
| `(srfi srfi-151)` | Part     | Bitwise Operations           |
| `(srfi srfi-196)` | Complete | Range Library                |
| `(srfi srfi-216)` | Part     | SICP                         |

### R7RS Standard Libraries
| Library                | Description           |
| ---------------------- | --------------------- |
| `(scheme base)`        | Base library          |
| `(scheme case-lambda)` | Provide `case-lambda` |
| `(scheme char)`        | Character Library     |
| `(scheme file)`        | File operations       |
| `(scheme time)`        | Time library          |


## Installation
Goldfish Scheme is bundled in Mogan Research (since v1.2.8), just [install Mogan Research](https://mogan.app/guide/Install.html) to install Goldfish Scheme.

Besides the Goldfish Scheme interpreter, a nice structured [Goldfish Scheme REPL](https://mogan.app/guide/plugin_goldfish.html) is availabe in Mogan Research.

The following guide will help you build and install Goldfish step by step.

### macOS
Here are commandlines to build it on macOS:
```
brew tap MoganLab/goldfish
brew install goldfish
```
For uninstallation, just:
```
brew uninstall goldfish
```

## Commandlinefu
If you build from source manually, you can find the executable at `bin/gf`.

### Subcommands

Goldfish Scheme uses subcommands for different operations:

| Subcommand | Description |
|------------|-------------|
| `help` | Display help message |
| `version` | Display version information |
| `eval CODE` | Evaluate Scheme code |
| `load FILE` | Load Scheme file and enter REPL |
| `repl` | Enter interactive REPL mode |
| `run TARGET` | Run main function from target |
| `test` | Run tests |
| `fix PATH` | Format Scheme code |
| `FILE` | Load and evaluate Scheme file directly |

### Display Help
Without any command, it will print the help message:
```
> gf
Goldfish Scheme 17.11.40 by LiiiLabs

Commands:
  help               Display this help message
  version            Display version
  eval CODE          Evaluate Scheme code
  load FILE          Load Scheme code from FILE, then enter REPL
  ...
```

### Display Version
`version` subcommand will print the Goldfish Scheme version and the underlying S7 Scheme version:
```
> gf version
Goldfish Scheme 17.11.40 by LiiiLabs
based on S7 Scheme 11.5 (22-Sep-2025)
```

### Evaluate Code
`eval` subcommand helps you evaluate Scheme code on the fly:
```
> gf eval "(+ 1 2)"
3
> gf eval "(begin (import (srfi srfi-1)) (first (list 1 2 3)))"
1
> gf eval "(begin (import (liii sys)) (display (argv)) (newline))" 1 2 3
("bin/gf" "eval" "(begin (import (liii sys)) (display (argv)) (newline))" "1" "2" "3")
```

### Load File
`load` subcommand helps you load a Scheme file and enter REPL:
```
> gf load tests/goldfish/liii/base-test.scm
; load the file and enter REPL
```

### Run File Directly
You can also load and evaluate a Scheme file directly:
```
> gf tests/goldfish/liii/base-test.scm
; *** checks *** : 1973 correct, 0 failed.
```

### Mode Option
`-m` or `--mode` helps you specify the standard library mode:

+ `default`: `-m default` is the equiv of `-m r7rs`
+ `liii`: Goldfish Scheme with `(liii base)`, `(liii error)` and `(liii string)`
+ `scheme`: Goldfish Scheme with `(liii base)` and `(liii error)`
+ `sicp`: S7 Scheme with `(scheme base)` and `(srfi sicp)`
+ `r7rs`: S7 Scheme with `(scheme base)`
+ `s7`: S7 Scheme without any extra library

### Library Search Path
Goldfish also supports extra library search directories during startup:

+ `-I DIR`: prepend `DIR` to the library search path
+ `-A DIR`: append `DIR` to the library search path

For example:
```bash
gf -I ~/.local/goldfish/liii-goldfix eval "(begin (import (liii goldfix)) 'ok)"
```

On startup, Goldfish also automatically prepends each directory under `~/.local/goldfish/` whose name matches `xxx-yyy` and which contains at least one `.scm` file.


## Versioning
Goldfish Scheme x.y.z means that it is using the tbox x, based on S7 Scheme y, and z is the patch version. To clarify, the second version of Goldfish Scheme is `17.10.1`, it means that it is using `tbox 1.7.x`, based on `S7 Scheme 10.x`, the patch version is `1`.

## Why we created Goldfish Scheme
Goldfish Scheme is implemented to overcome the defects of [S7 Scheme](https://ccrma.stanford.edu/software/s7/):
1. Distribute the ready-to-use Goldfish Scheme interpreter and structured REPL on Linux/macOS/Windows
2. Try to implement the [R7RS-small](https://small.r7rs.org) standard
3. Try to provide the useful SRFI in R7RS library format

## License
Goldfish Scheme is licensed under Apache 2.0, some of the code snippets which are derived from the S7 Scheme repo and SRFI have been explicitly claimed in the related source files.


## Citation

The reader can cite our work with the following BibTeX entry:

```
@book{goldfish,
    author = {Da Shen and Nian Liu and Yansong Li and Shuting Zhao and Shen Wei and Andy Yu and Siyu Xing and Jiayi Dong and Yancheng Li and Xinyi Yu and Zhiwen Fu and Duolei Wang and Leiyu He and Yingyao Zhou and Noctis Zhang},
    title = {Goldfish Scheme: A Scheme Interpreter with Python-Like Standard Library},
    publisher = {LIII NETWORK},
    year = {2024},
    url = {https://github.com/LiiiLabs/goldfish/releases/download/v17.10.9/Goldfish.pdf}
}
```
