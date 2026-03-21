# Goldfish Scheme / [金鱼 Scheme](README_ZH.md)
> Make Scheme as easy to use and practical as Python!

Goldfish Scheme is a Scheme interpreter with the following features:
+ R7RS-small compatible
+ Scala-like functional collection
+ Python-like versatile standard library
+ Small and fast

<img src="GoldfishScheme-logo.png" alt="示例图片" style="width: 360pt;">

## Demo Code
### Named parameter
``` scheme
(define* (person (name "Bob") (age 21))
  (string-append name ": " (number->string age)))

(person :name "Alice" :age 3)
```
### Unicode Support
``` scheme
(import (liii lang))

($ "你好，世界" 0) ; => 你
($ "你好，世界" 4) ; => 界
($ "你好，世界" :length) ; => 5
```

### Functional Data Pipeline
![](r7rs_vs_goldfish.png)

With `prime?` provided, filter twin prime numbers in this way:
``` scheme
(import (liii lang))

(($ 1 :to 100)
 :filter prime?
 :filter (lambda (x) (prime? (+ x 2)))
 :map (lambda (x) (cons x (+ x 2)))
 :collect)
```

### Scala like case class
``` scheme
(define-case-class person
  ((name string?)
   (age integer?))
  
  (define (%to-string)
    (string-append "I am " name " " (number->string age) " years old!"))
  (define (%greet x)
    (string-append "Hi " x ", " (%to-string))))

(define bob (person "Bob" 21))

(bob :to-string) ; => "I am Bob 21 years old!"
(bob :greet "Alice") ; => "Hi Alice, I am Bob 21 years old!"
```

## Simplicity is Beauty
Goldfish Scheme still follows the same principle of simplicity as S7 Scheme. Currently, Goldfish Scheme only depends on [S7 Scheme](https://ccrma.stanford.edu/software/s7/), [tbox](https://gitee.com/tboox/tbox) and C++ standard library defined in C++ 98.

Just like S7 Scheme, [src/goldfish.hpp](src/goldfish.hpp) and [src/goldfish.cpp](src/goldfish.cpp) are the only key source code needed to build the goldfish interpreter binary.


## Standard Library
### Scala-like collections
| Library | Description |
|---------|-------------|
| [(liii rich-char)](tests/goldfish/liii/rich-char-test.scm) | boxed char with rich char and instance methods |
| [(liii rich-string)](tests/goldfish/liii/rich-string-test.scm) | boxed string with rich char and instance methods |
| [(liii rich-list)](tests/goldfish/liii/rich-list-test.scm) | boxed list with rich static and instance methods |
| [(liii rich-vector)](tests/goldfish/liii/rich-vector-test.scm) | boxed vector with rich static and instance methods |
| [(liii rich-hash-table)](tests/goldfish/liii/rich-hash-table-test.scm) | boxed hash-table with rich static and instance methods |
| [(liii rich-path)](tests/goldfish/liii/rich-path-test.scm) | boxed path with rich static and instance methods |

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
| [(liii uuid)](goldfish/liii/uuid.scm)             | UUID generation                      | `uuid4`                                                          |


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

### GNU/Linux
Here are commandlines to build it on Debian bookworm:
```
sudo apt install xmake git unzip curl g++
git clone https://gitee.com/LiiiLabs/goldfish.git
# git clone https://github.com/LiiiLabs/goldfish.git
cd goldfish
xmake b goldfish
bin/gf --version
```
You can also install it to `/opt`:
```
sudo xmake i -o /opt/goldfish --root
/opt/goldfish/bin/gf
```
For uninstallation, just:
```
sudo rm -rf /opt/goldfish
```

## Commandlinefu
This section assumes you have executed `xmake b goldfish` sucessfully and `bin/gf` is available.

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
> bin/gf
Goldfish Scheme 17.11.30 by LiiiLabs

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
> bin/gf version
Goldfish Scheme 17.11.30 by LiiiLabs
based on S7 Scheme 11.5 (22-Sep-2025)
```

### Evaluate Code
`eval` subcommand helps you evaluate Scheme code on the fly:
```
> bin/gf eval "(+ 1 2)"
3
> bin/gf eval "(begin (import (srfi srfi-1)) (first (list 1 2 3)))"
1
> bin/gf eval "(begin (import (liii sys)) (display (argv)) (newline))" 1 2 3
("bin/gf" "eval" "(begin (import (liii sys)) (display (argv)) (newline))" "1" "2" "3")
```

### Load File
`load` subcommand helps you load a Scheme file and enter REPL:
```
> bin/gf load tests/goldfish/liii/base-test.scm
; load the file and enter REPL
```

### Run File Directly
You can also load and evaluate a Scheme file directly:
```
> bin/gf tests/goldfish/liii/base-test.scm
; *** checks *** : 1973 correct, 0 failed.
```

### Mode Option
`-m` or `--mode` helps you specify the standard library mode:

+ `default`: `-m default` is the equiv of `-m liii`
+ `liii`: Goldfish Scheme with `(liii oop)`, `(liii base)` and `(liii error)`
+ `scheme`: Goldfish Scheme with `(liii base)` and `(liii error)`
+ `sicp`: S7 Scheme with `(scheme base)` and `(srfi sicp)`
+ `r7rs`: S7 Scheme with `(scheme base)`
+ `s7`: S7 Scheme without any extra library


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
