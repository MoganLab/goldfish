# 金鱼Scheme / [Goldfish Scheme](README.md)
> 让Scheme和Python一样易用且实用！

金鱼Scheme 是一个 Scheme 解释器，具有以下特性：
+ 兼容 R7RS-small 标准
+ 提供类似 Python 的功能丰富的标准库
+ AI 编程友好
+ 小巧且快速

<img src="GoldfishScheme-logo.png" alt="示例图片" style="width: 360pt;">

## 以简为美
金鱼 Scheme 使用 C++17 原生求值器，以及 tbox、C++ 标准库和 BDWGC。

程序入口是 [src/runtime/native_main.cpp](src/runtime/native_main.cpp)，求值器与
自举实现位于 `src/runtime/`。

## 标准库
### 类似Python的标准库
形如`(liii xyz)`的是金鱼标准库，模仿Python标准库的函数接口和实现方式，降低用户的学习成本。

| 库                                                | 描述                            | 示例函数                                                           |
| ------------------------------------------------- | ------------------------------- | ------------------------------------------------------------------ |
| [(liii base)](goldfish/liii/base.scm)             | 基础库                          | `==`, `!=`, `display*`                                             |
| [(liii error)](goldfish/liii/error.scm)           | 提供类似Python的错误函数        | `os-error`函数抛出`'os-error`，类似Python的OSError                 |
| [(liii check)](goldfish/liii/check.scm)           | 基于SRFI 78的轻量级测试库加强版 | `check`, `check-catch`                                             |
| [(liii case)](goldfish/liii/case.scm)             | 模式匹配                        | `case*`                                                            |
| [(liii list)](goldfish/liii/list.scm)             | 列表函数库                      | `list-view`, `fold`                                                |
| [(liii bitwise)](goldfish/liii/bitwise.scm)       | 位运算函数库                    | `bitwise-and`, `bitwise-or`                                        |
| [(liii string)](goldfish/liii/string.scm)         | 字符串函数库                    | `string-join`                                                      |
| [(liii vector)](goldfish/liii/vector.scm)         | 向量函数库                      | `vector-index`                                                     |
| [(liii hash-table)](goldfish/liii/hash-table.scm) | 哈希表                          | `hash-table-empty?`, `hash-table-contains?`                        |
| [(liii sys)](goldfish/liii/sys.scm)               | 库类似于 Python 的 `sys` 模块   | `argv`                                                             |
| [(liii os)](goldfish/liii/os.scm)                 | 库类似于 Python 的 `os` 模块    | `getenv`, `mkdir`                                                  |
| [(liii path)](goldfish/liii/path.scm)             | 路径函数库                      | `path-dir?`, `path-file?`                                          |
| [(liii range)](goldfish/liii/range.scm)           | 范围库                          | `numeric-range`, `iota`                                            |
| [(liii option)](goldfish/liii/option.scm)         | Option 类型库                   | `option?`, `option-map`, `option-flatten`                          |
| [(liii either)](goldfish/liii/either.scm)         | Either 类型库（左值/右值）      | `left?`, `right?`, `either-map`                                    |
| [(liii uuid)](goldfish/liii/uuid.scm)             | UUID 生成                       | `uuid4`                                                            |
| [(liii http)](goldfish/liii/http.scm)             | HTTP 客户端库                   | `http-get`, `http-post`, `http-head`                               |
| [(liii json)](goldfish/liii/json.scm)             | JSON 解析和操作                 | `string->json`, `json->string`                                     |
| [(liii config-parser)](goldfish/liii/config-parser.scm) | INI 配置文件解析器      | `config-read-string`, `config-get`, `config-write`                 |

### SRFI

| 库                | 状态 | 描述                      |
| ----------------- | ---- | ------------------------- |
| `(srfi srfi-1)`   | 部分 | 列表库                    |
| `(srfi srfi-8)`   | 完整 | 提供 `receive`            |
| `(srfi srfi-9)`   | 完整 | 提供 `define-record-type` |
| `(srfi srfi-13)`  | 完整 | 字符串库                  |
| `(srfi srfi-16)`  | 完整 | 提供 `case-lambda`        |
| `(srfi srfi-39)`  | 完整 | 参数对象                  |
| `(srfi srfi-78)`  | 部分 | 轻量级测试框架            |
| `(srfi srfi-125)` | 部分 | 哈希表                    |
| `(srfi srfi-133)` | 部分 | 向量                      |
| `(srfi srfi-151)` | 部分 | 位运算                    |
| `(srfi srfi-196)` | 完整 | Range 库                  |
| `(srfi srfi-216)` | 部分 | SICP                      |


### R7RS 标准库

| 库                     | 描述               |
| ---------------------- | ------------------ |
| `(scheme base)`        | 基础库             |
| `(scheme case-lambda)` | 提供 `case-lambda` |
| `(scheme char)`        | 字符函数库         |
| `(scheme file)`        | 文件操作           |
| `(scheme time)`        | 时间库             |

## 安装
金鱼Scheme自 v1.2.8 起已集成在墨干理工套件中，只需[安装墨干](https://mogan.app/zh/guide/Install.html)即可安装 金鱼Scheme。

除了金鱼Scheme解释器外，墨干还提供了一个结构化的[金鱼Scheme REPL](https://mogan.app/guide/plugin_goldfish.html)。

### macOS 安装
使用 Homebrew 安装：
```
# 添加 Goldfish 的 Tap 仓库
brew tap MoganLab/goldfish

# 安装 Goldfish
brew install goldfish
```

卸载
如果需要卸载，请执行：
```
brew uninstall goldfish
```

## 命令行技巧
如果您手动从源码编译，可以在 `bin/gf` 找到可执行文件。

### 子命令

金鱼Scheme 使用子命令进行不同操作：

| 子命令 | 描述 |
|------------|-------------|
| `help` | 显示帮助信息 |
| `version` | 显示版本信息 |
| `eval CODE`, `-e CODE` | 求值 Scheme 代码 |
| `load FILE` | 加载 Scheme 文件并进入 REPL |
| `repl` | 进入交互式 REPL 模式 |
| `run TARGET` | 从目标运行 main 函数 |
| `test` | 运行测试 |
| `fix PATH` | 格式化 Scheme 代码 |
| `FILE` | 直接加载并求值 Scheme 文件 |

使用 `gf test --all` 运行全部测试，使用 `gf test PATH` 运行指定范围。

### 求值代码
`eval` 子命令或 `-e` 别名帮助您即时求值 Scheme 代码。Shell 中建议用单引号包住 `CODE`，这样 Scheme 字符串里的双引号通常不用转义：
```
> gf eval '(+ 1 2)'
3
> gf -e '(+ 1 2)'
3
> gf eval '(begin (import (srfi srfi-1)) (first (list 1 2 3)))'
1
> gf eval '(begin (import (liii sys)) (display (argv)) (newline))' 1 2 3
("bin/gf" "eval" "(begin (import (liii sys)) (display (argv)) (newline))" "1" "2" "3")
```

### 加载文件
`load` 子命令帮助您加载 Scheme 文件并进入 REPL：
```
> gf load tests/liii/base/copy-test.scm
; 加载文件并进入 REPL
```

### 直接运行文件
您也可以直接加载并求值 Scheme 文件：
```
> gf tests/liii/base/copy-test.scm
```

### 模式选项
`-m` 或 `--mode` 帮助您指定标准库模式：

+ `default`: `-m default` 等价于 `-m r7rs`
+ `liii`: 预加载 `(liii base)`、`(liii error)` 和 `(liii string)` 的 Goldfish Scheme
+ `scheme`: 预加载 `(liii base)` 和 `(liii error)` 的 Goldfish Scheme
+ `sicp`: 预加载 `(scheme base)` 和 `(srfi sicp)` 的金鱼 Scheme
+ `r7rs`: 预加载 `(scheme base)` 的金鱼 Scheme

### 库搜索路径
Goldfish 启动时也支持额外的库搜索目录：

+ `-I DIR`：将 `DIR` 前置到库搜索路径
+ `-A DIR`：将 `DIR` 追加到库搜索路径

例如：
```bash
gf -I ~/.local/goldfish/example-lib eval '(begin (import (example hello)) (quote ok))'
```

启动时，Goldfish 还会自动把 `~/.local/goldfish/` 下所有名称匹配 `xxx-yyy` 且至少包含一个 `.scm` 文件的目录前置到库搜索路径中。

## 项目目标

- 在 Linux、macOS 和 Windows 上分发金鱼 Scheme 解释器和结构化 REPL。
- 实现 [R7RS-small](https://small.r7rs.org)。
- 以 R7RS 库格式提供有用的 SRFI。


## 许可证
金鱼Scheme 根据 Apache 2.0 许可证授权，一些源自 S7 Scheme 和 SRFI 的代码片段已在相关源文件中明确声明。

## 引用

读者可以用以下的BibTeX代码来引用我们的工作.

```
@book{goldfish,
    author = {Da Shen and Nian Liu and Yansong Li and Shuting Zhao and Shen Wei and Andy Yu and Siyu Xing and Jiayi Dong and Yancheng Li and Xinyi Yu and Zhiwen Fu and Duolei Wang and Leiyu He and Yingyao Zhou and Noctis Zhang},
    title = {Goldfish Scheme: A Scheme Interpreter with Python-Like Standard Library},
    publisher = {LIII NETWORK},
    year = {2024},
    url = {https://gitee.com/LiiiLabs/goldfish/releases/download/v17.10.9/Goldfish.pdf}
}
```
