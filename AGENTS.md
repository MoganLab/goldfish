# Goldfish 仓库工作指南

## 1. 命令行使用

以下命令均在仓库根目录执行，命令行入口为 `bin/gf`：

```bash
xmake b goldfish            # 构建 Goldfish
bin/gf help                 # 查看全部命令
bin/gf repl                 # 启动 REPL
bin/gf eval '(+ 1 2)'       # 求值 Scheme 代码
bin/gf path/to/file.scm     # 执行 Scheme 文件
```

### 1.1 运行单元测试：`test`

`test` 是开发过程中最重要的子命令，用于运行文件名以 `-test.scm` 结尾的单元测试。优先运行与本次修改直接相关的测试，完成后再扩大测试范围。

```bash
bin/gf test tests/liii/string-test.scm  # 运行指定测试文件
bin/gf test tools/pi/tests/liii/pi/     # 运行指定目录下的测试
bin/gf test string                     # 运行路径中包含 string 的测试
bin/gf test --changed-since=main       # 运行相对 main 发生变更的测试
bin/gf test --all                      # 运行全部测试
```

工具层的测试必须带上对应的工具目录，例如 `bin/gf test tools/pi/tests/`，以便命令设置正确的库搜索路径。

不带参数执行 `bin/gf test` 时会根据当前分支选择测试范围；测试失败时命令返回非零退出码。

### 1.2 运行格式化：`fmt`

```bash
bin/gf fmt                         # 格式化仓库中配置的语言代码
bin/gf fmt goldfish/liii/os.scm    # 格式化单个文件
bin/gf fmt goldfish/               # 递归格式化目录
bin/gf fmt --check                 # 只检查格式，不修改文件
bin/gf fmt --dry-run file.scm      # 预览单个文件的格式化结果
```

### 1.3 运行自动修复：`fix`

`fix` 根据缩进修正 Scheme 代码的括号，使用前应保证缩进正确，建议先运行 `fmt`。

```bash
bin/gf fix file.scm                # 修复单个文件
bin/gf fix goldfish/               # 递归修复目录
bin/gf fix --dry-run file.scm      # 预览修复结果，不修改文件
```

自动修复后必须检查差异并重新运行相关测试。

### 1.4 查看文档：`doc`

`doc` 用于查看标准库概览、函数文档和测试中的使用示例：

```bash
bin/gf doc liii/path                  # 查看库文档
bin/gf doc liii/path "path-read-text" # 查看指定库中的函数文档
bin/gf doc "string-split"             # 在所有库中查找函数
bin/gf doc "string-spli"              # 支持模糊匹配函数名
bin/gf doc --build-json               # 重建函数索引
```

函数文档来源于 `tests/` 目录下的 `*-test.scm` 文件。新增或修改测试文档后，运行 `bin/gf doc --build-json` 更新索引。

索引按加载根目录分别写入其关联的 `tests` 根目录：语言层写入 `tests/function-library-index.json`，
标准库写入 `library/tests/function-library-index.json`，工具写入各自的 `tools/<工具>/tests/function-library-index.json`。
`--build-json` 会扫描每个加载根目录下的库源码目录（跳过 `tests/` 目录），把各加载根目录的导出记录合并后写出，
因此打包发布前需要执行一次该命令，索引文件会随 `tests/**`、`library/**`、`tools/**` 一并安装。

### 1.5 静态检查：`check`

`check` 对 Scheme 代码执行只读静态检查，包括文件行数、语法树嵌套深度、glue 封装、记录类型定义和单元测试规范。检查失败时返回非零退出码。

```bash
bin/gf check                         # 按配置检查全仓代码
bin/gf check goldfish/liii/os.scm    # 检查指定文件
bin/gf check goldfish/liii/          # 递归检查指定目录
bin/gf check --changed-since=main    # 检查相对 main 发生变更的代码
bin/gf check --only no-glue          # 只运行指定规则
```

规则和豁免配置位于仓库根目录的 `gf_check.json`。

可通过 `bin/gf help <command>` 查看各子命令的详细用法。

## 2. 仓库板块

### 2.1 四个板块

仓库正在按“语言、R7RS、标准库、工具”四个板块重构。这是一项正在进行中的任务：以下描述的是目标架构，现有代码尚未完成迁移。后续修改应持续遵循这一方向，逐步收敛到目标结构。

| 板块 | 目录 | 职责 |
| --- | --- | --- |
| R7RS | `r7rs/`（规划新增） | 维护 Scheme 语言标准 R7RS-small 所定义的部分：`(scheme ...)` 系列库与 R7RS 一致性测试 |
| 语言 | `goldfish/` | Goldfish Scheme 的语言本体、核心语义、运行时和启动必需的最小能力，以及语言增强：SRFI、数据结构与算法、语法与文本原语 |
| 标准库 | `library/` | Goldfish Scheme 官方标准库，即 `(liii ...)` 库：I/O、系统、网络、格式化与工具类能力 |
| 工具 | `tools/pi/` | Pi Coding Agent 的命令行、交互、Agent 工作流和产品策略 |

### 2.2 R7RS 板块

`r7rs/` 是最基础的板块，维护 **R7RS-small** 所定义的部分。R7RS-small 是 Scheme 语言的标准，因此本板块承载的是 `(scheme ...)` 系列库的实现，以及固化 R7RS-small 行为的一致性测试。

R7RS-small 规范定义的库及其在仓库中的对应关系：

| 库 | 职责 | 现状 |
| --- | --- | --- |
| `(scheme base)` | 基础库：绑定、谓词、数值、字符、列表、向量、字符串、输入输出、异常 | 已实现 |
| `(scheme case-lambda)` | `case-lambda` 变参过程构造 | 已实现 |
| `(scheme char)` | 字符分类与转换 | 已实现 |
| `(scheme complex)` | 复数构造与运算 | 已实现 |
| `(scheme cxr)` | `caar` … `cdddr` 组合子 | 已实现 |
| `(scheme eval)` | `eval` 与 `environment` | 已实现 |
| `(scheme file)` | 文件输入输出 | 已实现 |
| `(scheme inexact)` | 精确/非精确数值转换 | 已实现 |
| `(scheme lazy)` | `delay` / `delay-force` / `force` / `make-promise` / `promise?` | 已实现 |
| `(scheme load)` | `load` | 已实现 |
| `(scheme process-context)` | `command-line` / `exit` / `get-environment-variable` | 已实现 |
| `(scheme read)` | `read` | 已实现 |
| `(scheme repl)` | `interaction-environment` | 已实现 |
| `(scheme r5rs)` | R5RS 兼容层：`exact->inexact` / `inexact->exact` / `null-environment` / `scheme-report-environment` 等 | 待实现 |
| `(scheme time)` | `current-second` / `current-jiffy` / `jiffies-per-second` | 已实现 |
| `(scheme write)` | `write` / `write-shared` / `write-simple` | 已实现 |

板块划分规则：

- **以 R7RS-small 文本为唯一规范来源**，实现不得擅自扩大或缩小语义；超出 small 范围的扩展一律归入 `(liii ...)` 库。R7RS-small 之外的可选库（`(scheme mutable-pairs)` 等 R6RS 遗留库）不属于本板块。
- R7RS-small 附录 A 定义的库共 16 个，其中 `(scheme base)` 必须完整提供；其余库可以选择不提供，但一旦提供就必须完整且不得额外导出。
- 对规范的行为偏差与历史兼容处理，必须在 `r7rs/tests/` 中以一致性测试固化，并在源码注释中说明依据。
- 本板块只承载标准定义的库，不放置 Goldfish 私有扩展。
- `r7rs/scheme/boot.scm`（自举入口与环境初始化）与 `r7rs/scheme/syntax-rules.scm`（卫生宏展开器）不是 R7RS-small 定义的库，而是 `(scheme ...)` 系列库赖以构建的底座：它们只依赖 s7 内建原语，不反向依赖 `goldfish/`，因此随本板块一起置于 `r7rs/`。

### 2.3 依赖规则

依赖只能向下，`r7rs/` 是最底层，不依赖任何其它板块；工具也可以直接依赖语言：

```text
tools/ → library/ → goldfish/ → r7rs/
    └────────────────→ goldfish/
```

- `r7rs/` 不依赖 `goldfish/`、`library/` 或 `tools/`。
- `goldfish/` 可以依赖 `r7rs/`，但不依赖 `library/` 或 `tools/`。
- `library/` 可以依赖 `goldfish/` 和 `r7rs/`，但不依赖工具，库之间不得循环依赖。
- `tools/` 可以依赖语言、R7RS 和标准库，其他层不得依赖工具。

### 2.4 单元测试位置

各板块的单元测试放在对应代码目录下的 `tests/` 中，测试路径尽量与被测库的 import 路径一致：

| 板块 | 单元测试目录 | 示例 |
| --- | --- | --- |
| R7RS | `r7rs/tests/` | `r7rs/tests/scheme/base-test.scm` |
| 语言 | `goldfish/tests/` | `goldfish/tests/liii/list-test.scm` |
| 标准库 | `library/tests/` | `library/tests/liii/list-test.scm` |
| 工具 | `tools/pi/tests/` | `tools/pi/tests/liii/pi/agent-test.scm` |

现有语言测试主要存放在仓库根目录的 `tests/`，这一布局不再作为新增测试的目标。后续应参照 `tools/pi/tests/` 的组织方式，将语言测试逐步迁移到 `goldfish/tests/`；迁移模块时同步迁移对应测试。

### 2.4 代码放置与演进

- 新代码按职责归入对应板块；同时包含标准库和工具逻辑时，先拆分职责。
- `library/` 只放正式纳入标准库的能力，工具内部的公共代码仍留在工具层。
- 目录与 Scheme 的 import 库名解耦；迁移标准库时不改变公开库名和行为。
- 当前重点是逐步将 `goldfish/` 和 `tools/pi/` 中属于标准库职责的代码迁移到 `library/`。
- 迁移应小步进行，不批量移动无关代码；每次迁移配套回归测试。
- 测试、构建配置和基础设施服务于上述板块，不单独构成第五层。
