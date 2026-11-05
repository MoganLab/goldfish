# Goldfish 仓库工作指南

## 第一节：命令行使用

以下命令均在仓库根目录执行，命令行入口为 `bin/gf`：

```bash
xmake b goldfish            # 构建 Goldfish
bin/gf help                 # 查看全部命令
bin/gf repl                 # 启动 REPL
bin/gf eval '(+ 1 2)'       # 求值 Scheme 代码
bin/gf path/to/file.scm     # 执行 Scheme 文件
```

### 运行单元测试：`test`

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

### 运行格式化：`fmt`

```bash
bin/gf fmt                         # 格式化仓库中配置的语言代码
bin/gf fmt goldfish/liii/os.scm    # 格式化单个文件
bin/gf fmt goldfish/               # 递归格式化目录
bin/gf fmt --check                 # 只检查格式，不修改文件
bin/gf fmt --dry-run file.scm      # 预览单个文件的格式化结果
```

### 运行自动修复：`fix`

`fix` 根据缩进修正 Scheme 代码的括号，使用前应保证缩进正确，建议先运行 `fmt`。

```bash
bin/gf fix file.scm                # 修复单个文件
bin/gf fix goldfish/               # 递归修复目录
bin/gf fix --dry-run file.scm      # 预览修复结果，不修改文件
```

自动修复后必须检查差异并重新运行相关测试。

### 查看文档：`doc`

`doc` 用于查看标准库概览、函数文档和测试中的使用示例：

```bash
bin/gf doc liii/path                  # 查看库文档
bin/gf doc liii/path "path-read-text" # 查看指定库中的函数文档
bin/gf doc "string-split"             # 在所有库中查找函数
bin/gf doc "string-spli"              # 支持模糊匹配函数名
bin/gf doc --build-json               # 重建函数索引
```

函数文档来源于 `tests/` 目录下的 `*-test.scm` 文件。新增或修改测试文档后，运行 `bin/gf doc --build-json` 更新索引。

### 静态检查：`check`

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

## 第二节：三大板块

### 三个层次

仓库正在按“语言、标准库、工具”三个层次重构。这是一项正在进行中的任务：以下描述的是目标架构，现有代码尚未完成迁移。后续修改应持续遵循这一方向，逐步收敛到目标结构。

| 层次 | 目录 | 职责 |
| --- | --- | --- |
| 语言 | `goldfish/` | Goldfish Scheme 的语言本体、核心语义、运行时和启动必需的最小能力 |
| 标准库 | `library/`（规划新增） | Goldfish Scheme 官方标准库，包括 `(scheme ...)`、`(srfi ...)` 和 `(liii ...)` |
| 工具 | `tools/pi/` | Pi Coding Agent 的命令行、交互、Agent 工作流和产品策略 |

### 依赖规则

依赖只能向下，工具也可以直接依赖语言：

```text
tools/pi/ → library/ → goldfish/
    └────────────────→ goldfish/
```

- `goldfish/` 不依赖 `library/` 或 `tools/`。
- `library/` 可以依赖 `goldfish/`，但不依赖工具，库之间不得循环依赖。
- `tools/pi/` 可以依赖语言和标准库，其他层不得依赖工具。

### 单元测试位置

各层的单元测试放在对应代码目录下的 `tests/` 中，测试路径尽量与被测库的 import 路径一致：

| 层次 | 单元测试目录 | 示例 |
| --- | --- | --- |
| 语言 | `goldfish/tests/` | `goldfish/tests/scheme/base-test.scm` |
| 标准库 | `library/tests/` | `library/tests/liii/list-test.scm` |
| 工具 | `tools/pi/tests/` | `tools/pi/tests/liii/pi/agent-test.scm` |

现有语言测试主要存放在仓库根目录的 `tests/`，这一布局不再作为新增测试的目标。后续应参照 `tools/pi/tests/` 的组织方式，将语言测试逐步迁移到 `goldfish/tests/`；迁移模块时同步迁移对应测试。

### 代码放置与演进

- 新代码按职责归入对应层次；同时包含标准库和工具逻辑时，先拆分职责。
- `library/` 只放正式纳入标准库的能力，工具内部的公共代码仍留在工具层。
- 目录与 Scheme 的 import 库名解耦；迁移标准库时不改变公开库名和行为。
- 当前重点是逐步将 `goldfish/` 和 `tools/pi/` 中属于标准库职责的代码迁移到 `library/`。
- 迁移应小步进行，不批量移动无关代码；每次迁移配套回归测试。
- 测试、构建配置和基础设施服务于上述层次，不单独构成第四层。
