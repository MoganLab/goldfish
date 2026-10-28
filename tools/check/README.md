# gf check - Goldfish Scheme Static Code Metrics Checker

## Usage

```bash
gf check [options] [PATH...]
```

## Description

`gf check` 对 Scheme 源码执行只读静态度量检查，配合 `gf fmt` 的 80 列行宽限制，
在纵向行数与空间嵌套深度两个维度提供自动化质量防线。

## Rules (检查规则)

1. **max-file-lines**：单文件物理行数限制
   - 默认上限：单个 `.scm` 文件不得超过 400 行（计入空行与注释）
   - 引导开发者遵循单一职责原则（SRP），遏制上帝文件膨胀

2. **max-tree-depth**：语法树嵌套深度限制
   - 默认上限：单个顶级表达式的最大括号嵌套深度不得超过 15 层（基于 `(liii tree)` 的 `(tree-depth expr)` 度量）
   - 引导开发者提取辅助过程、使用 `(liii match)` 模式匹配、管道流扁平化深层嵌套，降低认知负荷

3. **no-glue**：底层胶水接口封装约束
   - `g_xxx` 只能出现在唯一一个 `define-library` 中，不得出现在测试、脚本等库外代码
   - 同一库可以多次使用该 glue，但其他库不得重复封装
   - `g_xxx` 可以没有对应的公开接口 `xxx`，但不得直接导出 `g_xxx`
   - 跨库唯一性基于全仓分析；`--changed-since` 仅过滤最终报告

4. **define-record-type**：记录类型命名与句法标识限制
   - `(define-record-type <t> ...)` 的类型名必须采用 `<xxx>` 形状，普通脚本及嵌套定义同样检查；裸标识、keyword、空名和缺失类型名均违规
   - R7RS/SRFI 9 中 `<t>` 仅是句法标识，不对应运行时绑定，不得出现在 `(export ...)` 中（含 `rename` 内外两个位置，转发导出同样违规）
   - 禁止 `(define <xxx> '<xxx>)` 占位定义
   - 构造器、谓词和字段访问器才是应导出的运行时绑定

## Options

- `-h, --help`: 显示帮助信息
- `--changed-since REV`: 仅检查自 git 提交 `REV` 以来发生变更的 `.scm` 文件
- `--only RULE`: 只执行指定规则，例如 `--only no-glue`

## Arguments

- `PATH...`: 指定要检查的一个或多个文件或目录（可选）
  - 若未指定 `PATH` 且未指定 `--changed-since`，则读取仓库根 `gf_check.json` 对全仓 `.scm` 文件进行检查

## Examples

```bash
# 全仓静态度量检查
gf check

# 仅检查自 main 分支以来变更的文件（适用于 pre-commit 与 CI 增量检查）
gf check --changed-since=main

# 检查指定单文件
gf check goldfish/liii/os.scm

# 只检查 glue 封装约束
gf check --only no-glue

# 递归检查指定目录
gf check goldfish/liii/
```

## Configuration (gf_check.json)

在仓库根目录可通过 `gf_check.json` 配置规则阈值与豁免白名单：

```json
{
  "goldfish_version": "18.11",
  "rules": {
    "max-file-lines": { "enabled": true, "limit": 400 },
    "max-tree-depth": { "enabled": true, "limit": 15 },
    "no-glue": { "enabled": true }
  },
  "exclude": [
    { "path": "goldfish/srfi/", "reason": "官方标准规范实现优先保证完整性与对照性" },
    { "path": "goldfish/liii/goldfmt/format.scm", "reason": "排版引擎核心算法不可分割" },
    { "path": "goldfish/liii/go.scm", "reason": "协程调度机核心算法不可分割" },
    { "path": "goldfish/liii/syntax-rules.scm", "reason": "宏展开器核心算法不可分割" }
  ]
}
```

- `exclude` 条目支持目录前缀（如 `goldfish/srfi/`）、通配符（如 `src/s7*`）以及携带 `path` 与 `reason` 的对象。

## Exit Codes

- `0`: 全部检查通过，无任何违规
- `1`: 存在规则违规项或配置错误
