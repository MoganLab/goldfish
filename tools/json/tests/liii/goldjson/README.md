# (liii goldjson) 模块导览

`(liii goldjson)` 是 `gf json` 命令行工具的实现库，提供 JSON 文本的
校验、格式化、排序、紧凑化与 JSON Lines 逐行处理。

命令行入口为 `tools/json/main.scm` 的 `(exit (main))`。

使用时导入（load-path 需包含 `tools/json`）：

```scheme
(import (liii goldjson))
```

## 查看函数文档

```bash
gf doc liii/goldjson "run-goldjson"
```

## 函数分类索引

### 一、入口与编排

| 函数 | 说明 |
|---|---|
| `main` | 子命令入口，从命令行或显式参数提取参数执行主流程 |
| `run-goldjson` | 解析参数并完成 JSON 校验、格式化或转换 |

### 二、参数解析与帮助

由子模块 `(liii goldjson args)` 提供，并经本模块再导出：

| 函数 | 说明 |
|---|---|
| `parse-json-args` | 解析命令行参数为选项关联列表 |
| `display-json-help` | 输出帮助文本 |

子模块详见 `liii/goldjson/args/README.md`。
