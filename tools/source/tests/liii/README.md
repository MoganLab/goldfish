# tools/source 测试目录导览

本目录存放 `gf source` 命令行工具的单元测试。

## 目录结构

| 目录/文件 | 说明 |
|---|---|
| `liii/goldsource/` | `(liii goldsource)` 库导览与 main 单元测试 |
| `liii/goldsource-args/` | `(liii goldsource-args)` 参数与库查询解析的单元测试 |
| `liii/goldsource-library/` | `(liii goldsource-library)` 源码路径定位的单元测试 |
| `liii/goldsource-cli/` | `(liii goldsource-cli)` 命令行主流程执行的单元测试 |

## 运行方式

```bash
bin/gf test tools/source/
```
