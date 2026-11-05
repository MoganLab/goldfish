# tools/fix 测试目录导览

本目录存放 `gf fix` 命令行工具的单元测试。

## 目录结构

| 目录/文件 | 说明 |
|---|---|
| `liii/goldfix/` | `(liii goldfix)` 库导览与 main、repair-source、repair-source\* 单元测试 |
| `liii/goldfix/record/` | `(liii goldfix record)` 记录类型的单元测试 |
| `liii/goldfix/scan/` | `(liii goldfix scan)` 词法扫描函数的单元测试 |
| `liii/goldfix/tokenize/` | `(liii goldfix tokenize)` 分词函数的单元测试 |
| `liii/goldfix/edit/` | `(liii goldfix edit)` 编辑应用函数的单元测试 |
| `liii/goldfix/repair/` | `(liii goldfix repair)` 括号修正函数的单元测试 |
| `liii/goldfix/cache/` | `(liii goldfix cache)` 缓存函数的单元测试 |
| `liii/goldfix/cli/` | `(liii goldfix cli)` 命令行解析函数的单元测试 |
| `liii/goldfix/batch/` | `(liii goldfix batch)` 批处理函数的单元测试 |

## 运行方式

```bash
bin/gf test tools/fix/
```

早期的 `goldfix-test.scm` 集成测试已按导出函数拆分到 `liii/goldfix/` 下的单函数测试文件。
