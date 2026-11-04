# tools/json 测试目录导览

本目录存放 `gf json` 命令行工具的单元测试。

## 目录结构

| 目录/文件 | 说明 |
|---|---|
| `liii/goldjson/` | `(liii goldjson)` 库导览与 main、run-goldjson 单元测试 |
| `liii/goldjson/args/` | `(liii goldjson args)` 库导览与参数解析、帮助文本单元测试 |

## 运行方式

```bash
bin/gf test tools/json/
```

早期的 `cli-test.scm` 集成测试已按导出函数拆分到
`liii/goldjson/` 与 `liii/goldjson/args/` 下的单函数测试文件。
