# (liii goldfmt) 模块函数分类索引

`(liii goldfmt)` 提供 Scheme 源码格式化能力，按子模块拆分：扫描、格式化、排版。

## 查看函数文档

```bash
gf doc liii/goldfmt
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/goldfmt "format-scheme-string"
```

## 函数分类索引

### 一、统一入口
- `format-scheme-string` - format-string 的别名入口

其余导出为子模块接口，按函数拆分的单元测试见对应子模块目录：

- `(liii goldfmt scan)` - `tests/liii/goldfmt/scan/`（`scan-string`、`can-inline?` 等）
- `(liii goldfmt format)` - `tests/liii/goldfmt/format/`（`format-string`、`format-datum`、`format-node`、`format-nodes` 等）
- `(liii goldfmt stem)` - `tests/liii/goldfmt/stem/`（`format-stem-string` 等）
- `(liii goldfmt record)` - `tests/liii/goldfmt/record/`
- `(liii goldfmt rule)` - `tests/liii/goldfmt/rule/`
- `(liii goldfmt tokenize)` - `tests/liii/goldfmt/tokenize/`
