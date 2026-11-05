# (liii goldsource) 模块函数索引

`(liii goldsource)` 是 `gf source` 子命令在 Scheme 层的总入口库：对外汇聚 `goldsource-args`、`goldsource-library`、`goldsource-cli` 子库功能，并提供 `main` 命令行入口。

使用时导入：

```scheme
(import (liii goldsource))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldsource "函数名"
```

## 函数分类索引

### 命令行入口
- `main` - 解析命令行参数并执行源码查看主流程，可显式传入参数列表供单元测试驱动

## 子库导览

| 子库 | 说明 |
|---|---|
| `(liii goldsource-args)` | 命令行参数与库查询字符串解析 |
| `(liii goldsource-library)` | 在当前 `*load-path*` 中定位库根目录与源码路径 |
| `(liii goldsource-cli)` | 执行命令行主流程与用法输出 |
