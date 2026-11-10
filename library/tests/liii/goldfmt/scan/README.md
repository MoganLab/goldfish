# (liii goldfmt scan) 模块函数索引

`(liii goldfmt scan)` 提供 goldfmt 的源码扫描入口：把一段源码文本或一个源文件转换为 formatter 使用的 node 树（env/atom），并在扫描前完成 reader 字面量的预处理。

使用时导入：

```scheme
(import (liii goldfmt scan))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/scan "函数名"
```

## 函数分类索引

### 源码扫描
- `scan-string` - 扫描一段 Scheme 源码文本，返回顶层表达式对应的 node 树
- `scan-source-string` - 按原始源码扫描文本，保留注释与空行信息
- `scan-file` - 读取文件内容并扫描成 formatter 使用的 node 树

### stem 模式
- `stem-mode?` - 判断当前是否处于 stem 模式
- `call-with-stem-mode` - 在 stem 模式下执行指定的 thunk
