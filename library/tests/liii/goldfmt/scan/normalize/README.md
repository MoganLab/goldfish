# (liii goldfmt scan normalize) 模块函数索引

`(liii goldfmt scan normalize)` 提供 goldfmt 扫描阶段的 datum 归一化与节点树构建能力：把 `read` 读出的原始 datum 归一化（还原 s7 内部形式、转换 reader 字面量），再转换为 formatter 使用的 env/atom 节点树。

使用时导入：

```scheme
(import (liii goldfmt scan normalize))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/scan/normalize "函数名"
```

## 函数分类索引

### 归一化
- `normalize-datum` - 将 read 读出的原始 datum 归一化
- `split-pair-list` - 将一个列表拆分为前缀元素列表与尾部

### 节点树构建
- `scan` - 将 Scheme datum 扫描成 formatter 使用的 node 树
- `scan-datum` - 将归一化后的 datum 扫描为 node（atom 或 env）

### stem 模式
- `stem-mode?` - 判断当前是否处于 stem 模式
- `call-with-stem-mode` - 在 stem 模式下执行指定的 thunk

### 输出工具
- `write-to-string` - 使用 write 语义将值转为字符串
