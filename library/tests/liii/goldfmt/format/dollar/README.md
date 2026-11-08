# (liii goldfmt format dollar) 模块函数索引

`(liii goldfmt format dollar)` 提供 `(liii lang)` 中 `$` 语法糖链式管道的多行排版游走实现：将算子行与首行数据源对齐，并保持一行一个算子。

使用时导入：

```scheme
(import (liii goldfmt format dollar))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/format/dollar "函数名"
```

## 函数分类索引

### 管道游走
- `walk-env-dollar!` - 将跨行 $ 管道节点树写入 writer 并返回带定位信息的新树
