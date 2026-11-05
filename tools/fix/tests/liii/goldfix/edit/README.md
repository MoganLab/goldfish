# (liii goldfix edit) 模块函数索引

`(liii goldfix edit)` 提供括号修正的编辑（edit）应用能力：把插入（insert）与删除（delete）两类编辑作用到原始源码文本上，得到修正后的源码。

使用时导入：

```scheme
(import (liii goldfix edit))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix/edit "函数名"
```

## 函数分类索引

### 编辑应用
- `apply-edits` - 将一组 edit 应用于原始 source，返回修正后的源码文本
