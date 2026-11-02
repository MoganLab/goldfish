# (liii tui fuzzy) 模块函数索引

`(liii tui fuzzy)` 提供用于 TUI 交互式选择列表等的模糊搜索与排序过滤算法，支持词边界加权、连续匹配奖励、字母数字颠倒容错及多关键词交集搜索。

使用时导入：

```scheme
(import (liii tui fuzzy))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc fuzzy-match
```

## 函数分类索引

### 模糊匹配与过滤
- `fuzzy-match` - 计算查询与文本的模糊匹配度与分数
- `fuzzy-filter` - 按模糊匹配过滤并排序条目列表
