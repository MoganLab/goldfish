# (liii pi ai models search) 模块函数索引

`(liii pi ai models search)` 提供模型搜索与过滤的底层文本匹配功能，支持以空格或斜杠分词的子序列模糊匹配。

使用时导入：

```scheme
(import (liii pi ai models search))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-fuzzy-match-model?
```

## 函数分类索引

### 搜索与匹配
- `pi-split-search-tokens` - 将搜索字符串切分为 token 列表
- `pi-fuzzy-match-model?` - 判断搜索词是否与模型描述文本进行子序列模糊匹配
