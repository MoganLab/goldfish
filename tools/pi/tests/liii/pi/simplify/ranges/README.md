# (liii pi simplify ranges) 模块函数分类索引

`(liii pi simplify ranges)` 提供代码简化目标文件的改动行范围收集与解析能力。

使用时导入：

```scheme
(import (liii pi simplify ranges))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-diff-ranges-command
gf doc pi-parse-changed-ranges
gf doc pi-collect-simplify-files
```

## 函数分类索引

### 改动范围与文件收集
- `pi-diff-ranges-command` - 构建获取单个文件行改动的 git diff 命令行字符串
- `pi-parse-changed-ranges` - 解析 git diff 的 hunk 头部并合并相邻行号区间
- `pi-collect-simplify-files` - 收集待简化的文件信息，包括改动行号区间与截断内容
