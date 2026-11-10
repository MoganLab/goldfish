# (liii diff hunk) 模块函数分类索引

`(liii diff hunk)` 负责把差异操作码格式化为标准统一差异（Unified Diff）的 hunk 头与差异行。

## 查看函数文档

```bash
gf doc liii/diff/hunk
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/diff/hunk "make-diff-header"
```

## 函数分类索引

### 一、范围格式化
- `format-range-unified` - 把行号区间格式化为统一差异的范围字符串

### 二、hunk 行生成
- `format-group` - 把一组差异操作码格式化为 hunk 行列表
- `format-all-groups` - 把多组差异操作码统一格式化为完整 hunk 行列表

### 三、文件头
- `make-diff-header` - 生成统一差异头部的原文件行与新文件行
