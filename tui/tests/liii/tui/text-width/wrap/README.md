# (liii tui text-width wrap) 模块函数索引

`(liii tui text-width wrap)` 提供按终端视觉显示宽度对文本进行安全折行的功能，支持保留换行、按单词折行（word wrap）、CJK 双宽字符安全换行以及 ANSI 转义序列样式保持。

使用时导入：

```scheme
(import (liii tui text-width wrap))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc wrap-text
```

## 函数分类索引

### 文本折行
- `wrap-text` - 按终端视觉显示宽度对文本进行安全折行，返回行字符串列表
