# (liii tui text-width truncate) 模块函数索引

`(liii tui text-width truncate)` 提供按终端视觉显示宽度截断单行文本的功能，支持自定义省略号，保证截断时不劈裂 CJK 双宽字符，并在截断包含 ANSI 样式的文本时自动闭合转义序列。

使用时导入：

```scheme
(import (liii tui text-width truncate))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc truncate-to-width
```

## 函数分类索引

### 文本截断
- `truncate-to-width` - 按终端视觉列宽截断单行文本，可选自定义省略符号
