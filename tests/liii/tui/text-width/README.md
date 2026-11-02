# (liii tui text-width) 模块函数索引

`(liii tui text-width)` 统一导出终端文本宽度计算、文本截断与折行相关函数，聚合自 `(liii tui text-width core)`、`(liii tui text-width truncate)` 和 `(liii tui text-width wrap)` 子模块。

使用时导入：

```scheme
(import (liii tui text-width))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc display-width
```

## 函数分类索引

### 视觉宽度计算与 ANSI 处理
- `strip-ansi` - 剔除字符串中的 ANSI 转义序列
- `display-width` - 计算字符串在终端中的视觉显示列宽
- `display-width-prefix` - 计算字符串前 n 个字符占用的视觉列宽

### 文本截断
- `truncate-to-width` - 按终端视觉列宽截断单行文本，可选自定义省略符号

### 文本折行
- `wrap-text` - 按终端视觉显示宽度对文本进行安全折行，返回行字符串列表
