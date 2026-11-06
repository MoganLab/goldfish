# (liii tui text-width core) 模块函数索引

`(liii tui text-width core)` 提供终端视觉列宽计算与 ANSI 转义序列处理的核心底层函数，支持 ASCII、CJK 全角字符、Emoji 与零宽字符的宽度计算，以及 ANSI CSI/OSC 序列匹配与剥除。

使用时导入：

```scheme
(import (liii tui text-width core))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc display-width
```

## 函数分类索引

### 视觉宽度计算
- `display-width` - 计算字符串在终端中的视觉显示列宽
- `display-width-prefix` - 计算字符串前 n 个字符占用的视觉列宽
- `%codepoint-width` - 计算单个 Unicode 码点在终端中的显示列宽

### ANSI 转义序列处理
- `strip-ansi` - 剔除字符串中的 ANSI 转义序列
- `%match-ansi` - 匹配 ANSI 转义序列（CSI、OSC、字符串序列及短 ESC 序列）
- `%match-ansi-csi` - 匹配 ANSI CSI 转义序列
- `%match-ansi-osc` - 匹配 ANSI OSC 转义序列
- `%is-sgr-reset?` - 判断字符串是否为 ANSI SGR 重置序列

`strip-ansi` 同时支持 RIS（`ESC c`）、键盘模式等短 ESC 序列，以及带私有参数的
SGR 鼠标报告。不完整 CSI/OSC 保留给后续流式输入补全。
