# (liii tui text-width sgr) 模块函数索引

`(liii tui text-width sgr)` 提供 SGR 选择性复位能力：跟踪一串 SGR 序列中实际生效的样式（前景、背景、粗体、下划线等），生成只复位这些样式的选择性复位序列，绝不发 `\x1b[0m` 全重置，避免清掉调用方在行外设置的背景等样式（issue #1484）。

使用时导入：

```scheme
(import (liii tui text-width sgr))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc %sgr-selective-reset
```

## 函数分类索引

### SGR 选择性复位
- `%sgr-selective-reset` - 根据行内累积的 SGR 序列生成只复位生效样式的选择性复位串

支持 `38;5;n` / `38;2;r;g;b` 扩展色与多参数 SGR（如 `\x1b[1;31m`）；
复位参数按升序合并为一条 SGR；全重置（`\x1b[0m`）清空全部状态；
非 SGR 的 CSI/OSC 序列与未收录参数不参与跟踪。
