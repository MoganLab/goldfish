# (liii pi code utils shell) 模块函数索引

`(liii pi code utils shell)` 提供终端 shell 命令输出与显示文本的安全清理功能，防止二进制垃圾、C0 控制字符、损坏的 UTF-8 字节以及不可信的 ANSI 转义序列导致终端显示错乱、崩溃或终端注入风险。

使用时导入：

```scheme
(import (liii pi code utils shell))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc sanitize-binary-output
gf doc sanitize-display-text
```

## 函数分类索引

### 文本安全清理
- `sanitize-binary-output` - 清理命令输出中的控制字符、特殊标注字符并容错修复非法 UTF-8
- `sanitize-display-text` - 剥除 ANSI 转义序列并清理不可信显示文本中的二进制与控制字符
