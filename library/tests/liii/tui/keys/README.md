# (liii tui keys) 模块函数索引

`(liii tui keys)` 提供 TUI 应用的按键输入解析，将终端按键字节流
解析为按键名，支持 ASCII 字符、UTF-8 多字节字符、控制字符以及
包括 kitty CSI-u / xterm modifyOtherKeys 增强键盘协议在内的转义序列。

使用时导入：

```scheme
(import (liii tui keys))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc parse-key
```

## 函数分类索引

### 按键解析
- `parse-key` - 解析单次按键输入，返回按键名
- `parse-keys` - 解析连续按键流，返回按键名列表
