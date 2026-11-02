# (liii tui keys parser) 模块函数索引

`(liii tui keys parser)` 提供终端按键输入的解析函数，将按键
字节流解析为按键名，支持 ASCII 字符、UTF-8 多字节字符、控制
字符以及包括 kitty CSI-u / xterm modifyOtherKeys 增强键盘协议
在内的转义序列。按键码表查询由 `(liii tui keys table)` 提供。

使用时导入：

```scheme
(import (liii tui keys parser))
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
