# (liii tui terminal poll) 模块函数索引

`(liii tui terminal poll)` 提供终端输入轮询与按键解析能力，包括非阻塞按键读取、ANSI 转义字节流解析、UTF-8 字节解码、鼠标 click 事件合成以及 bracketed paste 粘贴模式开关。

使用时导入：

```scheme
(import (liii tui terminal poll))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc terminal-poll-key
```

## 函数分类索引

### 按键轮询与粘贴模式
- `terminal-poll-key` - 非阻塞读取一个按键或鼠标事件
- `terminal-paste-mode?` - 查询粘贴模式开关状态
- `terminal-set-paste-mode!` - 开启或关闭粘贴模式
- `terminal-drain-mouse-events` - 一次性取走所有已到达的鼠标事件

### 字节流解析
- `%poll-console-key` - 从当前控制台非阻塞读取按键
- `%resolve-ansi-bytes` - 解析 ANSI 转义字节流
- `%resolve-utf8-bytes` - 解码 UTF-8 字节序列

### 鼠标事件合成
- `%poll-mouse-feed` - 注入字节流驱动 click 合成
- `%poll-mouse-reset!` - 清空 click 合成的 press 状态
