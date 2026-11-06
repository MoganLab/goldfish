# (liii signal) 模块函数分类索引

`(liii signal)` 提供 POSIX 风格的信号注册与发送能力，适合优雅退出、进程间通知与资源清理。

## 查看函数文档

```bash
gf doc liii/signal
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/signal "signal-trap"
```

## 函数分类索引

### 一、信号处理
- `signal-trap` - 为指定系统信号注册处理动作
- `signal-get` - 获取指定系统信号当前绑定的处理动作

### 二、信号发送
- `signal-raise` - 向当前进程主动发送指定系统信号
