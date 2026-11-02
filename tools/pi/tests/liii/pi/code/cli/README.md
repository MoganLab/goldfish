# (liii pi code cli) 模块函数分类索引

`(liii pi code cli)` 提供了 Pi Coding Agent 的命令行主入口、参数解析与调度执行功能。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/pi/code/cli "函数名"
```

## 函数分类索引

### 一、命令行入口与调度
- `main` - Pi CLI 命令行工具的主入口过程
- `run-pi` - 剥离命令行中的 gf/pi 前缀，加载扩展并执行 Pi 命令
- `run-goldpi` - 运行 Goldfish Scheme Pi CLI 入口，run-pi 的别名

### 二、辅助与交互输入
- `display-pi-help` - 输出 Pi 命令行工具的使用帮助信息
- `read-all-stdin` - 从标准输入读取全部内容并修剪两端空白字符
