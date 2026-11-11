# (liii pi code all-in-one alarm) 模块函数分类索引

`(liii pi code all-in-one alarm)` 提供安全、原子的日志追加能力，专职向配置目录下的 `logs/alarm.log`（默认为 `~/.gf/agent/logs/alarm.log`），时间戳使用机器本地时区写入节点失败、切换、回溯与全链耗尽的异常记录。

使用时导入：

```scheme
(import (liii pi code all-in-one alarm))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc resolve-alarm-log-path
gf doc pi-alarm-log!
```

## 函数分类索引

- `resolve-alarm-log-path` - 解析 alarm.log 文件的完整路径
- `pi-alarm-log!` - 向 alarm.log 安全追加单行异常汇总记录
