# (liii pi code all-in-one alarm) 模块函数分类索引

`(liii pi code all-in-one alarm)` 提供安全、原子的日志追加能力，把节点失败、切换、回溯与全链耗尽的异常记录写入按天日志 `logs/pi-YYYY-MM-DD.log`（默认为 `~/.gf/agent/logs/pi-YYYY-MM-DD.log`），时间戳使用机器本地时区，不单独维护 alarm.log。

使用时导入：

```scheme
(import (liii pi code all-in-one alarm))
```

## 查看函数文档

```bash
gf doc resolve-alarm-log-path
gf doc pi-alarm-log!
```

## 函数分类索引

- `resolve-alarm-log-path` - 解析当天日志文件的完整路径
- `pi-alarm-log!` - 向当天日志安全追加单行异常汇总记录
