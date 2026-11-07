# (liii date-time) 模块函数分类索引

日期时间库：日期、钟表时间、时区、时间戳、时刻、时间差与解析格式化。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 子模块索引

- `date` - 日期构造与公历转换（`make-date`、`date?`、`date->iso-8601`）
- `clock-time` - 钟表时间（`make-clock-time`、`clock-time->iso-8601`）
- `time-delta` - 时间差（`hours-delta`、`timestamp+`）
- `moment` - 时刻（`timestamp->moment`、`moment->timestamp`）
- `current-time` - 当前时间（`current-timestamp`、`now`）
- `parse` - 解析（`iso-8601->timestamp`）
- `format` - 格式化（`timestamp->iso-8601`）
- `gregorian` - 公历算法（`gregorian->days`、`days->gregorian`）
- `internal` - 内部工具

总聚合入口测试 `date-time-test.scm` 跨子库验证导出接口协同。
