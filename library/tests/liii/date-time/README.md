# (liii date-time) 模块函数分类索引

日期时间库：日期、钟表时间、时区、时间戳、时刻、时间差与解析格式化。

`(liii date-time)` 是聚合入口，重导出下列各子库的全部公开接口。
各子库亦可按需单独导入，如 `(liii date-time date)`、`(liii date-time time-delta)`。

## 常见用法示例

```scheme
(import (liii date-time))

(timestamp->iso-8601 (current-utc-timestamp)) ; => "2026-01-02T03:04:05Z"
(time-delta-minutes (milliseconds-delta 90000)) ; => 1
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/date-time "函数名"
```

## 函数地图（按子库分组）

### date — 历法日期

构造、字段读取、比较、ISO 周与 Rata Die / MJD 换算。

- `make-date` - 创建历法日期对象。
- `date?` - 判断给定对象是否为日期对象。
- `date-year` - 获取年份。
- `date-month` - 获取月份。
- `date-day` - 获取日。
- `date-ymd` - 以多值（values）形式返回年、月、日。
- `date->iso-8601` - 将日期对象转换为 ISO-8601 格式的字符串（YYYY-MM-DD）。
- `iso-8601->date` - 将符合 ISO-8601 格式的日期字符串解析为日期对象。
- `date-iso-weekday` - 获取 ISO 星期几（1-7，周一为 1，周日为 7）。
- `date-iso-week` - 获取日期所在的 ISO 周数（1-53）。
- `date-iso-week-year` - 获取日期所属的 ISO 周对应的年份。
- `date->rata-die` - 将日期转换为 Rata Die 日序数（公历 0001-01-01 为 Day 1）。
- `rata-die->date` - 将 Rata Die 日序数转换为公历日期对象。
- `date->mjd` - 将日期转换为简化儒略日（MJD）。
- `mjd->date` - 将简化儒略日（MJD）转换为公历日期对象。
- `date=?` - 比较两个或多个日期对象是否相等。
- `date<?` - 比较日期对象是否严格单调递增（小于）。
- `date<=?` - 比较日期对象是否单调递增（小于或等于）。
- `date>?` - 比较日期对象是否严格单调递减（大于）。
- `date>=?` - 比较日期对象是否单调递减（大于或等于）。

### gregorian — 公历算法

闰年、每月天数与"某月第 n 个星期几"等推算。

- `leap-year?` - 判断公历某年是否为闰年。
- `days-in-month` - 获取公历某年某月的天数。
- `day-name->weekday` - 将星期符号转换为 ISO 星期序号（1-7）。
- `first-in-month` - 获取某年某月中第一个指定星期几的日期号。
- `last-in-month` - 获取某年某月中最后一个指定星期几的日期号。
- `nth-in-month` - 获取某年某月中第 n 个指定星期几的日期号。

### clock-time — 钟表时间

一天内的时、分、秒，与日历无关。

- `make-clock-time` - 创建一天内的时钟时间对象。
- `clock-time?` - 判断给定对象是否为 clock-time 对象。
- `clock-time-hour` - 获取小时数。
- `clock-time-minute` - 获取分钟数。
- `clock-time-second` - 获取秒数（精确有理数或整数）。
- `clock-time-hms` - 以多值（values）形式返回时、分、秒。
- `clock-time->iso-8601` - 转换为 ISO-8601 时间字符串（如 "HH:MM:SS" 或 "HH:MM:SS.sss"）。
- `iso-8601->clock-time` - 将 ISO-8601 时间字符串解析为 clock-time 对象。
- `clock-time=?` - 比较两个或多个 clock-time 是否相等。
- `clock-time<?` - 比较 clock-time 是否严格单调递增（小于）。
- `clock-time<=?` - 比较 clock-time 是否单调递增（小于或等于）。
- `clock-time>?` - 比较 clock-time 是否严格单调递减（大于）。
- `clock-time>=?` - 比较 clock-time 是否单调递减（大于或等于）。

### moment — 时刻

历法日期加当日秒数，与时区无关的"日历 + 一天内位置"。

- `make-moment` - 由日期与当日秒数创建时刻对象。
- `moment?` - 判断给定对象是否为时刻对象。
- `moment-date` - 获取时刻的日期部分。
- `moment-second-of-day` - 获取时刻的当日秒数（精确有理数或整数）。
- `moment=?` - 比较两个时刻是否相等。
- `moment<?` - 比较时刻是否严格单调递增（小于）。
- `moment<=?` - 比较时刻是否单调递增（小于或等于）。
- `moment>?` - 比较时刻是否严格单调递减（大于）。
- `moment>=?` - 比较时刻是否单调递减（大于或等于）。

### time-delta — 时间差

年/月/周/日/时/分/秒各分量的增量，构造、读取与算术。

- `make-time-delta` - 创建一个时间增量（time-delta）对象。
- `time-delta?` - 判断给定对象是否为 time-delta 对象。
- `time-delta-years` - 获取年增量。
- `time-delta-months` - 获取月增量。
- `time-delta-weeks` - 获取周增量。
- `time-delta-days` - 获取日增量。
- `time-delta-hours` - 获取小时增量。
- `time-delta-minutes` - 获取分钟增量。
- `time-delta-seconds` - 获取秒增量（精确有理数或整数）。
- `years-delta` - 创建仅包含年份增量的 time-delta 对象。
- `months-delta` - 创建仅包含月份增量的 time-delta 对象。
- `weeks-delta` - 创建仅包含周数增量的 time-delta 对象。
- `days-delta` - 创建仅包含天数增量的 time-delta 对象。
- `hours-delta` - 创建仅包含小时增量的 time-delta 对象。
- `minutes-delta` - 创建仅包含分钟增量的 time-delta 对象。
- `seconds-delta` - 创建仅包含秒数增量的 time-delta 对象。
- `milliseconds-delta` - 从毫秒数创建 time-delta，进位到时/分/秒分量，亚秒余量保留为分数秒。
- `time-delta+` - 将多个 time-delta 对象的各分量对应相加。
- `time-delta-` - 计算 time-delta 的差值或取负。
- `time-delta-negate` - 将 time-delta 对象的所有分量取反。
- `time-delta=?` - 判断两个 time-delta 是否逐分量相等。
- `date+` - 将日期与时间增量相加。
- `date-` - 计算日期减去时间增量。

### timezone — 时区

时区对象构造与偏移、夏令时查询。

- `timezone?` - 判断给定对象是否为时区对象。
- `utc-timezone` - 返回 UTC 时区。
- `utc-offset-timezone` - 由固定 UTC 偏移构造时区。
- `system-timezone` - 返回系统本地时区。
- `tz-timezone` - 按 IANA 时区名（如 "Asia/Shanghai"）构造时区。
- `timezone-earliest` - 获取时区数据的最早覆盖时刻。
- `timezone-latest` - 获取时区数据的最晚覆盖时刻。
- `timezone-offset` - 查询时区在指定 UTC 时刻的偏移。
- `timezone-dst?` - 查询时区在指定 UTC 时刻是否处于夏令时（DST）。

### tzdb — 时区数据库

- `tz-timezones` - 列出可用的 IANA 时区名。
- `reload-timezone-data!` - 重新加载时区数据库。

### timestamp — 时间戳

历法日期 + 钟表时间 + 时区，对应绝对时间轴上的一点。

- `make-timestamp` - 由年月日时分秒与时区创建时间戳。
- `date+clock-time->timestamp` - 通过 date 与 clock-time 对象组合创建 timestamp。
- `timestamp?` - 判断给定对象是否为时间戳对象。
- `timestamp-date` - 获取日期部分。
- `timestamp-clock-time` - 获取钟表时间部分。
- `timestamp-timezone` - 获取时区部分。
- `timestamp-fold` - 将时间戳折叠为其组成的各字段。
- `timestamp-year` - 获取年份。
- `timestamp-month` - 获取月份。
- `timestamp-day` - 获取日。
- `timestamp-ymd` - 以多值（values）形式返回年、月、日。
- `timestamp-hour` - 获取小时。
- `timestamp-minute` - 获取分钟。
- `timestamp-second` - 获取秒。
- `timestamp-hms` - 以多值（values）形式返回时、分、秒。
- `moment->timestamp` - 将时刻按指定时区转换为时间戳。
- `timestamp->moment` - 将时间戳转换为时刻。
- `posix-time->utc-timestamp` - 由 POSIX 秒数创建 UTC 时间戳。
- `timestamp->utc-posix-time` - 将时间戳转换为 POSIX 秒数。
- `timestamp-in-timezone` - 将时间戳换算到指定时区。
- `timestamp-timezone-offset` - 获取时间戳所在时区的 UTC 偏移。
- `timestamp-timezone-dst?` - 判断时间戳所在时区是否处于夏令时。
- `timestamp-timezone-abbreviation` - 获取时区缩写（如 "CST"、"UTC"）。
- `timestamp->iso-8601` - 将时间戳转换为 ISO-8601 字符串。
- `iso-8601->timestamp` - 将 ISO-8601 字符串解析为时间戳。
- `timestamp+` - 时间戳加上时间增量。
- `timestamp-` - 时间戳减去时间增量。
- `timestamp=?` - 比较两个或多个时间戳在绝对时间上是否等价。
- `timestamp<?` - 比较时间戳是否严格单调递增（小于）。
- `timestamp<=?` - 比较时间戳是否单调递增（小于或等于）。
- `timestamp>?` - 比较时间戳是否严格单调递减（大于）。
- `timestamp>=?` - 比较时间戳是否单调递减（大于或等于）。

### current-time — 当前时间

- `current-moment` - 获取当前时刻（moment）。
- `current-utc-timestamp` - 获取当前 UTC 时间戳。
- `current-system-timestamp` - 获取当前系统本地时间戳。
- `current-monotonic-millis` - 获取自开机以来的单调毫秒数（精确整数）。
- `current-monotonic-seconds` - 获取自开机以来的单调秒数（精确整数）。

### format / parse — 格式化与解析

说明符与 Python `datetime.strftime` / `strptime` 对齐。

- `date-time-format` - 将日期时间对象按格式字符串格式化为字符串。
- `date-time-parse` - 将格式化的字符串解析为 timestamp 对象。

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 解析类函数在输入格式非法时抛出 `value-error` 或 `type-error`
