# (liii pi ai time) 模块函数索引

`(liii pi ai time)` 提供 AI 交互和事件序列化所需的时间戳与时钟函数，支持 ISO 8601 UTC 字符串生成、Unix epoch 秒/毫秒墙钟以及抗 NTP 回拨的单调时钟。

使用时导入：

```scheme
(import (liii pi ai time))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-iso-8601-utc-now
```

## 函数分类索引

### 墙钟时间
- `pi-iso-8601-utc-now` - 获取当前 UTC 时间的 ISO 8601 字符串
- `pi-current-time-seconds` - 获取当前 Unix 整秒时间戳
- `pi-current-time-millis` - 获取当前 Unix 毫秒时间戳

### 单调时钟
- `pi-current-monotonic-seconds` - 获取系统自开机以来的单调秒数
- `pi-current-monotonic-millis` - 获取系统自开机以来的单调毫秒数
