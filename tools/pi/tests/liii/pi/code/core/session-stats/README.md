# (liii pi code core session-stats) 模块函数索引

`(liii pi code core session-stats)` 负责会话统计分析：从持久化 entries 或运行时消息中聚合消息数量、Token 传输与读写缓存统计、计算单轮缓存命中率，并提供格式化输出。

使用时导入：

```scheme
(import (liii pi code core session-stats))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc <函数名>
```

## 函数分类索引

### 会话统计
- `make-pi-session-stats` - 创建会话统计 record
- `pi-session-stats?` - 判断是否为会话统计 record
- `pi-session-stats-session-file` - 获取会话文件路径
- `pi-session-stats-session-id` - 获取会话 ID
- `pi-session-stats-session-name` - 获取会话名称
- `pi-session-stats-total-messages` - 获取总消息数
- `pi-session-stats-user-messages` - 获取用户消息数
- `pi-session-stats-assistant-messages` - 获取助理消息数
- `pi-session-stats-tool-calls` - 获取工具调用数
- `pi-session-stats-tool-results` - 获取工具返回数
- `pi-session-stats-tokens` - 获取 Token 统计 record
- `pi-session-stats-cost` - 获取会话总费用
- `pi-get-session-stats` - 从会话文件读取并计算完整统计
- `pi-format-session-stats` - 格式化会话统计为纯文本报告

### Token 与缓存统计
- `make-pi-session-token-stats` - 创建 Token 统计 record
- `pi-session-token-stats?` - 判断是否为 Token 统计 record
- `pi-session-token-stats-input` - 获取未缓存输入 Token 数
- `pi-session-token-stats-output` - 获取输出 Token 数
- `pi-session-token-stats-cache-read` - 获取读缓存 Token 数
- `pi-session-token-stats-cache-write` - 获取写缓存 Token 数
- `pi-session-token-stats-total` - 计算 Token 总数
- `pi-format-token-count` - 将 Token 数量格式化为紧凑字符串（k/M 缩写）
- `pi-calculate-cache-hit-rate` - 计算缓存命中率百分比
- `pi-format-token-stats` - 构建紧凑 Token 传输与缓存统计行（含 CHxx.x%）
