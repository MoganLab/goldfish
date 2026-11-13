# (liii pi code all-in-one probe) 模块函数分类索引

`(liii pi code all-in-one probe)` 在进程内通过 `pi-request-provider` 非流式入口向候选节点发送单次轻量探测请求（`ping`）。预检不恢复或写入主会话，不加载工具与技能；避免子进程开销并直接获取结构化响应状态，失败统一追加到配置目录下的按天日志 `logs/pi-YYYY-MM-DD.log`。

探活遵循 gf pi 既有重试机制，但与长任务重试预算解耦：最多重试 3 次，退避基数取用户配置（上限 2000ms），单次退避上限 10s，用户显式禁用重试时探活也不重试。429 限流节点因此能快速失败并及时切换备用节点。

探活请求的 `max-tokens` 固定压到 16，避免思考型模型吃满预算导致探活超时。

使用时导入：

```scheme
(import (liii pi code all-in-one probe))
```

## 查看函数文档

```bash
gf doc pi-probe-node
gf doc pi-probe-error-code
gf doc pi-probe-prompt
gf doc pi-probe-max-tokens-metadata
gf doc pi-probe-retry-config
```

## 函数分类索引

- `pi-probe-node` - 进程内轻量预检节点，支持非流式请求、超时控制、隔离配置与失败日志记录
- `pi-probe-error-code` - 从预检错误文本或状态码提取 HTTP 状态码、timeout 或 error
- `pi-probe-prompt` - 微型预检提示词常量 `ping`
- `pi-probe-max-tokens-metadata` - 把节点模型元数据的 max-tokens 覆盖为探活专用的 16
- `pi-probe-retry-config` - 计算探活专用的重试预算配置（最多 3 次重试，退避有界）
