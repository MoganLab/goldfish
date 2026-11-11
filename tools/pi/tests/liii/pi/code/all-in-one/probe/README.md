# (liii pi code all-in-one probe) 模块函数分类索引

`(liii pi code all-in-one probe)` 在进程内通过 `pi-request-provider` 非流式入口向候选节点发送单次轻量探测请求（`ping`），并禁用探活重试。预检不恢复或写入主会话，不加载工具与技能；避免子进程开销并直接获取结构化响应状态，失败统一追加到配置目录下的按天日志 `logs/pi-YYYY-MM-DD.log`。

使用时导入：

```scheme
(import (liii pi code all-in-one probe))
```

## 查看函数文档

```bash
gf doc pi-probe-node
gf doc pi-probe-error-code
gf doc pi-probe-prompt
```

## 函数分类索引

- `pi-probe-node` - 进程内轻量预检节点，支持非流式请求、超时控制、隔离配置与失败日志记录
- `pi-probe-error-code` - 从预检错误文本或状态码提取 HTTP 状态码、timeout 或 error
- `pi-probe-prompt` - 微型预检提示词常量 `ping`
