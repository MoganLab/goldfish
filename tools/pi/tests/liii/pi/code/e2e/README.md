# (liii pi code e2e) 端到端测试

本目录使用本地回环 stub HTTP 服务端验证 `gf pi` 核心链路的端到端行为，不访问真实公网或真实模型凭据。

## 测试列表

- `http-timeout-e2e-test.scm` —— 验证 HTTP 空闲超时语义：长流不被总时限掐断、服务端停摆触发空闲超时并保留部分输出、超时后按重试策略恢复、`httpIdleTimeoutMs` 设为 `"disabled"` 时关闭空闲超时。

## 运行

```bash
bin/gf test tools/pi/tests/liii/pi/code/e2e/
```
