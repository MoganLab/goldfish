# (liii pi code all-in-one e2e) 端到端测试

本目录用**进程内 stub 供应商 / stub 探活**验证 all-in-one 的跨模块行为，不依赖真实 API Key、不访问公网。

## 测试列表

- `pi-all-in-one-recovery-e2e-test.scm` —— 全链枯竭进入 recovery 等待、节点恢复后用原上下文续跑、`continuous = #f` 快速失败、Esc 中止、Enter steering 只注入一次、显式 `--thinking` 在异构节点上的逐节点收敛，以及日志中不出现伪造的 `Auto-Continue`。
- `pi-all-in-one-recovery-stdout-test.scm` —— recovery 等待事件不污染 print / JSON 模式的 stdout（倒计时只走日志与 stderr）。
- `pi-app-recovery-card-e2e-test.scm` —— TUI 事件桥把 recovery 事件翻译为单张原地更新的等待状态卡片，结算后卡片不再追加。
- `pi-all-in-one-retry-switch-e2e-test.scm` —— 用户 settings.json 配置 retry 段时，首节点持续 429（本地 stub server）重试耗尽后必须切换备用节点；探活重试预算与长任务解耦，`provider.maxRetries = 12` 时探活仍最多重试 3 次（合计 4 次请求）。
- `pi-all-in-one-session-state-switch-e2e-test.scm` —— all-in-one 会话的第 2 条消息起，会话里 assistant 消息记录的物理节点不得覆盖伪模型：合并会话状态后 cfg 仍是 all-in-one，请求仍走链式调度，首节点 429 重试耗尽后切换备用节点。

## 运行

```bash
bin/gf test tools/pi/tests/liii/pi/code/all-in-one/e2e/
```
