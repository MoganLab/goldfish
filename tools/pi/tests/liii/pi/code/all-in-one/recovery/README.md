# (liii pi code all-in-one recovery) 单元测试

本目录包含 `(liii pi code all-in-one recovery)` 模块的单元测试。

`(liii pi code all-in-one recovery)` 是全链故障后的长程守护与重试引擎：`continuous` 关闭时立即返回错误消息，开启时进入可中断的步进式等待，并按 `maxWaitMs` 与 `retryIntervalMs` 决定重试时机。

## 函数分类索引

- `pi-recovery-run` - 全链枯竭后的步进等待、中止/唤醒分派与到期重试（唯一公开入口）

其余消息构造器（`%aborted-message`、`%interrupted-message`、`%exhausted-message`）与等待阶段分派（`%step-until-retry!`、`%after-step!`、`%wait-timeout!`）都是实现细节，由本目录的场景测试间接覆盖。

## 协议不变量

- 等待阶段优先级：Esc 中止 > 用户新消息唤醒 > 到期重试；新消息到达时不盲目重试，而是返回 `interrupted` 交回前台当作新任务处理。
- 结果分派：`aborted` / `interrupted` 就地结束并交回上层；恢复成功返回真实响应；仍枯竭则进入下一轮等待。
- `maxWaitMs` 是单次连续故障 episode 的累计预算，跨重试累计消耗，耗尽时记录一条 `[EXHAUSTED]` 日志。

## 测试列表

- `pi-recovery-run-test.scm`：全链故障步进式等待、Esc 中断、Enter steering 唤醒、连续故障累计超时与 EXHAUSTED 记录。
