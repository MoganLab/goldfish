# PI_PLAN：`bin/gf pi` 任务规格与后续任务书

> 状态：§5 所列九个阶段已全部完成（2026-10-18），print 模式与 JSON 模式可用，
> 经真实冒烟验证的命令行示例见 `PI_SPEC.md`。
> 本文档由 Claude 维护（与 `CLAUDE_CODE_PLAN.md`、`PI_SPEC.md` 同为计划文档），
> 作为拉起 pi 的输入与后续任务书。pi 只负责代码，不改本文档。

## 1. 目标与当前状态

`bin/gf pi` 用纯 Scheme 实现了 pi coding agent 的脚本模式，替代 `pi -p`：

```bash
bin/gf pi --print "..."          # print 模式：最终 assistant 文本写 stdout
bin/gf pi --mode json "..."      # JSON 模式：JSONL 事件流写 stdout
```

已实现：CLI 参数解析、七个内置工具（read/bash/edit/write/grep/find/ls）、
`~/.pi/agent/` 只读配置复用、Gemini provider（含流式 SSE）、Agent 循环、
print/JSON 双模式、会话持久化与 `--continue`、Anthropic provider。

**Provider 支持范围（用户裁定）**：google（Gemini）与 kimi-coding（Kimi）两个。
Anthropic 已实现但本机无访问权限，不在维护范围；其 OAuth 刷新与交互式 TUI、
项目级 `.pi/settings.json` 读取均为不做项。

## 2. 仍生效的关键规格（回归依据）

以下约束在后续开发与重构中持续有效：

- **超时规范（强制）**：每一条命令、每一次 IO 都必须有超时。bash 工具必传
  `:timeout`（默认取 `--timeout`，120s）；单次 HTTP 默认 300s；流式读取必须有
  空闲超时兜底；任何路径不允许静默挂起，超时必须结构化报错（退出码 1）。
- **提交规范（强制）**：提交越小越好，一个提交只做一件事；每个 commit 前必须
  依次通过 `timeout 600 xmake b goldfish`、`bin/gf fmt`、`git diff --exit-code`、
  `bin/gf check` 与相关测试；测试与实现同 commit；信息格式 `[x_y] 单行`。
- **测试规范**：TDD，先写测试再实现；测试按 goldfish 惯例**一个函数一个测试文件**
  （`tests/liii/<模块>/<函数名>-test.scm`），改测试后 `bin/gf doc --build-json`。
- **配置复用**：`~/.pi/agent/`（settings/auth/models-store）只读，绝不写入；
  可用 `PI_CODING_AGENT_DIR` 重定向（测试 fixture 依赖它）。
- **错误路径**：错误信息必须人可读且可诊断（含原因摘要），退出码语义按 print
  规范执行（error/length 退出码 1）；凭据缺失必须明确报错，绝不允许静默成功。
- **日志安全**：key/代理/请求体不允许写入任何日志（stderr 只输出错误摘要）。
- **JSON 模式事件序列**：`session → agent_start → turn_start → message_start/end
  （×N，含 message_update 流式增量）→ turn_end → agent_end → agent_settled`，
  事件流中出现 error 不改变退出码（进程异常才非 0）。
- **时间 API 规范（用户裁定）**：除 `sleep` 外不得使用 `(liii time)` 中的函数；
  时间相关功能一律优先使用 `(liii datetime xxx)` 系列。

## 3. 后续任务书（拉起 pi 时逐个指派）

### 3.1 Kimi provider【下一个】

- 凭据：`~/.pi/agent/auth.json` 的 `kimi-coding` 条目，**type:"api_key"**（仅
  key/type 字段，不涉及 OAuth）。
- 实现参照 `~/git/pi` 源码中 Kimi 的端点、路径与鉴权头（`--provider kimi-coding`
  选择该 provider）。
- 要求：TDD（fixture + stub server 覆盖正常回复/functionCall/错误码）；本机有
  真实凭据，端到端真实冒烟验证（timeout 包裹）通过后方可收尾；完成后由 Claude
  更新 PI_SPEC.md 的对应 TODO。

### 3.2 代码重构：数据结构 record 化【用户指定】

- 用 R7RS `define-record-type` 重构 tools/pi/liii/ 与 goldfish/liii/pi*.scm 中
  以 `list`/`cons`/alist 手工构造的数据结构（消息、事件、配置、工具结果等），
  以构造器/访问器收口数据边界。
- 硬约束：外部行为零变化（JSON 序列化输出、CLI 行为、事件序列不变），全部既有
  测试不回归；每重构一组数据结构一个小提交；`bin/gf check` 度量不超标。
- 遵守 §2 的时间 API 规范与提交规范。

### 3.3 更远期（优先级低，届时与用户确认）

- thinking 内容的请求与解析（`--thinking-level`）
- 图片输入（`@图片`）

## 4. Non-goals（不做项，用户裁定）

交互式 TUI / RPC 模式；extensions/MCP/codemode/packages/themes/prompt templates/
skills；图片输入（暂缓）；OAuth 凭据刷新（auth.json 的 oauth 类型条目不消费）；
会话写入 `~/.pi/agent/` 管理；跨平台（仅当前 Linux 开发机）；项目级
`.pi/settings.json`；Anthropic OAuth 刷新。

## 5. 阶段完成记录（原 §11 阶段表，全部完成）

| 阶段 | 内容 | 结果 |
|---|---|---|
| 一 | CLI 骨架：子命令注册、参数解析、模式选择、stdin/@file 组装 | ✅ |
| 二 | 工具层：read/bash/edit/write 默认集 + grep/find/ls | ✅ |
| 三 | 配置层：settings/auth/models-store 只读解析、`PI_CODING_AGENT_DIR` | ✅ |
| 四 | Gemini provider 非流式：请求构造、代理、映射、重试 | ✅ |
| 五 | Agent 循环 + 系统提示词 + print 模式 | ✅ |
| 六 | JSON 模式事件流 | ✅ |
| 七 | 流式 SSE + `message_update`/`tool_execution_update` + 空闲超时 | ✅ |
| 八 | Anthropic provider（含流式） | ✅ |
| 九 | 会话持久化与 `--continue`/`--no-session` | ✅ |

完成后的真实冒烟修复记录（2026-10-18）：Gemini 400（systemInstruction 形状与
thoughtSignature）、会话漏记 user 消息与 --continue 类型错误、Anthropic 凭据静默
失败、网络错误摘要、4 字节 UTF-8 挂起等，均以独立提交修复并有防回归测试。

## 6. 文件布局（参考性，随开发自由演进）

```
gfproject.json                    # 注册 "pi" 子命令
tools/pi/liii/goldpi*.scm         # CLI 入口/参数/配置/provider/agent/工具/会话/流式
tools/pi/tests/                   # CLI/工具层测试与 fixture/stub server
goldfish/liii/pi.scm 与 pi/       # 可复用核心库（provider/agent/工具/事件）
tests/liii/pi/                    # 核心库测试（一个函数一个测试文件）
PI_SPEC.md                        # 经真实冒烟验证的命令行示例（Claude 维护）
```

分层原则：`goldfish/liii/pi*` 只含可复用逻辑（无 CLI 副作用）；
`tools/pi/liii/goldpi*` 只做参数解析、组装与输出。

## 7. goldfish 与 pi 的有意偏差清单

| # | 偏差 | 理由 |
|---|---|---|
| 1 | bash 工具 timeout 必传，默认 120s | 强制防卡死 |
| 2 | 默认 `--max-turns 200` | 防失控循环 |
| 4 | grep/find 自实现，不下载 rg/fd | 免外部二进制依赖 |
| 5 | 只读全局 settings.json | 不引入 trust 门控（不做项） |
| 6 | 只消费 api_key 凭据，`$VAR` 支持而 `` !command `` 不支持 | 不执行任意命令 |
| 8 | 不注入 `PI_*` 会话环境变量到 bash 工具 | 减少实现面 |
| 9 | 会话 id 用自造同格式 ID（非 uuidv7） | s7 生态无现成 uuidv7 |
| 10 | 代理支持 `socks5h://` | libcurl 原生能力，实际网络硬需求 |
| 11 | 仅当前 Linux 开发机 | 用户裁定 |
