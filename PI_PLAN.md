# PI_PLAN：`bin/gf pi` 任务规格与后续任务书

> 状态：§5 所列九个阶段已全部完成（2026-10-18），print 模式与 JSON 模式可用，
> 经真实冒烟验证的命令行示例与各功能的预期用法见 `PI_SPEC.md`。
> 本文档由 Claude 维护（与 `CLAUDE_CODE_PLAN.md`、`PI_SPEC.md` 同为计划文档），
> 作为拉起 pi 的输入与后续任务书。pi 只负责代码，不改本文档。
> 分工：本文件写任务书与回归依据；`PI_SPEC.md` 写可运行示例与 TODO 用法。

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
- **提交规范（强制）**：**验收之后的代码才有必要 commit**。任务分三阶段
  （文档/开发/验收）：PLAN 建档时 commit 一次（`[编号][PLAN] 任务名：建档`）；
  开发阶段工作区改动**不 commit**；验收通过后 REVIEW 阶段 commit **两个**——
  先 `[编号][REVIEW] 任务名：代码`（实现改动），再 `[编号][REVIEW] 任务名：验收`
  （tools/pi/devel/ddd.scm 验收脚本 + ddd.md 收尾）。`[编号][CODE]` 标记保留给
  长任务中途保护现场的场合，非必须。每个任务常态三个 commit。commit 前必须依次通过 `timeout 600 xmake b goldfish`、
  `bin/gf fmt`、`git diff --exit-code`、`bin/gf check` 与相关测试；
  提交信息格式 `[编号] 任务名：简述`（如 `[001] Kimi·非流式最小可用`）。
  **commit message 标注执行模型（2026-10-19 起）**：`[编号][provider/model][阶段] 简述`，
  如 `[027][google/gemini-3.8-flash][CODE] ...`；外部 pi 执行用 `[pi/...]` 标注其实际
  模型——git log 即模型代码质量横评数据源。
  **所有代码提交一律使用 `[三位编号]` 格式**（缺陷修复也登记入 §3 任务表获得
  编号，编号递增追加），保证 git log 的编号与任务表一一对应；
  仅改动三份计划文档（CLAUDE_CODE_PLAN/PI_PLAN/PI_SPEC）的 commit 不涉及代码，
  不需要编号。
- **测试规范**：TDD，先写测试再实现；测试按 goldfish 惯例**一个函数一个测试文件**
  （`tests/liii/<模块>/<函数名>-test.scm`），改测试后 `bin/gf doc --build-json`。
  **测试目录镜像源码库结构**（函数文档即测试文件，gf doc 从中提取）：库文件
  `pi/gemini/request.scm` 的函数，测试放 `tools/pi/tests/liii/pi/gemini/request/
  <函数名>-test.scm`。**文档跟着实现走，不跟导出走**——实现只有一个（在
  `pi/gemini/request.scm`），导出可以经多层（`(liii pi gemini)`、`(liii pi)`
  门面再导出），但函数的测试文件只放实现所在的那一处。
  **单元测试不得引用三份计划文档**（`PI_PLAN.md`/`PI_SPEC.md`/`CLAUDE_CODE_PLAN.md`
  可能被移除）——测试素材一律使用 fixtures 目录下的自建文件。
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
- **文件组织规范（用户裁定）**：单文件 300 行放不下就继续拆分（与 `bin/gf check`
  的 300 行硬约束一致）——子库再拆子模块（如 `(liii pi gemini)` 超限可拆出
  `(liii pi gemini request)`、`(liii pi gemini sse)`），不允许超长文件存在。

## 3. 后续任务书（细粒度拆分；编号从 001 起全局递增，状态由 Claude 维护，便于观测进展）

拆分原则：每个任务一次 pi 拉起、一个可独立验证的验收点、若干小提交；
全部遵守 §2 规范（TDD、小提交、fmt/check 门禁、命令 timeout）。
每个任务执行时，pi 从模板 `tools/pi/devel/ddd.md` 复制建立 `tools/pi/devel/<编号>.md`
任务文档（任务描述、实现说明、提交清单），随任务 commit 入库；Claude 验收后在该
文档补充执行与验收记录。
新任务追加时编号继续递增；完成一项把状态改为「已完成」。
回退属正常现象：验收失败或后续发现回归时把状态改回「待执行」并在表中注明原因，
重做过程在 commit history 中如实保留，不做历史改写。
**滚动窗口（由 Claude Code 执行）**：表中累计 5 个「已完成」任务时，Claude 移除
这 5 行（已完成任务的完整记录保留在 tools/pi/devel/<编号>.md 与 commit history
中），并补充新任务行，使表中始终以待执行/进行中任务为主。

| 编号 | 任务 | 内容与验收点 | 状态 |
|---|---|---|---|
| 040 | web_search 工具声明与 Schema 注册 | 已完成（040.scm 51/51；commit `5b6c701f`/`e469cc1b` 已推送；开发 deepseek + 验收 minimax） |
| 041 | Gemini 搜索请求体构造与 Grounding 解析 | 已完成（041.scm 56/56；commit `0facefda`/`3e447f6b` 已推送；全程 gemini） |
| 042 | web_search 网络执行器与结果格式化 | 已完成（042.scm 51/51；commit `180b7a8b`/`16f619af` 已推送；全程 gemini） |
| 043 | web_search Agent 工具调用闭环联调 | 已完成（043.scm 47/47；commit `5d5dbce9`/`8824a5d3` 已推送；全程 gemini；web_search 四阶段收官） |
| 050 | deepseek tool_use 参数解析 value-error 修复 | 差距清单 #11：deepseek 首轮响应的 tool_use 参数形状触发 `value-error (("input") ...)`（037 两次同点复现，会话仅 2 行即崩）；需在 anthropic 协议解析层兼容该形状（如 input 为 JSON 字符串时先解析再消费）并加防回归测试；修复前 037 类任务避开 deepseek 首轮大上下文场景 | 已完成（050.scm 30/30；commit `109c4c0d`/`bf88888e` 已推送；gemini 两阶段；差距#11 消除，deepseek 恢复主力） |
| 044 | `code-review` 内置命令 | 用户裁定前置（同 040-043）：先在原生 pi 安装配置扩展并运行成功作对照基准，再实现 bin/gf pi 对应功能。参考 ~/git/pi-extensions/packages/pi-code-review（agent 写/改文件后自动语言感知代码审查）：bin/gf pi 脚本模式的对应形态在文档阶段对齐扩展源码后裁定（候选：--code-review 对最近 diff 审查输出报告）；验收点含真实审查输出与门禁；TDD | 待执行 |
| 045 | `simplify` 内置命令 | 参考 ~/git/pi-extensions/packages/pi-simplify（审查最近改动代码的清晰度/一致性/可维护性并给出改进）：形态文档阶段裁定；验收点含真实简化建议输出；TDD | 待执行 |
| 046 | `red-green` TDD 强制 | 参考 ~/git/pi-extensions/packages/pi-red-green（强制 RED-GREEN-REFACTOR 循环：先写失败测试再最小实现再重构）：形态文档阶段裁定（候选：--red-green 开关在 agent 循环中强制 TDD 顺序）；TDD | 待执行 |
| 047 | `blueprint` 计划命令 | 参考 ~/git/pi-extensions/packages/pi-blueprint（高级目标→分阶段多会话构建计划，依赖跟踪+验证门）：形态文档阶段裁定；与现有三阶段流程互补；TDD | 待执行 |
| 048 | `compass` 代码库导航 | 参考 ~/git/pi-extensions/packages/pi-compass（生成结构化 codemaps 与 code tours，省 token 快速定向）：形态文档阶段裁定；TDD | 待执行 |
| 049 | `continuous-learning` instincts | 参考 ~/git/pi-extensions/packages/pi-continuous-learning（观察会话提炼可复用 instincts，置信度+项目作用域+闭环验证）：形态文档阶段裁定；TDD | 待执行（较复杂，排最后） |
| 051 | `pi-config-test` 环境卫生化 | 全量回归 `bin/gf test tools/pi/tests/liii/` 恒有 1 项失败：`tests/liii/pi-config-test.scm` 读取本机 `~/.pi/agent/models-store.json`（deepseek `base-url` 实为 `https://api.deepseek.com/anthropic`），与用例期望 `https://api.deepseek.com` 不符——环境依赖导致每次验收都要维护「已知失败白名单」；需改用 `PI_CODING_AGENT_DIR` 指向 `tools/pi/tests/fixtures/` 下的自建 fixture，使全量回归真正全绿、验收脚本不再需要失败白名单；TDD 覆盖 | 待执行 |
| 052 | 超时基准改用单调时钟 | 039 迁移后，流式空闲超时（`(- now last-act)`）与会话超时仍以墙钟 `pi-current-time-seconds` 为基准；NTP 回拨使差值为负时空闲超时永不触发；方案：在 `(liii date-time current-time)` 增加 `current-monotonic-millis`/`current-monotonic-seconds`（底层 `time-utc->time-monotonic` 已验证可给出单调 epoch 基准），`pi/time.scm` 暴露 `pi-current-monotonic-seconds`，agent 与三路 stream 的超时基准改用单调钟，墙钟仅用于时间戳展示；验收点：回拨场景下超时仍能触发；TDD 覆盖 | 待执行 |
共同约束：
- 每个任务常态三个 commit：`[编号][PLAN]`（建档）+ `[编号][REVIEW]`（代码）
  + `[编号][REVIEW]`（验收脚本）；观测 git log 即知任务进度。
- 任务 1-4（Kimi）凭据事实：auth.json `kimi-coding` 为 type:"api_key"，仅
  key/type 字段，无 OAuth。
- 任务 5-7（迁移）动机：核心库只服务于 pi 子命令，不属于通用 liii 库；完成后由
  Claude 更新 PI_PLAN/PI_SPEC 中的路径描述。
- 任务 8-11（record 化）外部行为零变化（JSON 输出、CLI 行为、事件序列不变），
  全部既有测试不回归；先补访问器测试再迁移实现。

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
gfproject.json                    # 注册 "pi" 子命令（module → pi/cli）
tools/pi/liii/
  pi.scm                          # (liii pi) 门面：公开 API 再导出
  pi/                             # 多级子库（详见 PI_SPEC.md 架构演进节）
    types/gemini/anthropic/auth/models/retry/agent/
    tools/config/prompt/session/args/modes.scm
    cli.scm                       # (liii pi cli) CLI 入口
tools/pi/tests/liii/pi/           # 测试（镜像源码库结构，一个函数一个测试文件）
tools/pi/tests/                   # 其余测试与 fixture/stub server
tools/pi/devel/                   # 任务文档（ddd.md 模板 + 编号.md + 编号.scm 验收脚本）
PI_SPEC.md                        # 经真实冒烟验证的命令行示例（Claude 维护）
```

分层原则：模块只有 `(liii pi)` 与 `(liii pi xxx)` 两种形式；二级子库为导出层
（无 CLI 副作用），CLI 逻辑集中在 `(liii pi cli)`。

## 7. goldfish 与 pi 的有意偏差清单

| # | 偏差 | 理由 |
|---|---|---|
| 1 | bash 工具 timeout 必传，默认 120s | 强制防卡死 |
| 2 | 默认 `--max-turns 200` | 防失控循环 |
| 4 | grep/find 自实现，不下载 rg/fd | 免外部二进制依赖 |
| 5 | 只读全局 settings.json | 不引入 trust 门控（不做项） |
| 6 | 只消费 api_key 凭据，`$VAR` 支持而 `` !command `` 不支持 | 不执行任意命令 |
| 8 | 不注入 `PI_*` 会话环境变量到 bash 工具 | 减少实现面 |
| 10 | 代理支持 `socks5h://` | libcurl 原生能力，实际网络硬需求 |
| 11 | 仅当前 Linux 开发机 | 用户裁定 |
