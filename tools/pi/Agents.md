# Agents：`bin/gf pi` 任务规格与后续任务书

> 状态：§5 所列九个阶段及 050~053 优化已全部完成，print 模式与 JSON 模式可用。
> 当前大任务：**轻量纯 Scheme Extension 机制**（全面对齐原生 pi 概念模型，
> 目录约定即发现，解耦并迁移内置扩展至 `tools/pi/liii/pi/extensions/`）。
> 本文档由 Claude 维护（与 `CLAUDE.md`、`README.md` 同为计划文档），
> 作为拉起 pi 的输入与后续任务书。pi 只负责代码，不改本文档。
> 分工：本文件写任务书与回归依据；`README.md` 写可运行示例与 TODO 用法。

## 1. 目标与当前状态

`bin/gf pi` 用纯 Scheme 实现了 pi coding agent 的脚本模式，替代 `pi -p`：

```bash
bin/gf pi --print "..."          # print 模式：最终 assistant 文本写 stdout
bin/gf pi --mode json "..."      # JSON 模式：JSONL 事件流写 stdout
```

已实现：CLI 参数解析、七个内置基础工具（read/bash/edit/write/grep/find/ls）、
`~/.pi/agent/` 只读配置复用、Gemini/Kimi/Deepseek provider（含流式 SSE）、Agent 循环、
print/JSON 双模式、会话持久化与 `--continue`、单调时钟与 Retry-After 增强重试。

**当前大任务目标（Extension 机制）**：
- **纯 Scheme 实现，保持轻量与自包含**：不引入 Node.js 运行时或外部重型进程，所有扩展使用 Goldfish Scheme 实现。
- **全面对齐原生 pi 的概念模型与发现机制**：
  - API 模型对齐：`ExtensionAPI` / `ExtensionContext`，提供 `register-tool`、`register-flag`、`register-command`、`on` 事件订阅与拦截；
  - 发现机制对齐（**目录约定即生效**）：只要把相关的 `.scm` 扩展文件（或带有 `index.scm` 的子目录）放在对应扩展目录下，启动时自动扫描发现并加载；
  - 分层发现范围：
    1. 仓库内置扩展：`tools/pi/liii/pi/extensions/`（直接随 Goldfish 安装包内置分发，保留 `simplify`, `red-green`, `code-review` 等轻量内置插件）；
    2. 用户全局扩展：`~/.pi/agent/extensions/*.scm` 或 `~/.pi/agent/extensions/*/index.scm`；
    3. 项目局部扩展：`.pi/extensions/*.scm` 或 `.pi/extensions/*/index.scm`；
    4. 命令行显式指定：`-e, --extension <path.scm>`；
  - 插件生态安装：提供 `bin/gf pi install <git-url>` 命令，将第三方插件一键 git clone 到扩展目录（默认全局，支持 `--local`），克隆即生效；
- **核心解耦与外部插件示范（用户裁定 2026-10-20）**：
  - 将 `compass` 和 `blueprint` 两个重型扩展**正式移出主仓库**，抽离为外部独立的 Extension 仓库，作为标准外部插件的 Reference Implementation；
  - 彻底移除主核心代码（`args.scm`、`tools.scm`、`cli.scm`、`modes.scm` 等）对 `blueprint`、`compass` 的侵入式硬编码；
  - 验证端到端闭环：通过 `bin/gf pi install` 安装外部 `compass` 与 `blueprint` 扩展，验证其自动发现与功能的完整运作！

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
  仅改动三份计划文档（CLAUDE/Agents/README）的 commit 不涉及代码，
  不需要编号。
- **测试规范**：TDD，先写测试再实现；测试按 goldfish 惯例**一个函数一个测试文件**
  （`tests/liii/<模块>/<函数名>-test.scm`），改测试后 `bin/gf doc --build-json`。
  **测试目录镜像源码库结构**（函数文档即测试文件，gf doc 从中提取）：库文件
  `pi/gemini/request.scm` 的函数，测试放 `tools/pi/tests/liii/pi/gemini/request/
  <函数名>-test.scm`。**文档跟着实现走，不跟导出走**——实现只有一个（在
  `pi/gemini/request.scm`），导出可以经多层（`(liii pi gemini)`、`(liii pi)`
  门面再导出），但函数的测试文件只放实现所在的那一处。
  **单元测试不得引用三份计划文档**（`Agents.md`/`README.md`/`CLAUDE.md`
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
- **Extension 扩展机制规范（用户裁定 2026-10-20）**：
  - **轻量纯 Scheme 实现**：插件完全由 Goldfish Scheme 编写，不引入 Node.js 运行时或外部进程依赖。
  - **对齐原生 pi 概念模型**：
    - 接口对齐：提供 `ExtensionAPI` / `ExtensionContext`，支持 `register-tool`、`register-flag`、`register-command`，以及 `on` 事件监听；
    - 事件生命周期对齐：`session_start`、`agent_start`、`tool_call`（可拦截/修改输入/block）、`tool_result`、`agent_end`；
    - 上下文注入对齐：扩展可通过钩子或 `prompt-injectors` 向 system prompt 或用户轮次注入动态上下文；
  - **发现机制（目录约定即生效）**：
    - 开发者/用户只要把相关 `.scm` 文件放在对应目录下，启动时自动扫描并加载生效，无需额外手动配置；
    - 发现层级：内置（`tools/pi/liii/pi/extensions/`，随包分发）→ 全局（`~/.pi/agent/extensions/`）→ 项目（`.pi/extensions/`）→ 显式（`-e/--extension`）；
  - **核心解耦**：核心 `tools.scm`、`args.scm`、`cli.scm`、`modes.scm` 不得硬编码任何特定扩展的名称、分支或选项，全部由扩展通过注册表动态挂载；
  - **错误隔离与容错**：单个扩展扫描、加载或运行出错不得导致主进程崩溃，仅在 stderr 输出告警并安全跳过。

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
| 070 | Extension 核心抽象与 ExtensionAPI | 实现 `(liii pi extension)`：定义 `ExtensionAPI` / `ExtensionContext` 记录类型，提供工具注册（`register-tool`）、命令行参数注册（`register-flag`）、子命令注册（`register-command`）、事件监听与派发（`pi-on!`/`pi-emit!`）；事件生命周期对齐原生 pi（`session_start`, `agent_start`, `tool_call`, `tool_result`, `agent_end`）；TDD 覆盖 | 已完成 |
| 071 | 插件目录发现机制与加载器（Discovery & Loader） | 实现插件扫描与自动发现（“只要把相关文件放在对应目录即可生效”）：1) 内置目录 `tools/pi/liii/pi/extensions/`；2) 全局目录 `~/.pi/agent/extensions/*.scm`；3) 项目目录 `.pi/extensions/*.scm`；4) 命令行 `-e, --extension <file.scm>`；加载器执行扩展工厂函数注入 API；单插件异常隔离不崩溃主进程；TDD 覆盖 | 已完成 |
| 072 | 工具层解耦：接驳动态扩展工具注册表 | 改造 `tools.scm`：将 `get-tool-definition` 与 `execute-tool-call` 从硬编码分支改为查询扩展注册表（保留默认 7 个基础工具，扩展工具由 Extension 动态注册接管）；移除 `tools.scm` 对 `blueprint`, `compass`, `red-green` 的硬编码 import 与 cond 分支；既有工具测试 100% 保持通过 | 待执行 |
| 073 | 命令行与路由解耦：动态 Flags 挂载与 Local Flow 分发 | 改造 `args.scm`、`cli.scm` 与 `modes.scm`：支持从 Extension 注册表中动态提取扩展声明的命令行 flags 与验证规则；支持扩展注册 local-flow 与 command 路由处理器；移除核心中硬编码的 `--blueprint`, `--codemap`, `--simplify` 等分支；保持所有 CLI 参数与 local-flow 行为 100% 兼容。**注意行数余量**（072 收尾实测）：`tools/pi/liii/pi.scm` 已 299 行、`tools.scm` 293 行，均逼近 300 行硬约束——新增门面导出前先评估是否需再拆一层子库（如把门面导出下沉到 `(liii pi extension)` 等已有子库），不要硬塞 | 待执行 |
| 074 | 上下文与生命周期拦截：Prompt 注入与 Tool 拦截 | 支持扩展通过 `prompt-injectors` 向系统提示词注入动态内容（如 codemap、blueprint 状态、TDD 约束）；实现 `tool_call` 拦截钩子（支持返回 block 或修改 input 参数）；**补齐原生 pi 的 `terminate` 语义**（`ToolCallEventResult.terminate`——block 后提示 agent 结束当前工具批次，原生 `packages/coding-agent/src/core/extensions/types.ts:1462`；070 尚未实现，勿遗漏）；移出主干 prompt 与循环中硬编码的各扩展提示词注入逻辑；TDD 覆盖 | 待执行 |
| 075 | 内置扩展收敛迁移（simplify, red-green, code-review） | 将保留在仓库内的轻量内置插件正式收敛至 `tools/pi/liii/pi/extensions/`，改写为符合标准 Extension 契约的轻量 Scheme 扩展；`red-green` 通过 `register-command` 对齐提供 `/tdd` 与 `/tdd-status` 命令并兼容 `--red-green`；核心层仅通过扫描内置目录自动加载；全量单元测试与既有冒烟示例回归验证全绿 | 待执行 |
| 076 | `pi install` 插件安装命令实现 | 实现 `pi install <git-url>` 子命令：调用 git 将远程扩展仓库 clone 至对应目录（默认全局 `~/.pi/agent/extensions/<name>`，支持 `--local` 克隆至当前工程 `.pi/extensions/<name>`）；配合目录扫描机制实现插件一键安装即生效；含 git 操作超时控制（强制带 timeout）、目录冲突检查与安全性校验；TDD 覆盖 | 待执行 |
| 077 | 外部扩展实例移出与端到端实测验证（compass 与 blueprint） | 用户裁定移出 `compass` 与 `blueprint`：1) 将两者代码抽离为独立的外部扩展仓库（作为标准外部扩展参考实现）；2) 主仓库完全清理掉两者的实现代码与硬编码残留；3) 通过 `bin/gf pi install` 安装这两个外部扩展，端到端验证 `--blueprint` 与 `--codemap` 的完整功能无回归 | 待执行 |
| 078 | 工具调用 timeout 语义重定：对齐原生 pi + 默认 600s | 用户裁定：默认 timeout 可为 600s，且 **LLM 感知到后能通过工具调用传递更大的 timeout**（默认值只是缺省值、不是禁区），且**行为需与 `~/git/pi` 对齐**。原生 pi 基准（`packages/coding-agent/src/core/tools/bash.ts:22-43`）：① 工具 timeout **只由模型决定**——原生 CLI 根本没有 `--timeout` 参数，无任何 CLI 级天花板；② 模型未指定 → **无超时**（`resolveTimeoutMs(undefined)` 返回 undefined，不挂 setTimeout），schema 如实写 `"optional, no default timeout"`；③ 非法值（≤0 / 非有限）→ **显式抛错** `"Invalid timeout: must be a finite number of seconds"`，非静默忽略；④ 仅保留合理性上界 `MAX_TIMEOUT_SECONDS=2147483.647`（≈24.85 天）作 sanity cap，超出亦显式报错；⑤ 错误文案 `Command timed out after N seconds`（goldfish 已一致）。goldfish 现状四处不合：`bash.scm` 的 `(min valid-arg valid-def)` 把"默认值"当"天花板"、模型请求更大值被**静默压回且无回执**；`pi-bash-schema` 是无参静态函数、描述**硬编码** `"defaults to 120s"`；非法值（≤0/负数）被**静默忽略**而非报错；`--timeout` 默认仅 120s 且 `args.scm` 缺省写死 `120`（与 `session-timeout` 存 `#f` 的做法相反）致显式/缺省不可区分。要求：① **默认值 120s → 600s**（与 `--session-timeout` 默认 600s 对齐），且默认值**仅在模型未指定时**生效；② **模型显式值即生效值，不再被默认 clamp**（对齐原生"模型决定即生效"），最终兜底为会话死线；③ **非法值显式报错**（对齐原生 throw：≤0 / 非有限 / 超 sanity cap），不得静默忽略或静默压回；④ **schema 描述动态如实**，模型在决策前即知真实默认值与"默认值非上限"的语义；⑤ `--timeout` 语义由"天花板"改为"默认超时"（`args.scm` 缺省改 `#f` 以区分显式/缺省，`flow.scm` 判定），并消除 `--timeout 0` 当前的语义冲突（既会致模型值无 clamp 突破，又与既有测试断言的"安全回退 120 秒"矛盾）；⑥ **既有回归用例按新语义改写**——`pi-execute-bash-test.scm:68`「模型超大 timeout 不能突破顶层用户限制」（显式 `:timeout 1` + 模型 1000）表达的正是待废止的"天花板"语义，新语义下 `--timeout` 仅为默认值、模型 1000 应生效 1000，该用例须相应改写并在 078 任务文档中记录此语义变更；⑦ 其他工具（read/edit/write/grep/find/ls）目前无 per-call timeout 参数，一并统一可见性；TDD 覆盖 | 待执行 |
| 079 | 会话时限注入 system prompt：让 agent 感知剩余预算 | 修 `gf pi` 的会话死线对 agent 不可见问题：`prompt.scm` 中 timeout/max-turns/deadline **零命中**，system prompt 从不告诉 agent 它的会话预算；而 `--session-timeout` 是硬墙钟死线（`agent.scm`：`deadline = start + session-timeout`），到达即静默截断进程、最终报告全丢——070 阶段二实测触发：`Session timeout exceeded: total execution deadline reached`，仅余 59 字节错误日志，前序产出虽在但结论与验收依据全丢。要求把会话时限（`--session-timeout`）与轮次上限（`--max-turns`）注入 system prompt（或提供剩余预算查询手段），使 agent 可自我配速、临近时限优先收敛产出而非被动被杀；TDD 覆盖 | 待执行 |

共同约束：
- 每个任务常态三个 commit：`[编号][PLAN]`（建档）+ `[编号][REVIEW]`（代码）
  + `[编号][REVIEW]`（验收脚本）；观测 git log 即知任务进度。
- 任务 1-4（Kimi）凭据事实：auth.json `kimi-coding` 为 type:"api_key"，仅
  key/type 字段，无 OAuth。
- 任务 5-7（迁移）动机：核心库只服务于 pi 子命令，不属于通用 liii 库；完成后由
  Claude 更新 Agents/README 中的路径描述。
- 任务 8-11（record 化）外部行为零变化（JSON 输出、CLI 行为、事件序列不变），
  全部既有测试不回归；先补访问器测试再迁移实现。

## 4. Non-goals（不做项，用户裁定）

交互式 TUI / RPC 模式；Node.js 运行时引入 / 跨进程 JS 桥接（保持纯 Scheme 自包含轻量）；
MCP/codemode/packages/themes/prompt templates/skills；图片输入（暂缓）；
OAuth 凭据刷新（auth.json 的 oauth 类型条目不消费）；
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
  pi/                             # 多级子库
    types/gemini/anthropic/auth/models/retry/agent/
    tools/config/prompt/session/args/modes.scm
    extension.scm                 # (liii pi extension) 扩展核心机制门面
    extension/                    # 扩展子模块（api/registry/discovery/loader/hooks）
    extensions/                   # ★ 内置随包分发扩展（blueprint/compass/red-green 等）
    cli.scm                       # (liii pi cli) CLI 入口
tools/pi/tests/liii/pi/           # 测试（镜像源码库结构，一个函数一个测试文件）
  extension/                      # 扩展核心机制测试
  extensions/                     # 各内置扩展测试
tools/pi/tests/                   # 其余测试与 fixture/stub server
tools/pi/devel/                   # 任务文档（ddd.md 模板 + 编号.md + 编号.scm 验收脚本）
tools/pi/README.md                # 经真实冒烟验证的命令行示例（Claude 维护）
tools/pi/Agents.md                # 任务书与回归依据（本文档，Claude 维护）
tools/pi/CLAUDE.md                # Claude Code 与 pi 的协作规范（Claude 维护）
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
| 16 | simplify 默认只读（原生 pi 直接改写文件） | 防误改：先看建议再 --apply |
| 17 | simplify print 模式直接输出报告（原生 TUI 交互改写） | 脚本模式适配（045 裁定） |
| 18 | simplify 无独立模型通道（复用主模型） | 减少实现面（045 裁定） |
| 19 | 044 code-review：print 直接输出报告（原生 TUI/异步 follow-up 在 pi -p 下 stdout 空） | 脚本模式适配（044 实测缺陷） |
| 20 | 045 simplify：默认只读+--apply 改写 | 防误改（045 裁定；原表 #16-18 合并整理） |
| 21 | 046 red-green：状态仅存进程内（原生写 ~/.pi/red-green/） | 不污染共享目录（046 裁定） |
| 22 | 047 blueprint：计划状态写工程内 .pi/blueprint.json（非 ~/.pi/blueprints/） | 按 cwd 隔离防互污；跨进程存活（047 实测 native project_id 恒 unknown） |
| 23 | 047 blueprint：不提供 blueprint_create 工具，计划由流程生成+Scheme 确定性校验落盘 | 环/依赖校验从模型自觉变为可测代码（047 裁定） |
| 24 | 047 blueprint：门禁只留 custom_command/user_approval（去掉写死的 npm test/tsc） | 非 Node 工程必失败（047 实测缺陷 2） |
| 25 | 047 blueprint：user_approval 可用 --approve 放行 | 修 native 人工门死锁（047 实测缺陷 3） |
| 26 | 047 blueprint：门禁超时可配（默认 --timeout 120s） | native 写死同步 120s（047 实测缺陷 4） |
| 27 | 047 blueprint：print 直接输出计划/进度/门禁结果 | native pi -p stdout 空（同源缺陷 1） |
| 28 | 048 compass：codemap 状态写工程内 .pi/codemap.json，不写任何 ~/.pi/ | 防互污；~/.pi/agent/ 只读复用一贯原则 |
| 29 | 048 compass：内容哈希覆盖实际内容（深度 2 树+manifest） | 修 native 深度 2 改动不失效缺陷 |
| 30 | 048 compass：tour 不落盘实时计算 | 修 native tour 缓存陈旧缺陷 |
| 31 | 048 compass：print 直接输出 codemap/tour Markdown | 修 native pi -p stdout 0 字节（同源缺陷） |
| 32 | 048 compass：tour 主题全树搜索（native 只看根与 src/） | 深层主题目录可达 |
| 33 | 048 compass：源扩展名加 .scm/.sld/.ss + Scheme 入口规则 + xmake.lua | native 白名单不含 Scheme（本仓库几乎为空） |
| 34 | 048 compass：--tools 白名单时不自动追加 compass 工具 | 白名单即用户意图（有意收紧） |
| 35 | 048 compass：不移植 stale 提示（native 恒 false 死代码） | 哈希保证新鲜度 |
| 36 | 070 tool_call 输入改写用「返回值式 override」（`(override (input . ...))`）而非原生 pi 的「原地 mutate `event.input`」 | Scheme 数据不可变，函数式返回更贴合 Scheme 语义；原生 `types.ts:1254` 明示"mutate it in place" |
| 37 | 070 tool_call 监听器异常被隔离（捕获后告警并继续后续监听器） | 原生 `runner.ts:1250-1268` 的 `emitToolCall` 无 try/catch、异常向上传播中断整个工具流程（仅 `emitUserBash` 有 catch+rethrow）；goldfish 依 §2「错误隔离与容错」取更保守行为 |
| 38 | 071 扩展发现多一层「仓库内置目录」（`tools/pi/liii/pi/extensions/`，可用 `PI_BUILTIN_EXTENSIONS_DIR` 覆盖） | 原生 pi 仅两层：项目级 `<cwd>/.pi/extensions/` 与全局 `~/.pi/agent/extensions/`（`loader.ts:849-853`），无内置层；goldfish 需随包分发 simplify/red-green/code-review |
| 39 | 071 扩展仅支持 `.scm` 文件与 `<子目录>/index.scm`，不识别 `package.json` 的 `pi` 字段包形态 | 纯 Scheme 扩展不引入 Node 包生态；多入口扩展用 `index.scm` 表达，第三方安装形态归 076（git clone 目录） |
