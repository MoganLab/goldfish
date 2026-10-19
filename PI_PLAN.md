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
| 025 | `--no-tools` / `--no-builtin-tools` 支持 | 彻底关闭工具调用变为纯文本对话模式（不向 API 发送 tools 声明，模型不会发起工具调用）；两个别名等价；--help 与 PI_SPEC 用法；TDD 覆盖关闭后请求体无 tools 字段/纯文本对话可用/与 --tools/--exclude-tools 组合时以关闭优先 | 待执行 |
| 024 | `--exclude-tools <tools>` 支持 | 从默认工具集中排除指定工具（如 --exclude-tools bash,write,edit 快速变为只读审计模式）；与 --tools 的关系：--tools 是白名单替换、--exclude-tools 是黑名单排除（两者可共存时排除优先）；实现于工具选择逻辑 + --help + PI_SPEC 用法；TDD 覆盖排除生效/未知工具/与 --tools 组合 | 待执行 |
| 023 | 模型定价表与成本统计 | 内置各大模型定价表（输入/输出/cache 命中价格，可参照 `~/git/pi` 源码定价数据提取）；每次请求结束按 usage 计算 cost（input/output/cacheRead/cacheWrite），消息结构带 cost 字段（对齐 pi 的 usage.cost 形状）；会话结束（agent_end/print 结束）统计本次对话总花费与 Prompt Cache 节省金额（JSON 模式进事件流，print 模式可汇总输出）；定价表缺失的模型 cost 置 0 或标注未知；TDD 覆盖定价匹配与计算 | 待执行 |
| 012 | thinking 内容 | `--thinking-level`，用法见 PI_SPEC.md TODO；届时与用户确认后启动 | 待执行【用户裁定 2026-10-19：thinking level 可以做了】 |
| 013 | 图片输入 | `@图片`，用法见 PI_SPEC.md TODO；届时与用户确认后启动 | 远期（放在最后：Kimi/deepseek/glm/--list-models 等核心任务全部完成后再按需启动） |
| 022 | `--model` 跨 provider 自动路由【用户实测差距】 | 对齐 pi 的 model-resolver 行为：`--model <id>` 未命中当前 provider 时，在全部 provider 的模型目录（models-store.json 各 provider 条目 + 各 provider 内置模型）中搜索，命中即隐式选定 provider；仍未命中才走自定义模型兜底（当前 provider + warning）；实测用例：`bin/gf pi -p --model k3 "你是什么模型?"` 应自动路由 kimi 并正常回答（当前 404）；TDD 覆盖命中/未命中/多 provider 同名歧义（首个命中或明确报错） | 进行中（开发阶段） |
| 014 | 缺陷修复·4字节UTF-8挂起 | prompt 含 4 字节 UTF-8 字符与 `<` 组合时静默挂起且流式空闲超时未兜底（复现 `timeout 60 bin/gf pi -p '🚀 <'`）；修复后真实冒烟通过 | 已完成（第5轮定位真正根因：特殊输入引发连续工具调用且无会话级总超时兜底；实现 run-agent-loop 会话级 deadline，三 provider 真实验证 2 秒结构化返回；验收脚本偶发网络抖动重跑即绿） |
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
| 9 | 会话 id 用自造同格式 ID（非 uuidv7） | s7 生态无现成 uuidv7 |
| 10 | 代理支持 `socks5h://` | libcurl 原生能力，实际网络硬需求 |
| 11 | 仅当前 Linux 开发机 | 用户裁定 |
