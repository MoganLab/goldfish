# Agents：`bin/gf pi` 任务规格与后续任务书

> 状态：上一阶段（纯 Scheme 实现 pi agent 脚本模式，覆盖任务 001~053）已于 2026-10-20
> 在 commit `[1069]`（PR #1141）与 `[1074]` 中全量结项收官并合入主干。
> **当前阶段目标：研发 `bin/gf pi` 交互式 TUI，第一阶段先做通用的 `(liii tui)` 库**。
> 本文档由 Claude 维护（与 `CLAUDE.md`、`README.md` 同为计划文档），
> 作为 Claude Code 定时流水线拉起 pi 的输入与后续任务书。pi 只负责代码，不改本文档。
> 分工：本文件写任务书与回归依据；`README.md` 写可运行示例与实测说明。

## 1. 目标与当前状态

`bin/gf pi` 已完整实现 pi coding agent 的脚本模式（已在 `[1069]` 与 `[1074]` 结项）：

```bash
bin/gf pi --print "..."          # print 模式：最终 assistant 文本写 stdout
bin/gf pi --mode json "..."      # JSON 模式：JSONL 事件流写 stdout
```

已实现：CLI 参数解析、七个内置工具（read/bash/edit/write/grep/find/ls）、
`~/.pi/agent/` 只读配置复用、全套 Provider（google / kimi-coding / deepseek / minimax / zai-coding-cn / anthropic）、
流式 SSE、Agent 循环、会话持久化与 `--continue`、以及 blueprint / compass / simplify / red-green / compact 等全部扩展模式。

**平台范围裁定（用户裁定 2026-10-20）**：**本次任务开发机器是 Windows，目前不需要考虑 Linux**。当前全力在 Windows 开发机上推进 `(liii tui)` 库及 `demo/tui/snake.scm`；**等 062 完成之后，再在 Linux 上做系统适配**。

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

## 3. 后续任务书（细粒度拆分；编号从 054 起全局递增，状态由 Claude 维护，供定时流水线调度）

拆分原则：每个任务一次 pi 拉起、一个可独立验证的验收点、若干小提交；
全部遵守 §2 规范（TDD、小提交、fmt/check 门禁、命令 timeout、无真实 TTY 非交互测试安全）。
每个任务执行时，pi 从模板 `tools/pi/devel/ddd.md` 复制建立 `tools/pi/devel/<编号>.md`
任务文档（任务描述、实现说明、提交清单），随任务 commit 入库；Claude 验收后在该
文档补充执行与验收记录。
新任务追加时编号继续递增；完成一项把状态改为「已完成」。

### 3.1 第一阶段：`(liii tui)` 通用库研发任务表

> 架构约定：`(liii tui)` 是通用的终端 UI 基础库，功能本身与 pi 业务解耦；**`(liii tui)` 内部不包含 markdown 渲染**。

| 编号 | 任务 | 内容与验收点 | 状态 |
|---|---|---|---|
| 054 | `(liii tui component)` 核心协议与基础文本组件 | 定义 Component 协议（`component-render`、`component-handle-input`、`component-invalidate!` 等标准接口）；实现 `make-text` 组件（支持按 `width` 自动折行换行），以及 `make-truncated-text` 组件（单行超出 `width` 截断带 `...`）、`make-spacer` 占位组件；提供单元测试覆盖各种折行与截断边界 | 已完成（054.scm 70 项断言全绿；commit `7e0933e0`/`bfdc6ef9` 已推送；google/gemini-3.8-flash） |
| 055 | `(liii tui text-width)` 显示宽度与 ANSI 感知文本工具 | 实现纯函数 `display-width`、`truncate-to-width`、`wrap-text`、`strip-ansi`：ANSI SGR/CSI 序列宽度为 0，CJK/全角/emoji 按 2 列，组合字符按 0 列；截断与折行不得切断转义序列；将 054 的 `make-text`/`make-truncated-text` 迁移到显示宽度语义；单函数单测试，覆盖 ASCII/CJK/emoji/ANSI/零宽/负宽/正好等宽边界 | 已完成（055.scm 90 项断言全绿；commit `950349be`/`b1068cc1` 已推送；google/gemini-3.8-flash） |
| 056 | `(liii tui keys)` 键盘输入与转义序列解析 | 纯函数解析终端输入字符串/字节流，映射为标准按键符号（`'up`、`'down`、`'left`、`'right`、`'enter`、`'backspace`、`'tab`、`'ctrl-c`、`'ctrl-d`、字符文本等）；单函数单测试，全量覆盖 ANSI/xterm 转义序列 | 已完成（056.scm 108 项断言全绿；commit `3011d09b`/`7c8bdaab` 已推送；google/gemini-3.8-flash） |
| 057 | `(liii tui terminal)` 终端抽象与 `virtual-terminal` | 定义终端通用接口规范（写入、光标控制、尺寸查询）；实现脱机无头测试用的 `virtual-terminal`（内存模拟屏幕行缓冲区和光标位置，为后续所有组件与差分渲染提供确定性测试基础）；尺寸查询需有 `COLUMNS`/`LINES` 式环境变量回退与默认 80x24；单元测试验证行缓冲记录、光标移动、resize 与尺寸响应，全程不依赖真实 TTY | 待执行 |
| 058 | `(liii tui box)` 与布局容器组件 | 实现 `make-box` 边框组件（支持单线/双线/圆角 ASCII/ANSI 边框与内边距）、`make-vstack` 垂直拼接组件、`make-hstack` 水平排列组件；测试覆盖嵌套布局计算与宽度自适应 | 待执行 |
| 059 | `(liii tui loader)` 转轮动画与状态组件 | 实现 Spinner 动态加载转轮组件（多帧状态推进、前缀/后缀文本提示、显示与隐藏切换）；单元测试断言各帧文本渲染与状态转移 | 待执行 |
| 060 | `(liii tui editor)` 单行与多行交互编辑器组件 | 实现带光标位置指示、字符插入、退格删除、方向键光标移动、回车换行与历史记录的输入编辑器组件，实现 `component-handle-input`；通过输入转义序列进行全流程非交互测试 | 待执行 |
| 061 | `(liii tui main-screen)` 差分渲染与屏幕刷新 | 实现基于 `terminal` 的主屏幕输出驱动，比较前后帧行差异（差分渲染），只重绘变化行或追加新行，避免终端闪烁；在 `virtual-terminal` 上断言 viewport 内容、只写变化行、resize 触发全量重绘，并捕获原始 `write()` 验证 ANSI 序列 | 待执行 |
| 062 | `demo/tui/` 终端贪吃蛇游戏验收 | 使用 `(liii tui)` 在 `demo/tui/snake.scm` 实现终端贪吃蛇交互游戏，支持键盘方向键控制、食物生成与贪吃蛇增长、实时计分板、边界/自碰碰撞判定；验收分两层：纯游戏逻辑与脚本化按键回放自动化测试（非 TTY），以及在 Windows 终端由 Claude/开发者做真实交互冒烟；作为 `(liii tui)` 第一阶段通用库的生动实机交互验收成果 | 待执行 |

### 3.2 历史里程碑收官记录（已完成）
- **任务 001~053 全部结项**：脚本模式及所有核心能力已在 commit `[1069]` 与 `[1074]` 落地并全部并入主干。历史任务文档与验收脚本完整保留在 `tools/pi/devel/001.md~053.md` 中。

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

RPC 模式；extensions/MCP/codemode/packages/themes/prompt templates/skills；
图片输入（暂缓）；OAuth 凭据刷新（auth.json 的 oauth 类型条目不消费）；
会话写入 `~/.pi/agent/` 管理；项目级 `.pi/settings.json`；Anthropic OAuth 刷新；
**`(liii tui)` 内部不做 markdown 渲染**（定位为轻量纯通用 TUI 基础库）；
**当前阶段跨平台（聚焦 Windows 开发机，目前不考虑 Linux；等 062 完成后再做 Linux 适配）**。

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
  pi/                             # 多级子库（详见 README.md 架构演进节）
    types/gemini/anthropic/auth/models/retry/agent/
    tools/config/prompt/session/args/modes.scm
    cli.scm                       # (liii pi cli) CLI 入口
tools/pi/tests/liii/pi/           # 测试（镜像源码库结构，一个函数一个测试文件）
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
| 11 | 当前聚焦 Windows 开发机，暂不考虑 Linux | 062 贪吃蛇完成后再做 Linux 适配（用户裁定） |
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
