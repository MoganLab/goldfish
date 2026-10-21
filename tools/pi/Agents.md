# Agents：`bin/gf pi` 任务规格与后续任务书

> 状态：§5 所列九个阶段及 050~053 优化已全部完成，print 模式与 JSON 模式可用。
> **流水线进度（2026-10-21）**：Extension 机制核心任务 070~075 已全部完成（PR #1165，
> 分支 `da/plugins_pi`，跨平台 CI 修复并已合并最新 main 分支）。**第一阶段
> 076~080 已全部完成并 push 至 `origin/da/1079/code_pi`**（分支 `da/1079/code_pi`）：
> 076 内置扩展收敛（5 个插件归位 `extensions/`，核心零硬编码）→ 077 `pi install`
> 插件生态 → 078 compass/blueprint 外部示范与 E2E → 079 工具 timeout 语义重定
> （对齐原生 pi，默认 600s、模型决定即生效、非法值显式报错）→ 080 会话时限注入
> system prompt（`<session_budget>` 静态注入 + `<current_turn_budget>` 回合级刷新
> + `<budget_warning>` 临界预警，根治「静默截断丢报告」）。
> **第二阶段 081~086 亦已全部完成并 push**（同一分支）：081 TUI 架构骨架与 `dynamic-wind`
> 安全还原 → 082 状态机与主循环（脏标记门控）→ 083 `(liii tui editor)` 输入组件
> （UTF-8 感知退格 / Shift+Enter 折行 / 历史导航）→ 084 MainScreen 三分区布局与
> Resize 自适应 → 085 消息视口与中英文/emoji 显示宽度折行 → 086 流式 SSE 增量上屏与
> 行级差分（DECSET 2026 原子刷新，单次 chunk 仅覆写 1~2 行）。
> **当前推进 090**（人在回路敏感操作模态确认），其后 **091「修复 `bin/gf pi` 首次启动
> 黑屏」**为用户实测的最高优先级缺陷，其后 092 快捷键与 /help、093 虚拟终端 E2E
> 基础设施、095~097 为 E2E 的三段拆分（流式与 Spinner 链路、模态与 Slash 链路、
> 全量集成与 CI 门禁）。
> **094（provider 内 key 级轮换）已取消**——经核实该能力早已由任务 033 落地并 e2e 通过，
> 本机不生效系 google 仅有单 key 所致（凭据供给问题）。
> 编号因上述调整整体顺延一位（原 091→092、092→093、093→094），编号只作登记、不作次序，
> 实际执行顺序见 §3「共同约束」。
> （任务表按 §3 滚动窗口只保留未完成任务；076~086 的完整记录见 `tools/pi/devel/<编号>.md`
> 与 commit history。）
> **全局后续工作两阶段规划（用户裁定 2026-10-20）**：
> - **第一阶段**：完成当前未完成的工作，主要是 Extension 扩展机制收敛闭环（076~078）以及修复 `bin/gf pi` 核心功能与原生对齐（079~080）；
> - **第二阶段**：基于刚刚合入 main 的 `(liii tui)` 通用终端 UI 库，实现 `bin/gf pi` 原生沉浸式 TUI 交互界面！
> 本文档由 Claude 维护（与 `CLAUDE.md`、`README.md` 同为计划文档），
> 作为拉起 pi 的输入与后续任务书。pi 只负责代码，不改本文档。
> 分工：本文件写任务书与回归依据；`README.md` 写可运行示例与 TODO 用法。

## 1. 目标与当前状态

`bin/gf pi` 用纯 Scheme 实现了 pi coding agent，目标是全面对齐原生 pi 的脚本模式与终端交互模式：

```bash
bin/gf pi --print "..."          # print 模式：最终 assistant 文本写 stdout
bin/gf pi --mode json "..."      # JSON 模式：JSONL 事件流写 stdout
bin/gf pi                        # 第二阶段目标：启动基于 (liii tui) 的全屏交互式 TUI
```

已实现：CLI 参数解析、八个内置基础工具（read/bash/edit/write/grep/find/ls/web_search）、
`~/.pi/agent/` 只读配置复用、Gemini/Kimi/Deepseek provider（含流式 SSE）、Agent 循环、
print/JSON 双模式、会话持久化与 `--continue`、单调时钟与 Retry-After 增强重试、
Extension 机制核心（070~075：ExtensionAPI/Context、四层插件发现与安全加载、工具/CLI/路由完全解耦、Prompt 注入与 Tool 生命周期拦截、批次 terminate 截断）。

**全局战略路线（两阶段演进，用户裁定 2026-10-20）**：

### 阶段一：Extension 机制收敛闭环与核心运行能力修复（当前进行中，任务 076~080）
1. **纯 Scheme 实现，保持轻量与自包含**：不引入 Node.js 运行时或外部重型进程，所有扩展使用 Goldfish Scheme 实现；
2. **内置扩展规范收敛（任务 076）**：主仓库保留的轻量插件（`simplify`, `red-green`, `code-review`）正式收敛至 `tools/pi/liii/pi/extensions/`，彻底废除过渡的内置适配代码；`red-green` 通过标准 `register-command` 对齐提供 `/tdd` 与 `/tdd-status`；
3. **插件生态一键安装（任务 077）**：提供 `bin/gf pi install <git-url>` 子命令，支持全局（`~/.pi/agent/extensions/<name>`）与局部（`.pi/extensions/<name>`）一键 git clone 安装，实现「克隆即生效」；
4. **外部参考插件示范与端到端实测（任务 078）**：用户裁定：将重型扩展 `compass`（代码地图）和 `blueprint`（多任务规划）从主核心解耦，**直接把源码移动到两个相应的插件目录即可（无需在 GitHub 创建独立仓库）**；主核心代码彻底清理残留硬编码，端到端验证两插件自动发现、加载与功能的完整运作；
5. **工具超时语义重定：对齐原生 pi + 默认 600s（任务 079）**：默认值 120s 提升至 600s（对齐 session-timeout），超时只由模型决定且无 CLI 天花板 clamp 压回限制，非法值显式抛错；
6. **会话时限与轮次预算注入系统提示词（任务 080）**：将 `--session-timeout` 死线与 `--max-turns` 限制注入 system prompt，让 Agent 自主感知剩余预算并合理配速，避免死线到达时静默截断丢失产出。

### 阶段二：基于 `(liii tui)` 实现 `bin/gf pi` 的 TUI 交互界面（任务 081~090）
1. **依托通用终端 UI 库**：基于刚刚合入主干的 `(liii tui)` 库（PR #1159，已包含 Component 协议、Box/VStack/HStack 布局、按键流式解析、Standard/Virtual 终端、Spinner/Loader、单行/多行 Editor、MainScreen 行级差分渲染与 DECSET 2026 零闪烁同步输出）；
2. **交互式主事件循环**：启动沉浸式交互终端，维护 `IDLE`（等待输入）、`RUNNING`（Agent 思考中）、`CONFIRM`（人在回路授权确认）等状态机；
3. **流式增量文本与动态卡片渲染**：LLM 流式 SSE 回包实时差分上屏；Thinking 过程折叠展开；工具调用状态卡片（带动画 Spinner 与耗时结果展示）；
4. **Slash 命令悬浮菜单**：输入 `/` 时弹出基于 `Box/VStack` 的悬浮菜单，动态感知扩展注册的命令并支持键盘上下选择与 Tab 补全；
5. **人在回路（Human-in-the-Loop）**：当扩展拦截到敏感工具调用（如写文件、执行 bash）时，TUI 原地弹出模态交互对话框确认；
6. **100% 确定性单测**：借助 `(liii tui terminal virtual)` 在无 TTY 环境下注入按键流并断言屏幕缓冲区，实现全自动化 CI 回归。

**Provider 支持范围（用户裁定）**：google（Gemini）与 kimi-coding（Kimi）两个。
Anthropic 已实现但本机无访问权限，不在维护范围；其 OAuth 刷新与
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
| 087 | Thinking 推理思考过程动态折叠卡片组件 | 深度思考链条可视化呈现：1) 设计 Thinking 折叠卡片组件，状态包含 `collapsed?`（默认收起）；2) 折叠态展示为单行浅灰摘要 `> [Thinking] 正在深度思考 (N tokens)...` 或完成后的 `> [Thinking] 推理已完成 (按 Tab 展开)`；3) 展开态通过单线 Box 边框包裹完整思维链文本，支持独立视口内部滚动；4) 快捷键（Tab / Ctrl+O）自由切换展开/收起态；TDD 覆盖 | 已完成（commit `07ddc75a` PLAN + `10c00a43` CODE + `00dcbf22` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；**开发阶段经历 429 死亡螺旋并断点重拉**：首启 20 分钟零产出→终止→新会话 9 分钟完成 5 文件接线且未重写既有实现；Claude 独立复核：`bin/gf tools/pi/devel/087.scm` 实跑 **84/84 断言全绿**、全量 328/328 PASS、卡片模型含 `collapsed?`（默认收起）/`token-count`/`completed?`/**卡片级 `scroll-offset`**（与消息级视口分离）、折叠态浅灰单行摘要（推理中 `> [Thinking] 正在深度思考 (N tokens)...`／完成后 `> [Thinking] 推理已完成 (按 Tab 展开)`）、超窄终端自动截断、展开态单线 Box 边框 + `Thinking` 标题栏 + 内部 `wrap-text` 折行与独立视口截取、**Tab 仅在输入框为空时才切换**（非空保留给编辑热键，Ctrl+O 随时可切）、切换后置脏标记、非法参数抛 `type-error`、所有文件 ≤ 300 行；**并顺带清除了 081 遗留后被开发阶段重建的重复测试文件** `pi/args-test.scm`） |
| 088 | 工具调用状态卡片与执行中 Spinner 动态反馈 | 工具执行生命周期动态看板：1) 集成 `(liii tui loader)` 的 `make-spinner` 多帧动画；2) 收到 `tool_execution_start` 时追加状态卡片 `[⠋] bash: git status`；3) 主事件循环在 RUNNING 节拍驱动 Spinner 动画帧原地刷新；4) 收到 `tool_execution_end` 时 Spinner 停止，原地平滑替换为完成态 `[✓] bash (0.4s)` 或错误态 `[✗] bash: failed`，下方展示结果摘要；TDD 覆盖 | 已完成（commit `9d09f358` PLAN + `0c05d03f` REVIEW代码 + `e0296bf5` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；Claude 独立复核：`bin/gf tools/pi/devel/088.scm` 实跑 **92/92 断言全绿**、全量 329/329 PASS、工具卡模型含 `id`/`tool-name`/`args-summary`/`status`(`'running`/`'success`/`'failed`)/`elapsed`/`result-summary`/`spinner`/`start-time`、非法状态与参数抛 `type-error`、`format-tool-args-summary` 对 bash/read/edit/web_search 参数做智能摘要、`state-handle-tool-start!` 追加 `'running` 卡片并置脏、渲染为 `[⠋] <tool-name>: <args-summary>`、**主循环在 `'running` 态每节拍调 `state-tick-active-tools!` 推进 Spinner 帧并置脏触发差分刷新**、耗时精确到一位小数（`0.4s`）、`state-handle-tool-end!` 停止动画并平滑替换为 `[✓] <tool-name> (<elapsed>s)` 或 `[✗] <tool-name>: failed (<elapsed>s)`、视口原生支持工具卡与消息混排且过渡平滑无闪烁；**架构与 087 Thinking 卡完全同构**（组件 + state 切片 + 四处接线 + 镜像测试），第二张卡实现成本显著低于第一张） |
| 089 | Slash 命令悬浮菜单与动态自动补全 | 交互式扩展命令感知：1) 输入框首字符输入 `/` 时在输入框上方弹出悬浮选择菜单（Box + VStack）；2) 动态从活跃扩展注册表收集所有已注册的 `ExtensionCommandDef`（如 `/tdd`, `/review`, `/simplify`, `/help`）；3) 键盘上下键高亮切换选中项，Tab 或 Enter 一键填入输入框，Esc 取消；4) 键入后续字符时动态模糊过滤候选列表；TDD 覆盖 | 已完成（commit `11355cf1` PLAN + `048d9530` REVIEW代码 + `11b89568` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；Claude 独立复核：`bin/gf tools/pi/devel/089.scm` 实跑 **91/91 断言全绿**（8 组验收点）、全量 **330/330** PASS（`Total: 330 / Passed: 330`）、`bin/gf check` OK 最大 279 行；新增 `modes/tui/slash-menu.scm`+`slash-menu/{types,render}.scm`+`state/slash.scm`+`state/pure.scm`+`streaming/delta.scm`，改 `loop/dispatch.scm`（派发链 custom→slash→thinking→editor→scroll，**Tab 仲裁：菜单不可见时 slash 短路不吞键，Tab 按 087 约定下沉给 Thinking 卡**）、`state/{parse,types}.scm`、`state.scm`、`tui.scm`、`editor.scm`、`streaming.scm`；关键实测：①`collect-slash-commands` 真从活跃 `ExtensionRegistry` 动态收集 `ExtensionCommandDef`（补前导 `/`、按名去重、内置兜底 `/help` `/exit` `/clear`）；②输入以 `/` 开头即弹出、出现空格（进入参数输入）即隐藏；③`filter-slash-commands` 大小写不敏感前缀/子串匹配、前缀优先、无匹配显示 `无匹配命令`；④**补全为替换语义**（`pi-tui-editor-set-text!`，`/td`+Tab → `/tdd ` 而非 `/td/td`）；⑤`Esc` 仅隐藏菜单、状态保持 `'idle`、输入框文本原样保留；⑥`overlay-slash-menu-lines` 覆盖后**终端总行数恒定**（keep-h + menu-h = height）；⑦非法参数抛 `type-error`；**遗留小缺口**：`089.scm` 未硬断言全量 `Total/Passed: 330` 字面量（仅断言退出码），计数漂移防护需在后续任务脚本补齐 |
| 090 | 人在回路（Human-in-the-loop）敏感操作模态确认 | 危险操作终端拦截确认：1) 结合 074 `tool_call` 拦截钩子，高危工具调用（write/edit/高危 bash）触发模态拦截；2) 状态机切入 `CONFIRM`，主界面中央弹出模态警告对话框；3) 详细展示工具名、目标文件、命令参数与变更差异预览；4) 提示 `[y] 允许 / [n] 阻止 / [e] 编辑`；响应按键返回确认或拒绝的 tool-result，弹窗关闭后平滑恢复原屏幕；TDD 覆盖 | 执行中（文档阶段 commit `4cee4dff`；**注意**：该 commit 未按 §2 标注执行模型，已在 CLAUDE.md §5 模板中修正，后续阶段须带 `[provider/model]` 标注；验收点基线：①高危识别（write/edit/破坏性 bash：rm、git reset --hard、git push、kill）+ 074 `tool_call` 钩子触发，非法结构抛 `type-error`；②`ConfirmRequest`/`ConfirmDialog` 模型（`tool-call-id`/`tool-name`/`args`/`danger-level`/`target-summary`/`diff-preview`）+ 状态机切 `'confirm` 置脏；③**居中**模态 Box + 底部 `[y] 允许 / [n] 阻止 / [e] 编辑` + 窄终端折行截断；④`y` 放行回 `'running`、`n/N/Esc` 拒绝返回 `isError: #t` + `Operation rejected by user`、`e/E` 进编辑模式、关闭后平滑恢复无残影；⑤TDD + 门禁四连 + 全量无回归） |
| 091 | **修复 `bin/gf pi` 首次启动黑屏（用户实测缺陷，最高优先级）** | **现象（2026-10-21 用户实测）**：真实终端执行 `bin/gf pi`（无参进入 TUI），**首次启动屏幕全黑**——切入了备用屏却没有任何界面内容。**方向**：让 `bin/gf pi` 真正可用——启动即呈现完整界面并可交互；具体根因与修法由执行者调研后自行设计，不预设方案。**验收标准**：1) 启动后**立即可见完整界面**（消息视口 + 输入框 + 状态栏三区齐全），**输入框获得焦点、可直接打字**；2) 退出后终端状态**完全还原**（备用屏退出、raw mode 关闭、光标恢复、无残影）；3) 补回归测试（用 `(make-virtual-terminal)` 断言启动后屏幕缓冲区**非空**且含输入框提示符；可复用 092 建立的骨架，若 092 尚未完成则先自建最小断言，092 再抽取为通用辅助）；4) **顺带核查 081~090 的各项能力在真实交互中是否真的可触发**（用户反馈「TUI 里面功能很多」——单测通过不等于真实可用，须逐项实操确认，不可用者登记为后续缺陷） | 待执行（**原 094 号位，按用户裁定前置为 091**） |
| 092 | 全局快捷键调度与 /help 模态帮助屏 | 终端交互快捷操作与手册：1) 全局快捷键：`Ctrl+C`（RUNNING 时向 Agent 发出中断信号；IDLE 且输入空时按两次退出）；`Ctrl+L`（清空行缓冲并全量强制重绘）；`Ctrl+U`（清除当前输入行）；2) `/help` 或 `?` 触发全屏模态帮助面板，展示所有快捷键、可用基础工具、已加载插件列表及当前模型状态；按任意键返回；TDD 覆盖 | 待执行（**原 091 号位，按用户裁定依次调整到后面**） |
| 093 | TUI 虚拟终端 E2E 测试基础设施与启动/退出闭环 | **（原 092 号位，按用户裁定顺延至 093；原 092 拆分而来，用户裁定 2026-10-21：端到端验收是第二阶段收官的关键，须拆成多个任务逐层推进）**。本任务只做「地基 + 最小闭环」：1) 搭建 E2E 测试骨架 `tools/pi/tests/liii/pi/tui/e2e-test.scm`，封装虚拟终端的创建、按键注入、屏幕缓冲区快照与断言辅助函数（镜像源码库结构，按函数拆分测试文件）；2) 用 `(make-virtual-terminal)` 在纯无 TTY 环境启动 TUI，断言**启动即进入全屏形态**（备用屏切换、raw mode 置位、光标隐藏、DECSET 2026 同步输出开启）；3) 注入按键流完成「启动 → 输入一行文本 → 退出」最小闭环，断言**退出码 0** 且**终端状态正确还原**（raw mode 关闭、备用屏退出、光标恢复显示、无残影）；4) 建立可复用的「屏幕快照断言」辅助（按行/按列/子串三级粒度），供后续 095~097 复用；TDD 覆盖 | 待执行 |
| 094 | ~~provider 内 key 级轮换落地：消除 429 限流~~ | **已取消（用户裁定 2026-10-21）——本任务的前提经核实不成立**。核实结论：①**key 级轮换早已实现且已接线**——任务 033 交付的 `tools/pi/liii/pi/multikey.scm`（cooldown + LRU 选键 + 掩码）已接进 gemini/kimi 的 **request 与 stream 两条路径**，`pi-multikey-rotate!` 在 429/403 时冷却当前 key 并切下一个，池耗尽返回 `'exhausted` 由上层报错；配套 6 个单测 + `pi-multikey-e2e-test.scm`（stub Gemini server，池中一个 key 被拒、一个放行）——Claude 于 2026-10-21 实跑该 e2e **PASS**，轮换行为确实有效。②**本机不生效的根因不是代码缺陷**：`~/.pi/agent/multikey.json` **不存在** → `pi-load-multikey` 返回 `#f` → `pi-multikey-rotate!` 立即返回 `#f`，永远单 key 重试；且 **google/gemini 在 `auth.json` 中只有一个 key**（用户确认）——即使建池也只有 1 个成员，429 冷却后必然 `'exhausted`，**无从轮换**。③单 key 配额被打满属**凭据供给问题**，不是编码任务可解；若要提升吞吐需追加第二个 google key（用户侧动作）。④唯一确属缺失的是**文档**：`README.md` 曾把任务 033 列为未完成 TODO、且承诺了代码中并不存在的 stderr 轮换日志（`[multikey] ... switching to ...`，`grep "switching to"` 零命中）——已由 Claude 在 README 中**如实转正并删除该不实承诺**（README 是 Claude 直编的三份文档之一，无需单开编码任务）。⑤遗留的真实小缺陷（**登记于 §7 偏差清单第 40 条，不单开任务**）：`multikey.json` **格式非法**时 `pi-load-multikey` 的 JSON 解析被 `catch #t` 吞掉返回 `#f`，与「文件不存在」不可区分，二者均静默回退到 `auth.json` 单 key——与 §2「凭据缺失必须明确报错，绝不静默成功」相悖 | 已取消（用户裁定 2026-10-21；轮换能力经核实已由任务 033 落地并 e2e 通过，本机不生效系凭据仅单 key 所致；README 转正由 Claude 直接完成） |
| 095 | TUI 虚拟终端 E2E：流式回包与 Spinner 动画链路 | （093 拆分）虚拟终端下验证「Agent 运行中」阶段的动态渲染：1) 注入模拟 SSE 增量 chunk，断言打字机式**逐行差分上屏**（结合 086 的 DECSET 2026 原子刷新，单次 chunk 仅覆写 1~2 行）；2) 断言 088 的 **Spinner 多帧动画在运行中持续推进**（相邻两次快照帧字符不同），工具执行结束后**原地平滑替换**为 `[✓] <tool> (<elapsed>s)` 或 `[✗] ...` 完成态；3) 断言 087 的 **Thinking 折叠卡片**在运行期呈现为单行摘要、按 Ctrl+O 展开后思维链文本出现在屏幕上；4) 复用 092 的快照断言辅助，全链路在虚拟终端下零人工介入 | 待执行（**原 095 号位**） |
| 096 | TUI 虚拟终端 E2E：模态交互与 Slash 悬浮菜单链路 | （093 拆分）虚拟终端下验证「需要人干预」的两类浮层交互：1) 触发高危工具调用拦截，断言 **`CONFIRM` 状态下的居中模态对话框**渲染出工具名、目标路径、差异预览与 `[y] 允许 / [n] 阻止 / [e] 编辑` 提示；2) 注入 `y` 断言**放行闭环**（工具继续执行、弹窗关闭、屏幕恢复无残影），注入 `n` 断言**拒绝闭环**（工具结果为 `isError: #t` + `Operation rejected by user`）；3) 注入 `/` 断言 **089 的 Slash 悬浮菜单**浮层出现、候选来自活跃扩展注册表、`↑↓` 高亮切换、`Tab` 补全填入输入框、`Esc` 取消后输入框文本保留；4) 断言以上浮层与消息视口、输入框**共存不互相覆盖、终端总行数恒定** | 待执行（**原 096 号位**） |
| 097 | TUI 端到端全量集成验收与 CI 门禁接入 | （093 拆分，收官）把 093/095/096 的能力串成**完整交互链路的单次回归**：1) 编写 `tools/pi/tests/liii/pi/tui/e2e-test.scm` 全量用例与验收脚本 `096.scm`，覆盖从**启动 → 输入 → 发送 → 流式回包 → Spinner → Thinking 展开 → 工具卡完成 → 模态拦截放行 → Slash 补全 → 退出**的完整链路；2) 断言**虚拟屏幕字符快照**（关键行/关键子串的多级断言）、**退出码 0**、**终端状态完全还原**（备用屏/raw mode/光标/无残影）；3) 断言全平台门禁通过（`xmake b goldfish` / `bin/gf fmt` / `git diff --exit-code` / `bin/gf check` / 全量单测）；4) 确保 E2E 用例**纯无 TTY、零网络、零人工介入**（provider 用本地 stub），可稳定进入 CI 回归 | 待执行（**原 097 号位**） |

共同约束：
- **执行顺序不按编号**（用户裁定 2026-10-21）：**090 → 091 → 092 → 093 → 095 → 096 → 097**。
  理由：**091 是用户实测的「`bin/gf pi` 首启动黑屏」缺陷，应用当前不可用**，故置于
  最前；E2E 系列（093/095/096/097）的价值建立在「界面能正常渲染」之上，黑屏未修时
  E2E 只能验证黑屏；092 的 /help 面板加在看不见的界面上亦无意义，故排在黑屏修复之后。
  **编号只作登记，不作次序**；094（已取消）不占执行位。
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
| 40 | `multikey.json` **格式非法**时静默降级为 `auth.json` 单 key，不报错 | 2026-10-21 实测发现：`pi-load-multikey` 的 JSON 解析被 `catch #t` 吞掉返回 `#f`，与「文件不存在」不可区分，二者均静默回退——与 §2「凭据缺失必须明确报错，绝不静默成功」相悖。本机未配置该文件故未触发；已知的偏差登记备查（原 093 任务取消时留痕，暂不单开任务） |
