# Agents：`bin/gf pi` 任务规格与后续任务书

> 状态：§5 所列九个阶段及 050~053 优化已全部完成，print 模式与 JSON 模式可用。
> **流水线进度（2026-10-21）**：Extension 机制核心任务 070~075 已全部完成（PR #1165，
> 分支 `da/plugins_pi`，跨平台 CI 修复并已合并最新 main 分支）。**第一阶段
> 076~080 已全部完成并 push 至 `origin/da/1079/code_pi`**（分支 `da/1079/code_pi`）：
> 076 内置扩展收敛（5 个插件归位 `extensions/`，核心零硬编码）→ 077 `pi install`
> 插件生态 → 078 compass/blueprint 外部示范与 E2E → 079 工具 timeout 语义重定
> （对齐原生 pi，默认 600s、模型决定即生效、非法值显式报错）→ 080 会话时限注入
> system prompt（`<session_budget>` 静态注入 + `<current_turn_budget>` 回合级刷新
> + `<budget_warning>` 临界预警，根治「静默截断丢报告」）。**当前进入第二阶段
> 081~092 的 `(liii tui)` 原生 TUI 交互实现**，从 081（TUI 架构骨架与启动路由）起步。
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
| 080 | 会话时限注入 system prompt：让 agent 感知剩余预算 | 修 `gf pi` 的会话死线对 agent 不可见问题：`prompt.scm` 中 timeout/max-turns/deadline **零命中**，system prompt 从不告诉 agent 它的会话预算；而 `--session-timeout` 是硬墙钟死线（`agent.scm`：`deadline = start + session-timeout`），到达即静默截断进程、最终报告全丢——070 阶段二实测触发：`Session timeout exceeded: total execution deadline reached`，仅余 59 字节错误日志，前序产出虽在但结论与验收依据全丢。要求把会话时限（`--session-timeout`）与轮次上限（`--max-turns`）注入 system prompt（或提供剩余预算查询手段），使 agent 可自我配速、临近时限优先收敛产出而非被动被杀；TDD 覆盖。**补充证据（074 阶段二实测）**：**轮次上限同样会静默截断**——`Maximum turns limit reached`（默认 200 轮用尽），最终报告全丢，工作区产出虽在但验收依据全丢（事后实测 18/18 扩展测试仍全绿，说明断在最后的全量回归阶段而非实现阶段）。**拉起侧对策**：`--max-turns 0` 表示不限轮次（见 `gf pi --help`），长时间开发阶段应显式传 0 或大值；**根治侧对策**：两条死线（会话时限、轮次上限）都须对 agent 可见 | 已完成（commit `e233294f` PLAN + `2d570cac` REVIEW代码 + `d5354c5a` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；Claude 独立复核：`bin/gf tools/pi/devel/080.scm` 实跑 51/51 断言全绿、319/319 全量单测 PASS、`--max-turns 0` 语义坑已正确处理（显示 "unlimited turns" 且不误触发临界告警）、`prompt.scm` 295→249 行并拆出 `prompt/context.scm`、另拆 `agent/{budget,loop}.scm`） |
| 081 | TUI 架构骨架与启动路由（StandardTerminal 切换与安全还原） | 实现交互式 TUI 启动门面：1) `args.scm` 支持缺省无参时进入 `mode = 'tui`；2) 新建 `modes/tui.scm`，通过 `(make-standard-terminal)` 初始化终端（raw 模式、alternate 屏幕缓冲区 `\x1b[?1049h`、隐藏光标）；3) **安全还原兜底**：使用 `dynamic-wind` 确保任何异常、中断或退出时 100% 恢复终端原始形态（退出 alternate 屏、显示光标、还原 cooked 模式），绝不卡死终端；4) 按 q/Esc 优雅退出返回 0；虚拟终端单测覆盖 | 已完成（commit `29ac6c51` PLAN + `39c87ae4` CODE + `7507c0bc` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；Claude 独立复核：`bin/gf tools/pi/devel/081.scm` 实跑 42/42 断言全绿、320/320 全量单测 PASS、**真实冒烟：非 TTY 环境下 `bin/gf pi` 缺省无参立即打印帮助并以 0 退出、无挂死**；`dynamic-wind` 四步对称还原且 `console-raw-mode!` 用 catch 包裹防还原逻辑自身抛错；`--print`/管道输入仍回退 print 模式；`'escape`/`"q"`/`"Q"` 三键优雅退出返回 0；`resolve-app-mode` 断言已按 §2 无损收敛至 `resolve-app-mode-test.scm`） |
| 082 | TUI 会话状态机与主事件循环驱动器 | 实现响应式主循环引擎：1) 定义 `pi-tui-state`（包含 `status` [IDLE/RUNNING/STREAMING/CONFIRM/EXIT]、`messages`、`input-editor`、`active-tool`、`scroll-offset` 等字段）；2) 对接 `(liii tui terminal)` 的非阻塞按键轮询（`terminal-poll-key`）；3) 节拍控制（~30-60fps）与脏标记局部重绘驱动；4) 利用 `virtual-terminal` 注入按键流验证状态机完整流转；TDD 覆盖 | 已完成（commit `6c3484dc` PLAN + `7fba4be5` CODE + `765d0178` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；Claude 独立复核：`bin/gf tools/pi/devel/082.scm` 实跑 82/82 断言全绿、全量实测 322/322 测试文件 PASS（开发阶段报告曾误称「306 项」，已在验收阶段指令中要求更正，验收脚本断言标题已改为准确的 322）、状态机用不可变记录实现且提供纯函数 `state-with-*`（自动激活 dirty?）与受控变异 `state-set-*!` 双套 API、`state.scm` 277 行再拆出 `state/types.scm`（第五次「拆而非压」）、脏标记门控经单测断言「4 轮 tick 仅触发 2 次脏重绘」） |
| 083 | 用户输入组件集成（(liii tui editor) 与多行编辑） | 集成沉浸式 Prompt 输入框：1) 嵌入 `(liii tui editor)` 为 TUI 输入组件；2) 完整按键映射：字符插入、退格删除、左右方向键光标移动、Home/End 跳转；3) 历史记录导航：上下箭头在空行或首行时翻阅会话历史 Prompt；4) Enter 提取完整输入并提交，Shift+Enter 或反斜杠多行折行；5) 现代化圆角外框包裹与提示符 `> `；虚拟终端断言输入与提交行为；TDD 覆盖 | 已完成（commit `c2e9be4f` PLAN + `3abd23b7` CODE + `9eaad101` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；Claude 独立复核：`bin/gf tools/pi/devel/083.scm` 实跑 86/86 断言全绿、**验收点 10 把测试计数做成硬断言**（`check-true (string-contains out "Total:  323")`）——把「报告须如实」从口头要求变成可执行门禁、彻底消除 082/083 连续两次的计数推算偏差（306/322、307/323）、**UTF-8 感知光标与退格**（按完整汉字删除而非单字节）、**Shift+Enter 经 `'shift-enter`/`'alt-enter`/`'ctrl-enter` 拦截**（库按键流可区分）、**输入框内含 'q' 不误触发退出**（`loop.scm:209` 仅在未被编辑器拦截时才响应 q/Q）、首行 up 翻历史 / 末行 down 恢复草稿） |
| 084 | MainScreen 动态布局划分与窗口 Resize 自适应 | 构建响应式屏幕分区：1) 基于 `(liii tui box)` 与 `(liii tui main-screen)` 划分界面拓扑（顶部消息视口 + 中部自适应高度输入框 + 底部状态栏）；2) 动态读取 `terminal-detect-size` 计算视口比例；3) 监听窗口尺寸变化，检测到列宽行高变动时调用 `screen-resize!` 全量差分重绘无闪烁；4) 窄屏（<60列）优雅折叠状态栏与装饰边框；TDD 覆盖 | 已完成（commit `4e8dab1e` PLAN + `33fd1460` REVIEW代码 + `2ed021cb` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；Claude 独立复核：`bin/gf tools/pi/devel/084.scm` 实跑 85/85 断言全绿、全量 324/324 测试文件 PASS、**回归冒烟：非 TTY 下 `bin/gf pi` 仍立即打印帮助并以 0 退出、无挂死**、三分区在 24 行终端下精确占满无溢出（消息 top 0..19 / 输入框 top 20..22 高 3 / 状态栏 top 23）、`compute-editor-height` 上限为终端行数 40%（24 行终端→最大 9 行，50 行→最大 20 行）、降级三档（cols<60 / rows<15 / rows<10 折叠状态栏为 0 高度）、`handle-screen-resize!` 未变尺寸时不产生重绘开销、虚拟终端 E2E 验证 Resize 与按键输入共存；**计数硬断言已成为执行器默认做法，无需提醒**） |
| 085 | 消息历史视口与自然折行渲染（Message Viewport & Wrap） | 消息列表排版与滚动浏览：1) 设计 `pi-tui-message-item` 视图组件，区分 User 蓝色前缀、Assistant 绿色前缀与 System 灰色提示；2) 采用 `(liii tui text-width)` 的 `wrap-text` 与 `display-width`，按视口宽度自然折行，精准处理中英文与 emoji 终端显示宽度；3) 超出屏幕时默认自动贴底跟随；4) 支持 PageUp/PageDown 与方向键调整 `scroll-offset` 向上回滚查看历史；TDD 覆盖 | 已完成（commit `7acf9e1d` PLAN + `61f0e853` REVIEW代码 + `85e16bba` REVIEW验收，已 push 至 `origin/da/1079/code_pi`；执行模型 google/gemini-3.8-flash；Claude 独立复核：`bin/gf tools/pi/devel/085.scm` 实跑 80/80 断言全绿、全量 326/326 PASS、**折行按显示宽度扣除前缀占宽**（`display-width` 求前缀宽后 `body-width = max 1 (- width prefix-w)`，全角前缀不漏算）、**续行按前缀宽度对齐缩进**且中文不跨行断裂、三色角色前缀（user 蓝 `> ` / assistant 绿 `● ` / system 暗灰 `■ `，非法参数抛 `type-error`）、`scroll-offset = 0` 精确贴底显示最后 viewport-height 行且总行数不足一屏时完整展示不越界、滚动窗口严格 clamp 在 `max-scroll-offset` 内、边缘显示 `[Top]`/`[N%]`、PageUp/PageDown 步长 5 行并置脏驱动刷新、按键分发拆出 `loop/dispatch.scm`（第七次「拆而非压」）、**验收点 9 已把「非 TTY 冒烟」固化为每个 TUI 任务的常规验收项**） |
| 086 | 流式 SSE 增量文本上屏与行差分渲染（Streaming Output） | 实时打字机效果无闪烁上屏：1) 在 Agent 产生 `message_update` 事件时捕获增量文本 chunk；2) 原地追加到当前轮次助手消息末尾并重新排版末尾变化行；3) 借助 `(liii tui main-screen)` 的行级 diff 引擎，仅原地覆写被追加修改的 1~2 行，行缩短时使用 `terminal-clear-line-to-end` 擦尾；4) 整帧 DECSET 2026 同步包裹单次 flush，达成 0 瞬时空白闪烁；TDD 覆盖 | 执行中（文档阶段 commit `0596d27b`，5 条验收点齐备（增量 chunk 捕获与累加 / **末尾行局部增量重排** / MainScreen 行级差分覆写 / DECSET 2026 同步包裹 0 闪烁 / TDD 门禁）；设计要点：`message_update` 触发时若无活跃 Assistant 消息则自动初始化空消息并累加 chunk、维持 `STREAMING` 与脏标记；**只对被修改的末尾文本重排**（复用 085 的 `wrap-text`）避免全量历史重排、保持贴底跟随；行缩短时用 `terminal-clear-line-to-end` 擦尾；每帧由 `\x1b[?2026h`/`\x1b[?2026l` 包裹实现原子刷新；开发阶段已拉起，google/gemini-3.8-flash） |
| 087 | Thinking 推理思考过程动态折叠卡片组件 | 深度思考链条可视化呈现：1) 设计 Thinking 折叠卡片组件，状态包含 `collapsed?`（默认收起）；2) 折叠态展示为单行浅灰摘要 `> [Thinking] 正在深度思考 (N tokens)...` 或完成后的 `> [Thinking] 推理已完成 (按 Tab 展开)`；3) 展开态通过单线 Box 边框包裹完整思维链文本，支持独立视口内部滚动；4) 快捷键（Tab / Ctrl+O）自由切换展开/收起态；TDD 覆盖 | 待执行 |
| 088 | 工具调用状态卡片与执行中 Spinner 动态反馈 | 工具执行生命周期动态看板：1) 集成 `(liii tui loader)` 的 `make-spinner` 多帧动画；2) 收到 `tool_execution_start` 时追加状态卡片 `[⠋] bash: git status`；3) 主事件循环在 RUNNING 节拍驱动 Spinner 动画帧原地刷新；4) 收到 `tool_execution_end` 时 Spinner 停止，原地平滑替换为完成态 `[✓] bash (0.4s)` 或错误态 `[✗] bash: failed`，下方展示结果摘要；TDD 覆盖 | 待执行 |
| 089 | Slash 命令悬浮菜单与动态自动补全 | 交互式扩展命令感知：1) 输入框首字符输入 `/` 时在输入框上方弹出悬浮选择菜单（Box + VStack）；2) 动态从活跃扩展注册表收集所有已注册的 `ExtensionCommandDef`（如 `/tdd`, `/review`, `/simplify`, `/help`）；3) 键盘上下键高亮切换选中项，Tab 或 Enter 一键填入输入框，Esc 取消；4) 键入后续字符时动态模糊过滤候选列表；TDD 覆盖 | 待执行 |
| 090 | 人在回路（Human-in-the-loop）敏感操作模态确认 | 危险操作终端拦截确认：1) 结合 074 `tool_call` 拦截钩子，高危工具调用（write/edit/高危 bash）触发模态拦截；2) 状态机切入 `CONFIRM`，主界面中央弹出模态警告对话框；3) 详细展示工具名、目标文件、命令参数与变更差异预览；4) 提示 `[y] 允许 / [n] 阻止 / [e] 编辑`；响应按键返回确认或拒绝的 tool-result，弹窗关闭后平滑恢复原屏幕；TDD 覆盖 | 待执行 |
| 091 | 全局快捷键调度与 /help 模态帮助屏 | 终端交互快捷操作与手册：1) 全局快捷键：`Ctrl+C`（RUNNING 时向 Agent 发出中断信号；IDLE 且输入空时按两次退出）；`Ctrl+L`（清空行缓冲并全量强制重绘）；`Ctrl+U`（清除当前输入行）；2) `/help` 或 `?` 触发全屏模态帮助面板，展示所有快捷键、可用基础工具、已加载插件列表及当前模型状态；按任意键返回；TDD 覆盖 | 待执行 |
| 092 | TUI 端到端全量自动化集成验收（Virtual Terminal E2E） | 闭环自动化回归：1) 编写 `tests/liii/pi/tui/e2e-test.scm` 与验收脚本 `092.scm`；2) 利用 `(make-virtual-terminal)` 在纯无 TTY 环境下注入按键流，覆盖从启动、输入、Spinner 动画、流式回包、模态拦截放行到退出的完整交互链路；3) 断言虚拟屏幕字符快照、退出码（0）以及终端状态正确恢复；全平台门禁通过 | 待执行 |
| 093 | provider 内 key 级轮换落地：消除 429 限流 | **取证（2026-10-21 任务 078 开发阶段实测）**：全程 `HTTP 429` 限流，日志反复出现 `[pi retry] HTTP 429, attempt 1/12 → 2/12 → 3/12`（退避 2s→4s→8s），计数器**按请求独立重置**故未触顶、产出未受阻，但每次响应首字节延迟被显著拉长。`CLAUDE.md` §5.5 记载任务 033 已建 multikey 密钥池且「限额管理从 provider 级轮换细化到 provider 内 key 级自动轮换——429/403 不再打断流水线阶段任务」，但本机实测 **key 级轮换未生效**（`~/.pi/agent/auth.json` 单 key 形态，429 仍高频复现）。要求：1) 核对 `auth.json` / `models-store.json` 的多 key 配置格式与 `multikey.scm` 的实际接线情况，定位「已建能力未生效」的断点；2) 实现并启用 provider 内 key 级自动轮换——遇 429/403 自动切下一 key，池耗尽才回退到单 key 重试；3) 凭据缺失或多 key 格式非法时**明确报错**（绝不静默成功）；4) TDD 覆盖轮换逻辑（可用 mock provider 注入 429 序列断言切换行为）；5) 补一份可用 key 池配置说明（`README.md` 登记） | 待执行 |

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
