# CLAUDE_CODE_PLAN：Claude Code 与 pi agent 的协作规范

> 本文档定义 goldfish 仓库中 Claude Code 的职责边界、命令的存放位置与调用方法。
> 若职责或流程有变化，先改本文档再动工。

## 1. 角色分工（核心约定）

**整体协作只有两方：Claude Code 与 pi。** 不引入任何第三方 agent 或工具。
**Claude Code 负责编辑并 commit 三份计划文档**：`CLAUDE_CODE_PLAN.md`（本文件）、
`PI_PLAN.md`（任务规格与后续任务书）、`PI_SPEC.md`（经真实冒烟验证的命令行示例）；
并负责把 pi 拉起来、对 pi 的产出做真实冒烟验收；**不写不改任何实现代码与测试**。
pi 被拉起后全自主运行。

| 两方 | 职责 | 不做什么 |
|---|---|---|
| **Claude Code** | **编辑并 commit `CLAUDE_CODE_PLAN.md`、`PI_PLAN.md`、`PI_SPEC.md` 三份文档**；用 `setsid pi -p` 拉起 pi；真实冒烟测试 `bin/gf pi` 并把发现的缺陷作为任务拉起 pi 修复；到达稳定点时同步快照到 `~/git/goldfish2` | 不写、不改任何实现代码与测试；不代替 pi 提交代码；**不负责 git push（推送由 pi 负责）** |
| **pi（脚本模式）** | 被拉起后**全自主**：按任务完成实现、测试（TDD）、格式化、小步提交；**负责定期推送到 GitHub**——每完成一批小提交就 `git push`，不积压在本地 | 不修改三份计划文档（文档问题与协作规范的修订写进输出，交回 Claude 处理） |

用户（Da）通过 Claude 下达需求与范围决策；编码、测试、提交、推送在两方之间闭环完成。

一句话：**Claude 控制三份计划文档 + 测试验收 + 拉起 pi；全部代码由 pi 维护。**

> 职责沿革：2026-10-17 初版为"Claude 只编辑 CLAUDE_CODE_PLAN.md、PI_PLAN.md 由 pi
> 维护"；2026-10-18 用户指示三份文档归 Claude 控制、并转入"真实冒烟测试→拉 pi 修复"
> 循环（PI_PLAN §11 九个阶段已全部完成后）。

## 2. 为什么这样分工

- **限额约束（根本原因）**：Claude Code 背后是 GLM，有 **5 小时用量限额**。编码迭代
  （写测试→跑→修→再跑）与任务规格的反复修订都是 token 消耗大头，全部交给 pi 执行，
  把限额压到最低——Claude 的产出物只有本文件和拉起命令，两者都很短。
- 文档先行：`PI_PLAN.md`（任务规格）首版已就绪，架构决策、文件布局、验收标准已固定，
  pi 无需反复询问即可执行，且由 pi 自己维护后续修订。
- pi 脚本模式适合一次性、边界清晰的任务执行；每个阶段一个 `pi -p` 调用，产出可独立验证。
- 计划文档同时是回归依据：后续阶段可以引用同一份文档检查早期约定是否被遵守。

## 3. 命令的存放位置

### 3.1 `gf` 子命令（本仓库自身）

| 内容 | 位置 |
|---|---|
| 子命令注册 | `gfproject.json` 的 `tools` 字段：命令名 → `{organization, module, description}` |
| 子命令实现 | `tools/<命令名>/liii/gold<命令名>.scm`，模块名 `(liii gold<命令名>)`（如 `tools/fmt/liii/goldfmt.scm`） |
| 子命令的辅助模块 | `tools/<命令名>/liii/gold<命令名>-<子模块>.scm` |
| 子命令测试 | `tools/<命令名>/tests/liii/...-test.scm`（用 `bin/gf test` 运行） |
| 可复用 Scheme 库 | `goldfish/liii/<库名>.scm`，模块 `(liii <库名>)` |
| 库测试 | `tests/goldfish/liii/<库名>-test.scm`（用 `bin/gf` 直接运行） |

正在推进的例子：`pi` 子命令的完整布局见 `PI_PLAN.md` §4.1。

### 3.2 计划文档

- 仓库根目录：`PI_PLAN.md`（pi 脚本模式的任务规格）、`CLAUDE_CODE_PLAN.md`（本文档）。
- `devel/` 目录已停止维护，不再新增任务文档。

## 4. 命令的调用方法

**所有命令都必须设置超时**（用 shell 的 `timeout <秒>` 包裹），避免交互式等待或
网络/构建卡死导致流程挂起。以下命令均已按此约定书写。

### 4.1 用 pi 脚本模式驱动编码

```bash
# 把计划文档作为输入，指派某个阶段
# 注意：@文件 必须是独立的位置参数（整个参数以 @ 开头），不能写在 -p 的引号字符串里——
# 否则整串会被当作文件路径而报 "File not found"（参考 ~/git/dev-agent tools/autocode 的拉起方式）
setsid nohup timeout 7200 pi -a -p @PI_PLAN.md "按照 §11 阶段一实现 CLI 骨架，并给出测试结果" \
  > /tmp/pi-goldfish-run.log 2>&1 < /dev/null &

# 管道输入作为上下文
cat tests/goldfish/liii/pi-test.scm | timeout 900 pi -p "补充超时路径的测试用例"

# 指定模型
timeout 900 pi --model sonnet:high -p "实现 §11 阶段二的工具层"
```

要点：
- 一次调用只指派 `PI_PLAN.md` 中一个阶段的任务，产出与验收标准一一对应。
- `-a` 信任本项目级上下文文件（CLAUDE.md 等）；`-p` 是布尔 flag（print 模式），
  任务文本作为普通位置参数或 `@文件` 位置参数传入。
- pi 直接在任务分支（当前 `da/pi`）上提交，不进入主干；主干隔离靠分支 + 合并请求。
- 超时时长按任务体量设置，拉起编码任务用 `timeout 7200`（两小时）；超时即视为该次
  执行失败，缩小任务重试。
- **长任务拉起必须脱离 Claude 会话进程树**：用 `setsid nohup ... > /tmp/pi-goldfish-run.log
  2>&1 < /dev/null &` 拉起，pi 的最终报告落在该日志里。完成判定不依赖会话内后台
  任务通知，而是定期检查 `pgrep -f "pi -a -p"`（进程是否存活）、`git log/status`
  （产出是否落地）与日志尾部（最终报告）。
  - 起因（2026-10-17 故障记录）：会话内 `run_in_background` 拉起的 pi 在阶段五连续
    3 次被外部终止（存活 13/2/1 分钟，均非 timeout 7200 到期，被杀时 pi 正常干活），
    同期短任务（9~30 分钟）可正常完成。改为脱离进程树 + 日志轮询后不再依赖会话
    生命周期。
- **死循环实例可提前终止**：若 pi 会话出现确诊的低效死循环——同一动作（同一检索/
  同一 read）重复 3 次以上且 45 分钟以上零提交、零文件改动（阶段六第 1 次、阶段九
  第 1 次均如此，该模式不会自愈）——Claude 可提前终止该实例并从断点缩小任务重拉，
  计为该次执行失败，不等待 timeout 7200 自然到期。这是拉起环节的失败判定，不涉及
  pi 的技术决策干预。

### 4.2 构建 / 格式化 / 测试（pi 与人工通用）

```bash
# 构建
timeout 600 xmake b goldfish

# 格式化（整仓库执行即可，自动跳过缓存文件）
timeout 120 bin/gf fmt

# 运行库测试
timeout 120 bin/gf tests/goldfish/liii/pi-test.scm

# 运行 tools 目录下的测试
timeout 300 bin/gf test tools/pi/tests/liii/goldpi-args-test.scm

# 新增或修改测试文件后重建文档索引
timeout 120 bin/gf doc --build-json
```

### 4.3 目标态：`bin/gf pi` 替代 pi 脚本模式

`PI_PLAN.md` 完成后，§4.1 中的 `pi -p` 全部替换为 `bin/gf pi --print`，
即本仓库的编码执行者也是 goldfish 自己的产物（自举）：

```bash
timeout 900 bin/gf pi --print "@PI_PLAN.md 实现阶段八：会话持久化"
```

## 5. 协作流程（两方闭环）

1. **计划**：三份计划文档（`CLAUDE_CODE_PLAN.md`/`PI_PLAN.md`/`PI_SPEC.md`）由
   Claude 编辑与 commit；`PI_PLAN.md` §3 任务表是任务书，编号从 001 递增，
   Claude 维护其状态列（待执行/进行中/已完成/已回退），回退属正常现象、
   重做过程在 commit history 如实保留。
2. **拉起**：Claude 取任务表中**最靠前的待执行任务**，用 `setsid pi -p` 拉起。
   **每次发起任务都必须在输出中明确打印**：任务编号、任务名、指派要点。
   从拉起这一刻起 Claude 不干预，pi 全自主执行（TDD、小步提交、fmt/check 门禁、
   命令一律 timeout）。
3. **提交与推送**：一个任务编号对应一个 commit，信息格式 `[编号] 任务名：简述`；
   仅改动三份计划文档的 commit 不需要编号。推送由 pi 负责，不积压在本地，
   禁止 force push。
4. **验收**：pi 退出后，Claude 依据任务验收点做真实冒烟复测与 git 核实
   （commit 与 push 落地、工作区干净）。**验收结果必须在输出中明确打印**：
   任务编号、通过或失败、验证依据（哪些验收点过了/哪些没过）。通过→任务表状态
   置「已完成」并取下一个任务；失败→把未通过点作为新任务重新拉起（同一任务
   最多 4 次执行），失败情况同样明确打印。
5. **异常处置**：pi 死循环（同一调用重复 3 次以上且 45 分钟零产出）或挂死
   （会话活动停止 20+ 分钟但进程存活）→ 终止实例，从断点缩小任务重拉。
6. **升级**：协作规范/分工问题由 Claude 修订本文件后继续；技术规格问题由
   Claude 修订 `PI_PLAN.md`/`PI_SPEC.md` 后继续；其余全部在 pi 侧闭环。

## 6. 给 pi 脚本模式的硬性要求

- 执行的每一条命令都要设置超时；被超时杀掉的命令按失败处理，不要重试超过两次。
- **禁止 force push**：不使用 `git push -f` / `--force-with-lease`，不 amend 或 rebase
  已推送过的提交；错误修正一律追加新提交，完整保留 commits 历史。
- 测试一律写入 `tests/` 或 `tools/<命令名>/tests/` 下的版本控制文件。
- 决策分两类处理：**技术规格问题**（实现细节、验收标准调整）由 pi 自行决策、修订
  `PI_PLAN.md` 并 commit 记录理由；**协作流程/分工问题**（本文件的约定）才停止执行，
  把问题写进输出交回 Claude——Claude 编辑并 commit 本文件后重新拉起 pi 继续。
- 产出报告必须如实说明：哪些验收标准通过、哪些未通过、跳过了什么。
