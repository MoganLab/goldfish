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
| **Claude Code** | **编辑并 commit 三份计划文档**（`CLAUDE_CODE_PLAN.md`/`PI_PLAN.md`/`PI_SPEC.md`）；真实冒烟测试 `bin/gf pi`；**直接编码完成 `PI_PLAN.md` §3 任务表的任务**（2026-10-18 用户裁定：pi 退出执行链，Claude 直接完成迁移/重构/Kimi 等任务）；到达稳定点时同步快照到 `~/git/goldfish2` | （原"不写不改代码"约束已由用户于 2026-10-18 解除；文档 commit 无需编号，代码 commit 用 `[编号]` 格式） |
| **pi（脚本模式）** | 已退出执行链（保留本行记录模式沿革）。如将来恢复：被拉起后全自主完成实现与测试 | — |

用户（Da）通过 Claude 下达需求与范围决策；编码、测试、提交、推送在两方之间闭环完成。

一句话：**Claude 控制三份计划文档 + 测试验收 + 拉起 pi；全部代码由 pi 维护。**

> 职责沿革：2026-10-17 初版为"Claude 只编辑 CLAUDE_CODE_PLAN.md、PI_PLAN.md 由 pi
> 维护"；2026-10-18 上午改为三份文档归 Claude + "真实冒烟→拉 pi 修复"循环；同日
> 用户裁定 **Claude 直接编码完成任务**（pi 退出执行链），git push 由 Claude 一并
> 负责（原"推送由 pi 负责"随之失效）。

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

### 3.2 计划文档与任务文档

- 仓库根目录：`PI_PLAN.md`（任务书与回归依据）、`PI_SPEC.md`（实测示例）、
  `CLAUDE_CODE_PLAN.md`（本文档）。
- **任务文档**：`tools/pi/devel/`，每个任务一个 `<编号>.md`（编号对应 PI_PLAN.md
  §3 任务表），模板见 `tools/pi/devel/ddd.md`——pi 执行任务时从模板复制并填写
  （随任务 commit 入库）；Claude 在验收后补充「执行与验收记录」。
- 仓库根目录 `devel/` 已停止维护，不再新增任务文档。

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

## 5. 任务生命周期（三阶段）

每个任务依次经过**文档阶段 → 开发阶段 → 验收阶段**：

```
创建 ──→ 待执行 ──→ 【文档阶段】──→ 【开发阶段】──→ 【验收阶段】──→ 已完成
                       │                │               │
                       │                │               └─ 失败/回归 → 已回退 →（重做）→ 开发阶段
                       │                │                  └─ 连续失败 10 次 → 回文档阶段修订任务定义
                       │                │                     └─ 修订后整轮仍失败累计 3 轮 → 放弃任务，继续下一个
                       │                └─ 用户叫停/中断 → 半成品保留，状态回「待执行」
                       └─ 依赖未满足或需用户确认 → 搁置
```

**阶段发起命令**：每个阶段发起时使用明确的命令（当前执行者为 Claude 直接编码；
以下 pi 命令是各阶段的标准发起形式，将来自举时把 `pi` 替换为 `bin/gf pi`）：

```bash
# 文档阶段：登记+建档
timeout 900 pi -a -p @PI_PLAN.md "执行任务 ddd 的文档阶段：读 §3 任务表中任务 ddd 的验收点，从 tools/pi/devel/ddd.md 模板建立 tools/pi/devel/ddd.md（填写状态/代码文件/验收点/测试方法）并 commit"

# 开发阶段：TDD 实现（长任务，脱离进程树）
setsid nohup timeout 7200 pi -a -p @PI_PLAN.md "执行任务 ddd 的开发阶段（验收点见 tools/pi/devel/ddd.md）：先写测试再实现，工作区累积改动不 commit，完成后如实报告各验收点情况" > /tmp/pi-goldfish-run.log 2>&1 < /dev/null &

# 验收阶段：门禁+冒烟+结算
timeout 1800 pi -a -p @PI_PLAN.md "执行任务 ddd 的验收阶段：按 tools/pi/devel/ddd.md 的测试方法依次通过 xmake b goldfish、bin/gf fmt、git diff --exit-code、bin/gf check 与相关测试，逐条验证验收点，通过则 commit（[ddd] 任务名：简述）并 git push，如实报告验收结论"
```

### 5.1 文档阶段

1. 登记：`PI_PLAN.md` §3 任务表新增一行（编号从 001 递增），写明任务名、
   内容与验收点；冒烟缺陷须附复现命令与取证（退出码/stdout/stderr）。
2. 建档：从模板 `tools/pi/devel/ddd.md` 复制建立 `tools/pi/devel/<编号>.md`，
   填写状态（待执行）、涉及的代码文件、验收点、测试方法。
3. 依赖检查：前序任务未完成或有依赖关系的任务不启动。

### 5.2 开发阶段

1. **发起时必须在输出中显式打印完整的阶段命令**（即上方模板代入实际编号后的
   命令原文），以及任务编号、任务名、实施要点。
2. TDD：先写测试再实现；工作区累积改动，中途不 commit（一个任务编号对应
   一个 commit）。
3. 中断处理：用户叫停或会话中断时，半成品保留在工作区，状态回「待执行」，
   下次从断点继续。

### 5.3 验收阶段

1. 门禁四连：`timeout 600 xmake b goldfish` → `bin/gf fmt` →
   `git diff --exit-code` → `bin/gf check`，加全量相关测试。
2. 真实冒烟：逐条验证任务表中的验收点（涉及网络的用 timeout 包裹）。
3. **验收发起与结果都必须在输出中显式打印**：发起时打印验收命令原文；结果
   打印任务编号、通过或失败、验证依据（哪些验收点过了/没过）。
4. 通过：一次性 commit（信息 `[编号] 任务名：简述`）并 `git push`（禁止
   force push）；任务表状态置「已完成」；devel 文档补提交清单与验收记录。
   仅改动三份计划文档的 commit 不需要编号。
5. 失败：任务状态置「已回退」并注明原因与重试次数（重做在 commit history
   如实保留），按失败点修正后重新进入开发阶段。
6. **升级到文档阶段**：开发→验收循环连续失败达 **10 次**，说明问题很可能出在
   任务定义本身（验收点不可验证、范围不合理、前置假设错误）——回到文档阶段
   修订任务文档与验收点（必要时拆分或重定义任务），再重新进入开发阶段；
   每次失败都须在 devel 文档的执行记录中留痕。
7. **放弃任务**：修订后的「文档→开发→验收」完整循环再失败，累计达 **3 轮**者，
   放弃该任务——状态置「已放弃」并在任务表与 devel 文档注明原因（含已尝试的
   方向），编号不回收；继续执行下一个任务。后续若条件变化（新信息、新依赖就绪）
   可用新编号重新立项。

## 6. 给 pi 脚本模式的硬性要求

- 执行的每一条命令都要设置超时；被超时杀掉的命令按失败处理，不要重试超过两次。
- **禁止 force push**：不使用 `git push -f` / `--force-with-lease`，不 amend 或 rebase
  已推送过的提交；错误修正一律追加新提交，完整保留 commits 历史。
- 测试一律写入 `tests/` 或 `tools/<命令名>/tests/` 下的版本控制文件。
- 决策分两类处理：**技术规格问题**（实现细节、验收标准调整）由 pi 自行决策、修订
  `PI_PLAN.md` 并 commit 记录理由；**协作流程/分工问题**（本文件的约定）才停止执行，
  把问题写进输出交回 Claude——Claude 编辑并 commit 本文件后重新拉起 pi 继续。
- 产出报告必须如实说明：哪些验收标准通过、哪些未通过、跳过了什么。
