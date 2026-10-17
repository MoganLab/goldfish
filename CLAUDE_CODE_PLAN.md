# CLAUDE_CODE_PLAN：Claude Code 与 pi agent 的协作规范

> 本文档定义 goldfish 仓库中 Claude Code 的职责边界、命令的存放位置与调用方法。
> **本文件是 Claude Code 唯一有权编辑的文件。** 若职责或流程有变化，先改本文档再动工。

## 1. 角色分工（核心约定）

**整体协作只有两方：Claude Code 与 pi。** 不引入任何第三方 agent 或工具。
**Claude Code 只负责编辑 `CLAUDE_CODE_PLAN.md` 这一个文件**（即本文件：协作规范与
拉起方法），并负责把 pi 拉起来；**不编辑 `PI_PLAN.md`，不写实现代码，除此之外不做
任何干预**。pi 被拉起后全自主运行。

| 两方 | 职责 | 不做什么 |
|---|---|---|
| **Claude Code** | **只编辑并 commit `CLAUDE_CODE_PLAN.md`（本文件）**；用 `pi -p` 拉起 pi | 不编辑、不提交 `PI_PLAN.md`（其维护权在 pi），不改实现代码，不审查产出，不代替提交，**不负责 git push（推送由 pi 负责）** |
| **pi（脚本模式）** | 被拉起后**全自主**：编辑并 commit `PI_PLAN.md`（任务规格，由 pi 自己负责），完成实现、测试、格式化、提交；并**负责定期推送到 GitHub**——每完成一个阶段或一批小提交就 `git push`，不积压在本地 | 不修改 `CLAUDE_CODE_PLAN.md`（协作规范的修订写进输出，交回 Claude 编辑并提交本文件） |

用户（Da）同样**不做任何干预**：编码、测试、提交、推送全部在两方之间闭环完成。

一句话：**Claude 只编辑本文件并拉起 pi；`PI_PLAN.md` 与全部代码由 pi 维护。**

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
# 把计划文档作为输入，指派某个阶段（@文件 会把内容包含进 prompt）
timeout 900 pi -p "@PI_PLAN.md 按照 §11 阶段一实现 CLI 骨架，并给出测试结果"

# 管道输入作为上下文
cat tests/goldfish/liii/pi-test.scm | timeout 900 pi -p "补充超时路径的测试用例"

# 指定模型
timeout 900 pi --model sonnet:high -p "实现 §11 阶段二的工具层"
```

要点：
- 一次调用只指派 `PI_PLAN.md` 中一个阶段的任务，产出与验收标准一一对应。
- pi 直接在任务分支（当前 `da/pi`）上提交，不进入主干；主干隔离靠分支 + 合并请求。
- 超时时长按任务体量取 300–1800s；超时即视为该次执行失败，缩小任务重试。

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

1. **计划**：`PI_PLAN.md`（任务规格）由 pi 自己负责——编辑、修订与 **commit** 都由
   pi 完成；Claude 不编辑、不提交它。`CLAUDE_CODE_PLAN.md`（本文件）由 Claude
   负责编辑与 commit。
2. **拉起**：Claude 用 `pi -p` 把 pi 拉起，一次指派 `PI_PLAN.md` §11 的一个阶段；
   从这一刻起 Claude 不再干预，由 pi 全自主执行。
3. **实现与验证**：pi 按 `PI_PLAN.md` §10 跑构建、格式化、`bin/gf check` 与测试；
   测试代码必须保存在 `tests/` 下的版本控制文件中（禁止 /tmp 临时文件、
   禁止 `bin/gf -e` 做 adhoc 测试）。执行中发现规格要改，pi 直接修订并 commit
   `PI_PLAN.md`，同时在 §13 偏差清单登记理由。**阶段表（`PI_PLAN.md` §11）由 pi
   自由更新**——可增删、拆分、合并、重排阶段，无需回到 Claude；Claude 每次拉起前
   以当时的 §11 为准选取下一个未完成阶段。
4. **提交与推送**：分支固定使用 `da/pi`（不走 `$USER/dddd/descr` 命名规则），提交遵循
   `PI_PLAN.md` §11.1（小提交；每个 commit 必须通过 `bin/gf fmt` 与 `bin/gf check`），
   提交信息 `[x_y] 简短描述`；推送由 pi 负责——每完成一个阶段或一批小提交就
   `git push`（首次 `-u origin da/pi`），不积压在本地，并按输出中的链接创建合并请求。
5. **归档**：只有涉及协作规范/分工的问题才交回 Claude，由 Claude 编辑并 commit
   本文件；其余全部在 pi 侧闭环。

## 6. 给 pi 脚本模式的硬性要求

- 执行的每一条命令都要设置超时；被超时杀掉的命令按失败处理，不要重试超过两次。
- **禁止 force push**：不使用 `git push -f` / `--force-with-lease`，不 amend 或 rebase
  已推送过的提交；错误修正一律追加新提交，完整保留 commits 历史。
- 测试一律写入 `tests/` 或 `tools/<命令名>/tests/` 下的版本控制文件。
- 决策分两类处理：**技术规格问题**（实现细节、验收标准调整）由 pi 自行决策、修订
  `PI_PLAN.md` 并 commit 记录理由；**协作流程/分工问题**（本文件的约定）才停止执行，
  把问题写进输出交回 Claude——Claude 编辑并 commit 本文件后重新拉起 pi 继续。
- 产出报告必须如实说明：哪些验收标准通过、哪些未通过、跳过了什么。
