# CLAUDE：Claude Code 与 pi agent 的协作规范

> 本文档定义 goldfish 仓库中 Claude Code 的职责边界、命令的存放位置与调用方法。
> 若职责或流程有变化，先改本文档再动工。

## 1. 角色分工（核心约定）

**整体协作只有两方：Claude Code 与 pi。** 不引入任何第三方 agent 或工具。
**Claude Code 负责编辑并 commit 三份计划文档**：`CLAUDE.md`（本文件）、
`Agents.md`（任务规格与后续任务书）、`README.md`（经真实冒烟验证的命令行示例）；
并负责把 pi 拉起来、对 pi 的产出做真实冒烟验收；**不写不改任何实现代码与测试**。
pi 被拉起后全自主运行。

| 两方 | 职责 | 不做什么 |
|---|---|---|
| **Claude Code** | **编辑并 commit 三份计划文档**（`CLAUDE.md`/`Agents.md`/`README.md`）；**按任务生命周期发起 pi**（显式打印各阶段命令、监控执行、异常处置、维护任务表状态与 devel 文档的执行记录）；真实冒烟复核 pi 的验收结论；到达稳定点时同步快照到 `~/git/goldfish2` | 不直接编码（任务三阶段由 pi 执行）；文档 commit 无需编号 |
| **pi（脚本模式）** | **执行任务三阶段**（文档/开发/验收）：按各阶段标准命令被拉起，全自主完成登记建档、TDD 实现、门禁+冒烟验收、commit 与 push；最终目标是被 `bin/gf pi`（自举）替换 | 不修改三份计划文档（文档与协作问题写进输出交回 Claude） |

用户（Da）通过 Claude 下达需求与范围决策；编码、测试、提交、推送在两方之间闭环完成。

一句话：**Claude 控制三份计划文档 + 测试验收 + 拉起 pi；全部代码由 pi 维护。**

> 职责沿革：2026-10-17 初版为"Claude 只编辑 CLAUDE.md、Agents.md 由 pi
> 维护"；2026-10-18 上午改为三份文档归 Claude + "真实冒烟→拉 pi 修复"循环；同日
> 同日稍后裁定 **任务三阶段（文档/开发/验收）由 pi 执行**（Claude 回到发起与
> 监控角色）——此前"Claude 直接编码"的过渡裁定作废；git push 由 pi 负责
> （任务验收阶段内完成）。

## 2. 为什么这样分工

- **限额约束（根本原因）**：Claude Code 背后是 GLM，有 **5 小时用量限额**。编码迭代
  （写测试→跑→修→再跑）与任务规格的反复修订都是 token 消耗大头，全部交给 pi 执行，
  把限额压到最低——Claude 的产出物只有本文件和拉起命令，两者都很短。
- 文档先行：`Agents.md`（任务规格）首版已就绪，架构决策、文件布局、验收标准已固定，
  pi 无需反复询问即可执行，且由 pi 自己维护后续修订。
- pi 脚本模式适合一次性、边界清晰的任务执行；每个阶段一个 `pi -p` 调用，产出可独立验证。
- 计划文档同时是回归依据：后续阶段可以引用同一份文档检查早期约定是否被遵守。

## 3. 命令的存放位置

### 3.1 `gf` 子命令（本仓库自身）

| 内容 | 位置 |
|---|---|
| 子命令注册 | `gfproject.json` 的 `tools` 字段：命令名 → `{organization, module, description}` |
| 子命令实现 | `tools/<命令名>/liii/gold<命令名>.scm`，模块名 `(liii gold<命令名>)`（如 `tools/fmt/liii/goldfmt.scm`） |
| 子命令的辅助模块 | `tools/<命令名>/liii/gold<命令名>-<子模块>.scm`（或 `tools/pi/liii/pi/`） |
| 子命令内置扩展 | `tools/pi/liii/pi/extensions/`，模块名 `(liii pi extensions <扩展名>)` |
| 子命令测试 | `tools/<命令名>/tests/liii/...-test.scm`（用 `bin/gf test` 运行） |
| 可复用 Scheme 库 | `goldfish/liii/<库名>.scm`，模块 `(liii <库名>)` |
| 库测试 | `tests/goldfish/liii/<库名>-test.scm`（用 `bin/gf` 直接运行） |

正在推进的例子：`pi` 子命令与 Extension 机制的完整布局见 `Agents.md` §1 与 §6。

### 3.2 计划文档与任务文档

- `tools/pi/` 目录：`Agents.md`（任务书与回归依据）、`README.md`（实测示例）、
  `CLAUDE.md`（本文档）。
- **任务文档**：`tools/pi/devel/`，每个任务一个 `<编号>.md`（编号对应 Agents.md
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
setsid nohup timeout 7200 pi -a -p @tools/pi/Agents.md "按照 §11 阶段一实现 CLI 骨架，并给出测试结果" \
  > /tmp/pi-goldfish-run.log 2>&1 < /dev/null &

# 管道输入作为上下文
cat tests/goldfish/liii/pi-test.scm | timeout 900 pi -p "补充超时路径的测试用例"

# 指定模型
timeout 900 pi --model sonnet:high -p "实现 §11 阶段二的工具层"
```

要点：
- 一次调用只指派 `Agents.md` 中一个阶段的任务，产出与验收标准一一对应。
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
- **Windows + git-bash 环境的等价做法（2026-10-21 实测修正）**：本机 git-bash
  **无 `pgrep`、无 `setsid`**，`setsid nohup ... &` 的写法不可用。落地约定：
  - 进程存活检查改用 `ps -W | grep "goldfish2/bin/gf"`（`ps -W` 能看到原生
    Windows 进程；注意 scoop 目录下另有同名 `gf.exe`，按路径过滤只认快照执行器）；
  - 脱离会话进程树改用 Claude Code harness 托管的后台任务（`run_in_background`），
    配合 `> /tmp/pi-goldfish-run.log 2>&1` 重定向实现日志轮询，二者效果等价；
  - 外层 `timeout` 必须**大于**内层 `--session-timeout`（否则外层先到期、agent
    拿不到最终报告），阶段模板中的配对值为：文档阶段 1800/1500、开发阶段
    7200/6900、验收阶段 3600/3300；
  - **print 模式（`-p`）的日志在进程运行期间保持 0 字节**，最终报告只在进程退出
    时一次性落盘——故运行期的进展判定只能依据 `git log`/`git status`/文件 mtime，
    不能依据日志长度。
- **死循环实例可提前终止**：若 pi 会话出现确诊的低效死循环——同一动作（同一检索/
  同一 read）重复 3 次以上且 45 分钟以上零提交、零文件改动（阶段六第 1 次、阶段九
  第 1 次均如此，该模式不会自愈）——Claude 可提前终止该实例并从断点缩小任务重拉，
  计为该次执行失败，不等待 timeout 7200 自然到期。这是拉起环节的失败判定，不涉及
  pi 的技术决策干预。
- **实例「在干活」还是「在空等」的精确判据（2026-10-21 实测确立）**：仅看文件 mtime
  与日志长度不足以区分二者——限流退避期间日志持续增长但 CPU 近乎为零。精确做法：
  1) `ps -W | grep "goldfish2/bin/gf"` 的输出列为
     `PID PPID PGID WINPID TTY UID TIME CMD`，**第 4 列是 Windows 侧 PID**；
  2) `powershell -NoProfile -Command "Get-Process -Id <WINPID> | Select-Object CPU"`；
  3) 间隔 10 秒再测一次，**CPU 增量 ÷ 墙钟增量** 即真实占用率：
     - **> 20%** → 正在实做（编译、跑测试、大批量读写），继续等；
     - **< 5%** → 空闲态，**须再结合日志区分两种情形**（二者表象都是「低 CPU +
       无产出」，但处置相反）：
       - **日志有 `[pi retry] HTTP 429/403, attempt N/12`** → 退避空等（见下文
         「429 死亡螺旋」）；
       - **日志为 0 字节、无任何 retry 记录** → 正在**等待一次慢速响应**
         （网络绑定，非空等）——大上下文首包可能需数分钟，属正常，继续等。
  - **注意**：pi 的会话 JSONL **按回合落盘**（一个回合写一行），运行中的工具调用
    不会即时写入，因此**会话文件不能用作实时活动探针**；判别活动请用 CPU 探针 +
    日志 + 文件 mtime 三者交叉，勿单看会话文件行数。
- **阶段切换的拉起竞态（2026-10-21 任务 088 切换时实测）**：拉起下一阶段前虽已确认
  「无滞留实例」，但**上一阶段的实例可能恰在此时退出并写入最终 commit 与报告**——
  拉起瞬间会出现两个实例并存（实测 088 文档阶段实例与开发阶段实例短暂共存 17 秒）。
  - **后果**：一般无害（上一阶段的 commit 与新阶段的改动不重叠时），但若两个实例
    同时操作 git 索引，可能产生 index 冲突或错误提交。
  - **处置**：拉起后**立即复查 `ps -W | grep "goldfish2/bin/gf"`**，若出现两个实例，
    **以更早启动的 PID 为准**（它是上一阶段的收尾实例），等其退出后再推进；不要
    在双实例并存时做任何 git 操作。
  - **更稳的节奏**：上一阶段的后台任务通知（`<task-notification>`）到达后再拉起下一
    阶段，可完全避开该竞态。
  - 实测基线：2026-10-21 任务 087 开发阶段遭遇连续 429，7 分钟零文件写入且 CPU
    仅 1.1%，确认空等；因未达 45 分钟零改动判据故未终止，待其自行恢复。
  - **空等本身即 093（provider 内 key 级轮换）要治的病**：每次重试按请求独立计数
    （`attempt 1/12 → 2/12 → 3/12` 循环出现），单轮退避 2+4+8=14 秒，墙钟被大量
    吞掉，而 `--session-timeout` 正是这部分预算。
- **「429 死亡螺旋」的识别与处置（2026-10-21 任务 087 开发阶段实测确立）**：
  - **现象**：执行器连续 20 分钟只增 `[pi retry] HTTP 429` 而零文件改动、CPU 约
    1%/3min；但**同一 provider/model 的短探测请求却能立即正常响应**。
  - **根因**：配额按**请求体大小**受限（TPM 类），而长会话累积了大量工具结果后，
    单次请求体过大而持续被拒；**每次重试携带同样大的上下文，故重试永不成功**——
    这是死亡螺旋，而非「等一等就会好」。
  - **区分要点**：短探测请求能否通过，是判别「配额窗口关闭」与「请求过大被拒」的
    决定性证据；两者表象相同（都只见 429），处置却相反。
  - **处置**：终止该实例并以**全新会话**重拉同一阶段——新会话上下文更小，请求得以
    通过；**工作区产出按 §5.2 完整保留**，重拉即从断点继续，不重做。
  - **拉起指令应附带上下文节制提示**：「先审阅既有成果再继续，不要从零重写；避免
    一次性读取大量源码或反复重读同一文件」。
  - **实测**：任务 087 开发阶段首启在 20 分钟零产出后被终止重拉，新会话 2 分钟内即
    开始产出（工作区 `thinking.scm` 与测试文件原样保留、未重写）。

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

`Agents.md` 完成后，§4.1 中的 `pi -p` 全部替换为 `bin/gf pi --print`，
即本仓库的编码执行者也是 goldfish 自己的产物（自举）：

```bash
timeout 900 bin/gf pi --print "@tools/pi/Agents.md 实现阶段八：会话持久化"
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

**阶段发起命令**：任务三阶段由 pi 执行，每个阶段发起时 Claude 显式打印并执行
以下命令（将来自举时把 `pi` 替换为 `bin/gf pi`）：

**阶段产物与 commit 对应**：PLAN commit = `tools/pi/devel/ddd.md`（任务文档）；
CODE commit = 代码改动；REVIEW commit = `tools/pi/devel/ddd.scm`（**验收脚本**，
可执行、逐条断言验收点，入库留痕）。REVIEW 阶段也可能顺带修订 `ddd.md`
（补执行记录、按实况修正验收点描述）——两个文件可同入 REVIEW commit。
每个任务在 devel 下留下成对文件。

**验收脚本规范**：ddd.scm 是 Scheme 脚本（`bin/gf tools/pi/devel/ddd.scm` 直接
运行），内部用 `(liii subprocess)` 的 `run` 执行外部命令——**必带 `:timeout`**
（与仓库防卡死规范一致），对每步断言退出码与关键输出（`(liii check)`）。

```bash
# 文档阶段：登记+建档（产物 tools/pi/devel/ddd.md）
timeout 900 pi -a -p @tools/pi/Agents.md "执行任务 ddd 的文档阶段：读 §3 任务表中任务 ddd 的验收点，从 tools/pi/devel/ddd.md 模板建立 tools/pi/devel/ddd.md（填写状态/代码文件/验收点/测试方法），commit 信息 [ddd][provider/model][PLAN] 任务名：建档"

# 开发阶段：TDD 实现（产物为代码改动）
setsid nohup timeout 7200 pi -a -p @tools/pi/Agents.md "执行任务 ddd 的开发阶段（验收点见 tools/pi/devel/ddd.md）：先写测试再实现，commit 信息 [ddd][provider/model][CODE] 任务名：简述，鼓励小步；自己执行的每条命令必须 timeout 包裹且不超过 300 秒；完成后在输出中如实报告各验收点情况" > /tmp/pi-goldfish-run.log 2>&1 < /dev/null &

# 验收阶段：写验收脚本+跑门禁+结算（产物 tools/pi/devel/ddd.scm）
timeout 1800 pi -a -p @tools/pi/Agents.md "执行任务 ddd 的验收阶段：1) 编写验收脚本 tools/pi/devel/ddd.scm（可执行，逐条断言 ddd.md 中的验收点：构建、fmt、check、相关测试、真实冒烟命令与期望输出）；2) 运行该脚本全绿后 commit（[ddd][provider/model][REVIEW] 任务名：验收）并 git push；3) 在输出中逐条报告验收点通过情况"

**阶段 commit 标注（2026-10-21 任务 090 补正）**：三段模板的 commit 信息必须带
`[provider/model]` 标注（`[ddd][google/gemini-3.8-flash][PLAN] ...`），否则 git log
无法作为模型横评数据源。090 的 PLAN commit `4cee4dff` 因模板缺标注而写成
`[090][PLAN]`，已作为已知偏差留痕于 Agents.md 任务表。
```

**自举拉起的强制参数（`bin/gf pi`）**：`gf pi` 有两条**静默截断**的硬预算——
`--session-timeout`（默认 600s 墙钟）与 `--max-turns`（默认 200 轮），触顶即
`Session timeout exceeded` / `Maximum turns limit reached` 截断进程，**最终报告全丢**
（工作区产出保留，但验收依据丢失）。拉起任何阶段必须显式给足：

```bash
# 阶段预算：--session-timeout 给足真实墙钟，--max-turns 0 解除轮次上限
# （把约束交给时间预算，轮次上限退为交互式使用的防失控兜底）
timeout 7200 bin/gf pi -p --session-timeout 5400 --max-turns 0 --timeout 300 ...
```

**为什么 `--max-turns 0` 不可省**：auto-compaction 限的是**上下文长度**（能不能继续聊），
`--max-turns` 限的是**循环次数**（会不会失控）——两者不可互相替代。且压缩**不重置**
turn 计数器（`agent.scm` 循环里 turn 单调递增，压缩只替换消息列表），故压缩救不了
"任务本身需要 200 轮"。反向看，压缩会抹掉"我已经试过 X"的历史、拆掉天然刹车，
轮次上限反而是压缩之后**唯一**的防失控兜底——故代码里必须保留该参数。
根治方向见 `Agents.md` §3 任务 079：把两条预算注入 system prompt 让 agent 可见可自配速。


### 5.1 文档阶段

1. 登记：`Agents.md` §3 任务表新增一行（编号从 001 递增），写明任务名、
   内容与验收点；冒烟缺陷须附复现命令与取证（退出码/stdout/stderr）。
2. 建档：从模板 `tools/pi/devel/ddd.md` 复制建立 `tools/pi/devel/<编号>.md`，
   填写状态（待执行）、涉及的代码文件、验收点、测试方法；建档后 commit
   （`[编号][PLAN] 任务名：建档`）——这是该任务第一个 commit。
3. 依赖检查：前序任务未完成或有依赖关系的任务不启动。

### 5.2 开发阶段

1. **发起时必须在输出中显式打印完整的阶段命令**（即上方模板代入实际编号后的
   命令原文），以及任务编号、任务名、实施要点。
2. TDD：先写测试再实现；**开发阶段不 commit**——只有验收之后的代码才提交，
   工作区改动保留到验收阶段结算。
3. 中断处理：用户叫停或会话中断时，半成品保留在工作区，状态回「待执行」，
   下次从断点继续。

### 5.3 验收阶段

1. 门禁四连：`timeout 600 xmake b goldfish` → `bin/gf fmt` →
   `git diff --exit-code` → `bin/gf check`，加全量相关测试。
2. 真实冒烟：逐条验证任务表中的验收点（涉及网络的用 timeout 包裹）。
3. **验收发起与结果都必须在输出中显式打印**：发起时打印验收命令原文；结果
   打印任务编号、通过或失败、验证依据（哪些验收点过了/没过）。
4. 通过：REVIEW 阶段 commit 两个——先 `[编号][REVIEW] 任务名：代码`（实现
   改动），再 `[编号][REVIEW] 任务名：验收`（devel 验收脚本 + ddd.md 收尾）——
   然后 `git push`（禁止 force push）；任务表状态置「已完成」；devel 文档补
   提交清单与验收记录。每个任务常态三个 commit：PLAN 建档、REVIEW 代码、
   REVIEW 验收；`[编号][CODE]` 仅用于长任务中途保护现场。
   仅改动三份计划文档的 commit 不需要编号。
   **commit message 标注执行模型（用户裁定 2026-10-19）**：格式
   `[编号][provider/model][阶段] 简述`，如 `[027][google/gemini-3.8-flash][CODE] ...`、
   `[027][anthropic/k3][REVIEW] ...`；外部 pi（node 版）执行用 `[pi/...]` 标注
   其报告的实际模型——git log 即模型横评数据源。
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

## 5.5 自举试验（bin/gf pi 逐步接管）

从轻量任务开始，定期尝试让 `bin/gf pi` 替代外部 pi 执行阶段任务（首选**文档
阶段**：读 Agents §3 验收点、写 devel/xxx.md、commit）。

**执行环境选择（用户裁定）**：
- **只写文档的任务**（文档阶段等）→ 在当前仓库 `~/git/goldfish3` 用 `bin/gf pi`
  执行（最新实现做轻活，顺便实战验证）。
- **需要改代码的任务**（开发阶段等）→ 用 `~/git/goldfish2/bin/gf`（稳定快照）
  执行——执行器用稳定版，避免当前仓库迭代中的 bin/gf 影响自己；快照在到达
  稳定点时同步并重新构建。
- **快照机制实测澄清（2026-10-21 任务 076 收尾时核实）**：快照提供的是
  **C++ 运行时与 `gf` 子命令分发**，**不提供 Scheme 库代码**——库按**当前工作
  目录**解析。实测证据：`~/git/goldfish2` 树中既无 `tools/pi/liii/pi/extensions/`
  也无其测试，但在 `~/git/goldfish3` 工作目录下用 `~/git/goldfish2/bin/gf.exe`
  运行 `gf test tools/pi/tests/liii/pi/extensions/` 仍 **3/3 PASS**（加载的是
  goldfish3 的新代码）。因此：
  - 快照的真正保护面是「**当前仓库 C++ 构建处于半改状态时执行器仍可启动**」，
    而**不是**「执行器自带最新 Scheme 实现」；
  - 快照同步（拉取最新代码 + `xmake b goldfish`）仍是必要动作，但它保障的是
    **兜底二进制与其代码树的一致性**，不改变「Scheme 代码取自当前工作目录」
    这一事实；
  - 由此推论：若当前工作目录的 Scheme 代码本身处于损坏状态，快照**救不了**
    该次执行——此时应先回滚工作区再重拉，而非指望换执行器。

- 尝试成功 → 该类任务改由 `bin/gf pi` 执行，逐步扩大范围（文档阶段 → 小修复
  → 开发阶段）。
- **轮换池的本机可用性实测（2026-10-21 任务 078 拉起时核实）**：§5.5 上述轮换池
  是**历史任务在其他环境**积累的清单，**在本机（Windows + git-bash）并非全部可用**。
  逐个实测结果（`gf pi -p --provider X --model Y` 短提示词探测）：
  - `google` / `gemini-3.8-flash` — **可用**（本机唯一稳定可用的 provider）；
  - `minimax` / `MiniMax-M3` — **不可用**：`Error: No credentials found for provider "minimax"`；
  - `kimi-coding` / `kimi-for-coding-highspeed` — **不可用**：`No credentials found`；
  - `zai` / `glm-5.2` — **不可用**：请求已发出但网关拒绝，报 `HTTP 400: JSON parse
    error: Invalid UTF-8 start byte`（回显 `messages[1].content` 解析失败）；
  - `deepseek` / `deepseek-v4-flash` — **不可用**：流式首包即 `HTTP 400 stream error`，
    进程立即退出（该失败形态**不触发** retry，属协议不兼容而非瞬时抖动）。
  - **结论**：本机自举执行**只能用 google/gemini-3.8-flash**；横向模型横评需先补齐
    对应 provider 凭据（或修复协议适配）后才可在本机进行。**拉起前不要盲目轮换**——
    每次无效轮换都会浪费一整轮阶段执行（078 的首次拉起即因此立即失败）。
- **模型轮换（用户裁定 2026-10-19）**：自举执行时可用 `--provider`/`--model`
  指定不同模型完成同类阶段任务，横向实测各模型的实际工作能力；拉起命令与
  devel 执行记录中注明所用 provider/模型，便于事后对比。轮换池：google
  gemini-3.8-flash、`kimi-coding --model k3`（k3 归属 kimi-coding
  provider，用户裁定 2026-10-19——勿用 anthropic 链路；k3 响应比较慢，
  用户裁定 2026-10-19——大体量任务需耐心等待）、
  `deepseek --model deepseek-v4-flash`（029 已入池）、`glm --model glm-5.3-flash`
  （028 已入池——质量高但单次响应慢，适合轻量任务）；任务 030 完成后加入
  minimax m3.1-flash 作为主力（用户裁定 2026-10-19：M3.1 Flash 特别快）。
  **主力偏好（用户裁定 2026-10-19）：多用 minimax、deepseek**——这两家
  速度快；k3/glm 慢，留作对照与兜底。gemini token 预算充足
  （用户裁定 2026-10-19），429 只是短时窗口，可继续承担默认与轮换任务。注：models-store 中 glm 模型实际挂在 `zai-coding-cn`
  provider 下（glm-5.3/5.3-flash 等），deepseek 下有 deepseek-flash/v4-pro，
  minimax 与 minimax-cn 各有 MiniMax-M2.7/M3——028/029/030 的定位线索。
  任务 033（multikey 密钥池）完成后，限额管理从 provider 级轮换细化到
  provider 内 key 级自动轮换——429/403 不再打断流水线阶段任务（027 验收曾因
  kimi-coding 周配额 403 与 google 429 连续中断两次）。
- 尝试失败 → **差距分析是最好的测试用例**：对比 `bin/gf pi` 与 pi 在同一任务上
  的行为差异（哪一步卡住、缺什么能力），把差距登记为缺陷任务，改进 `bin/gf pi`
  后再试——这就是用真实任务驱动自举迭代。
- 试验记录写在对应 devel 文档的执行记录中（执行者标注 bin/gf pi 或 pi）。
- 差距分析结果由 Claude 沉淀到 `README.md` 的「bin/gf pi 与 pi 的差距」一节——
  据此规划 `bin/gf pi` 的后续改进（Claude 据此自主规划 README 的演进）。

## 5.6 收官阶段（用户裁定 2026-10-20）

任务表清空（051/052 完成后流水线停止）时，由 Claude Code 亲自执行收官：

1. **PR 检查**：`da/pi` → `main` 的合并请求（Gitee），确认 CI 全绿；
   CI 失败则定位修复（可按缺陷登记流程立任务或 Claude 直接修非代码项）。
2. **门禁复跑**：`xmake b goldfish` → `bin/gf fmt` → `git diff --exit-code` →
   `bin/gf check` → 全量测试（`bin/gf test tools/pi/tests/liii/` 应全绿，
   051 完成后不再有失败白名单）。
3. **文档收尾**：三份计划文档最终状态对齐（任务表清空注记、差距清单复核）。
4. **定时任务清理**：CronDelete 流水线驱动任务，会话进入纯待命。
5. **收官总结**：向用户报告流水线总战绩（任务数/commit 数/模型横评结论）。


## 5.7 后续工作两阶段推进（用户裁定 2026-10-20）

Extension 机制核心任务 070~075 顺利落地后，后续规划分为两个清晰的战略阶段：

- **第一阶段（Extension 闭环与核心体验/预算修复）**：
  - 任务 076：内置扩展收敛迁移（`simplify`, `red-green`, `code-review`）至 `tools/pi/liii/pi/extensions/`，对齐 `/tdd` 子命令；
  - 任务 077：`pi install <git-url>` 插件一键安装命令；
  - 任务 078：外部扩展实例示范（`compass`, `blueprint`：直接将源码移动到两个相应的扩展目录即可，无需在 GitHub 创建仓库）与端到端实测验证；
  - 任务 079：工具调用 timeout 语义重定（对齐原生 pi + 默认 600s，由模型决定）；
  - 任务 080：会话时限与轮次上限注入系统提示词（避免死线静默截断）。

- **第二阶段（基于 `(liii tui)` 实现 `bin/gf pi` 的 TUI 交互界面）**：
  - 基于刚刚合入主干的 `(liii tui)` 终端 UI 库，实现自包含、沉浸式的原生交互终端界面；
  - 细分为 081~092 共 12 个小任务：TUI 架构骨架与启动路由、状态机与主事件循环驱动器、Editor 多行输入框、MainScreen 差分渲染与 Resize 自适应、流式文本实时打字、Thinking 折叠展开、Tool Card 与动画 Spinner、Slash 悬浮补全菜单、人在回路敏感操作模态确认、会话历史管理、快捷键与 /help 面板、以及虚拟终端确定性 E2E 自动化回归。

## 6. 给 pi 脚本模式的硬性要求

- 执行的每一条命令都要设置超时；被超时杀掉的命令按失败处理，不要重试超过两次。
- **ANSI 转义序列书写约定（2026-10-21 实测澄清，防误判）**：本仓库 Scheme 读取器的
  十六进制转义 `\xNN` **会消费尾随的 `;`**，故 `(liii tui)` 库源码中 ANSI 序列写作
  `"\x1b;[K"`、`"\x1b;[?2026h"` 这类形式时，**运行时字节是正确的**
  （实测 `\x1b;[?2026h` 为 `27 91 63 50 48 50 54 104` = `ESC[?2026h`），**并非缺陷**。
  - **判断 ANSI 序列是否正确必须验运行时字节，不能只看源码字面**——Claude 曾据字面
    误判为「转义序列畸形」，经字节级验证后排除（该误判与更正已在 Agents.md 任务表
    086 行的历史留痕中，表格滚动出表后仍可于 commit history 查证）。
  - 修改 `(liii tui)` 库或 pi 的 TUI 渲染代码时，**不要**把这些 `;` 当作笔误删除或
    「修正」——删掉后 `\x1b[` 形式同样正确，但随意增删可能落到错误位置而静默失效；
  - 虚拟终端只记录原始字符串、**不校验序列合法性**，故虚拟终端单测**无法发现转义
    序列畸形**；涉及 ANSI 的断言应对**运行时缓冲区**做字节级核对。
- **禁止 force push**：不使用 `git push -f` / `--force-with-lease`，不 amend 或 rebase
  已推送过的提交；错误修正一律追加新提交，完整保留 commits 历史。
- 测试一律写入 `tests/` 或 `tools/<命令名>/tests/` 下的版本控制文件。
- **禁止修改三份计划文档**（2026-10-20 注：任务 039 验收执行器曾越权维护任务表
  并擅自登记 051/052——内容虽合理被保留，但程序违规；此后执行器发现值得登记的
  任务/状态变更时，一律写进输出报告交回 Claude 处理，不直接改 Agents.md）。
- 决策分两类处理：**技术规格问题**（实现细节、验收标准调整）由 pi 自行决策、修订
  `tools/pi/devel/<编号>.md`（任务文档，属 pi 可改范围）并 commit 记录理由；**协作流程/分工问题**（本文件的约定）才停止执行，
  把问题写进输出交回 Claude——Claude 编辑并 commit 本文件后重新拉起 pi 继续。
- 产出报告必须如实说明：哪些验收标准通过、哪些未通过、跳过了什么。
