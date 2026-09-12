# PHASE_DESIGN — 相位实例化模型(v5,own 绑定严格相位)

状态: v5 已落地。目标：**Racket 式语义、正确实现、无 workaround、无回退多路、
单线代码。**与 Racket 的唯一分歧见 §5；phase≥2 缓存分段 deferred。

## 1. 核心原则(三条,各只有一条规则)

### 原则一:绑定身份 = (实例, 相位, 名字)

一次导入把库**实例化**为一个 unit inlet(s7 sublet)里的 cell 集。
同一库被以两个 level 导入 = **两个独立实例** = 体各运行一次、
gensym 各自独立（Racket 语义，非副作用 bug）。

机制:`call-with-fresh-expand-unit` + 在 inlet 里求值缓存产物；
registry/runtime/being-loaded 按 level 键(level 0 裸名，
level ≥ 1 为 (level . name))；level ≥ 1 实例不注册运行时模块。

### 原则二:解析公式(唯一一条)

在相位 q 解析名字 x 时,对 x 的各导入视图:

> **level 0 的视图在任意相位都是候选;level n > 0 的视图只在相位 n
> 是候选;候选中取 level 最高者,若无任何视图,视为未绑定。**

- plain 导入(level 0):**所有相位可见**(substrate 规则，唯一保留分歧，见 §5)；
- `(for X expand)`(level 1):只在相位 1 可见；
- 同库 multi-level:高 level 遮蔽 level 0；
- `(for X run expand)` 多 level:逐 level 各注册一视图并各自实例化（并集）。

Region store 只做精确相位:store[k] 的定义仅在相位 k 可见。

### 原则三:own 绑定严格相位

同实例 own 绑定按种类定相位:

- 值 `define` → **仅 phase 0 可见**;
- 宏 `define-syntax` → phase ≥ 1 可见（transformer 绑定是派发关键字，
  全相位可见；transformer 体在 phase ≥1 经它派发）;
- region define(eval-when (expand) / begin-for-syntax)→ 仅 home 相位可见。

展开期助手只有两条相位正确的来路：本库 region，或另一库的 plain 导入。
**没有相位上限，也没有逐相位塔**（逐相位塔已否决，勿复活）；
transformer 体内标识符在定义相位解析一次，之后命中同一 binding。

## 2. 由严格化直接得到的语义(单一行为,各路径一致)

| 场景 | 语义 |
|---|---|
| transformer 体引用本库 phase-0 值 define | 展开期 unbound-variable（编译期错误） |
| ewx-5(eval-when (expand) 中 set! 运行期变量) | 同上，编译期拒绝 |
| 库体内引用被 level 门挡掉的导入名 | 展开期 unbound-variable |
| REPL / per-form 路径 | 同一规则 |

## 3. 相位正确的助手写法（三种机制）

- **primitive binding**：展开期装置（parse-template、syntax-case-dispatch 等）
  注册为相位无关的裸名，供 transformer 体调用；
- **内联**：小助手集合内联为 transformer 的 `letrec*`（syntax-case 自包含惯用法）；
- **拆分导入**：展开期函数拆为一等库（如 `(goldfish match expansion)`），
  使用方 plain 导入（substrate 规则 → 全相位）。

## 4. 测试

- ewx-5 断言 `check-catch 'unbound-variable`；transformer 引 own 值 → 错，
  region 助手 → 成，substrate 导入 → 成，program/library 两路径同断言；
- per-level 双实例、精确相位与多 level 并集、库体 for-gated miss 均覆盖；
- 全量 `--all` 为最终门。

## 5. 与 Racket 的分歧清单(唯一一条)

| 分歧 | 形式 |
|---|---|
| level 0 视图全相位可见 | substrate 规则(一条) |

其余（own 严格相位、region 精确相位、level 公式、多 level 多实例）均与 Racket
一致。移除 substrate 即完全一致，但它承托全部 transformer ergonomics，
明确不取。
