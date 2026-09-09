# PHASE_DESIGN — 相位实例化模型(v3,单线设计)

状态: v3 落地。v2 的 exact-phase 解析已合入;per-level 实例化
已实现(见 §4);v1 补丁(回退链、min-merge、visit-only 判定位)
保持废弃。远期项经 census 无现实用户,保持 deferred(见 §5 末)。

目标:**Racket 式语义、正确实现、无 workaround、无回退多路、
单线代码。**

## 1. 核心原则(三条,各只有一条规则)

### 原则一:绑定身份 = (实例, 相位, 名字)

一次导入把库**实例化**为一个 unit inlet(s7 sublet)里的 cell 集。
同一库被以两个 level 导入 = **两个独立实例** = 体各运行一次、
gensym 各自独立。这是 Racket 的标准行为(required 两次,体跑两次),
不是副作用 bug,是语义。

机制**已经存在**:`call-with-fresh-expand-unit` + 在 inlet 里求值
缓存产物。缓存产物里烘焙的 gensym 在不同 inlet 里是不同 cell——
实例隔离由 gensym 区隔免费获得,不需要任何新环境设施。

### 原则二:解析公式(唯一一条)

在相位 q 解析名字 x 时,对 x 的各导入视图:

> **取 level ≤ q 中 level 最高者;若无任何视图,视为未绑定。**

- plain 导入(level 0):0 ≤ q 恒成立 → **所有相位可见**(substrate
  规则,ergonomics,文档化分歧——Racket 严格版要求显式 for-expand);
- `(for X expand)`(level 1):只在相位 1 可见;
- 同库 multi-level:高 level 在其相位内遮蔽 level 0 —— 一条
  公式,没有扫描、没有合并、没有回退。

Region store 同理**只做精确相位**:store[k] 的定义仅在相位 k 可见。
删除 region-lookup 的跨 store 回退(单线化的一部分)。

### 原则三:library 内部 trans-phase 保留

同一实例内,transformer 体可以引用本库的运行时定义(它们在同一
inlet 里)。这是 goldfish 生态的既有惯用法,**在一个实例内部保留**;
跨实例(两个 level 的两次实例化)天然隔离。=> 既有库零迁移,同时
跨相位隔离是真实的(Racket 一致)。

## 2. 由原则直接导出的语义(全部无补丁)

| 场景 | 语义 |
|---|---|
| `(import X)` | X 实例化为 level 0,体跑一次,绑定相位 0 可见(且按原则二对高相位可见——substrate 规则) |
| `(import (for X expand))` | X 实例化为 level 1,体跑一次(在导入时,即编译期副作用——**Racket 的 for-syntax 正是如此**,体是会运行的),绑定相位 1 可见 |
| 同库 plain + expand 并存 | **两次实例化、体跑两次**;phase-1 代码看到 phase-1 实例(公式取最高 level ≤ 相位);变异互不可见 |
| `(for X (meta 2))` | level 2 实例;相位 2 可见 |
| 嵌套 begin-for-syntax | region store 按 home 相位精确查找,无回退 |

不需要的东西(相对 v1 全部删除):

- ~~visit-only 模式与判定位~~ —— Racket 的 for-syntax 本来就运行
  被导入库的体,不存在"跳过体"的模式;
- ~~add-use min-merge~~ —— 多 level = 多实例,不是合并;
- ~~region-lookup 跨 store 回退~~ —— 精确相位。

## 3. 既有语义的改写(诚实的代价清单)

1. **ewx-6**(begin-for-syntax 内层再包 eval-when (expand)):双 shift
   使 h2 落 store[2],而 m2 的体在 phase 1 —— 旧单 store 掩盖了这一
   相位违规。精确相位下 h2 对 m2 不可见。**测试改写为 Racket 规范形**
   (去掉冗余内层 eval-when,h2 直接定义于 begin-for-syntax)。
2. **case 11(跨 store 回退)**改写:phase-2 的消费必须由注册于
   phase 1 的宏承担(其 RHS 在 phase 2 展开,精确命中 store[2])。
   嵌套 bfs 内定义 define-syntax 的注册相位机制已存在,改写即可。
3. **多 level 导入 = 体跑两次**:依赖"只跑一次"的库需要自查副作用
   (Racket 同性质,文档化)。
4. **level-0 全相位可见**:文档化分歧(ergonomics;严格版要求显式
   for-expand,生态成本过高,不取)。

## 4. 实现面(全部复用已有机制,无新设施)

1. **实例化 = unit**:load 库时 `call-with-fresh-expand-unit` +
   在 inlet 里求值缓存产物(level 0 沿用 rootlet 零迁移;level ≥ 1
   在 unit inlet 求值,registry 持有 inlet 防 GC)。region store、
   缓存记录格式均不变;registry/runtime/being-loaded 按 level 键
   (level 0 裸名,level ≥ 1 为 (level . name)),同库豁免冲突检查;
2. **registry / 视图键**:库实例按 (源库, level) 区分;视图注册
   时携带 level(已实现),去掉 merge;
3. **解析**:exp-library-ref-at-phase 改为"level ≤ q 取最高"的
   单公式;region-lookup 删除,精确 store 查找回归 resolve;
4. **缓存**:零变化(记录按源+管线;实例化在求值端)。

工作量集中在 (2)(3):键结构 + 解析公式,均为局部改动;
unit 包裹在 (1),机制已有。

## 5. 测试改写清单

- ewx-6:去内层冗余 eval-when(Racket 规范形);
- case 11(at2): RHS 精确 store[2] → 20(已验证的结构,保留);
- 新增:multi-level 双实例用例(X 的 plain 实例与 expand 实例
  变异互不可见;体跑两次的副作用计数)
  → 已落地 `tests/expander/import-perlevel-test.scm`(11 checks);
- case 5(ewx-5)保持:per-form 语义不变(需专用设计,本次不做);
- 全量 --all 为最终门 → 2026-09-09 补跑 1552/1552 全绿。
- 远期(phase≥2 缓存分段、visit-only):census 显示零现实用户
  ((meta 2) 仅一处负向测试;visit-only 无 import 语义诉求),
  per-level 通用键已覆盖任意 level 正确性,优化延后。

## 6. 与 Racket 的最终分歧清单(全部为单条规则,非补丁)

| 分歧 | 形式 |
|---|---|
| level 0 视图全相位可见 | substrate 规则(一条) |
| own 定义跨相位可见(实例内) | 同一 inlet 的自然结果(一条) |
| region store 深层定义对浅展开不可见(无回退) | 精确相位(一条) |

每条都是均匀作用于全系统的规则;没有任何"特例 A 走路线 1、
特例 B 走路线 2"的结构。
