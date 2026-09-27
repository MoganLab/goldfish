# Goldfish Runtime Contract

状态：R0–R3 已完成；R4 进行中。Native 已覆盖 kernel 自举、普通库的
source/cache 闭环、CLI/REPL，以及 C3 固定工作流。默认入口 `bin/gf` 已
切换为 native；host/s7 保留在 `bin/gf-host`。删除 s7/gf0 过渡层是单独的后续步骤。
本文定义替换 vendored s7 后的宿主边界；现有 `gf0`/s7 bridge 不是此合同
的一部分，只是过渡实现。

### R3 收口记录（2026-09-24）

- 冷启动路径：`tools/test-native-cold-bootstrap.sh` 与 default/r7rs 两模式
  `-e '(+ 20 22)'` 均返回 42；`tools/test-native.sh` 套件通过。
- 单元：`native-reader/evaluator/dependency/source-bootstrap-test` 通过。
  `native-library-source-test` 需要 `vbootstrap0` 预热缓存（不在
  test-native.sh 门禁内），缺缓存时 abort。（已修复：见准备清单第 4 条）
- 宿主回归：`gf test tests/scheme/eval` 2/2、`import-perlevel` 1/1、
  reader 301/1（NaN round-trip 为 HEAD 既有失败）+ write-roundtrip 9/9。
- 关键修复：`(scheme eval)` 的 `%s7-eval` 改经私有名 `%host-eval` 解析
  host evaluator，消除 import 后的自递归；`g-native-trace/dump` 探针已移除；
  `liii/prelude.scm` 括号修正后以**不含 `cond` 的 9 宏形态**落地
  （`when and or case let let* do let-values let*-values`）——real `cond`
  在 native 的 install.scm 展开路径触发 `expected proper list`，占位
  `cond`（bootstrap-prelude）+ 无 `cond` 的 prelude 使双端同时通过。
  （该绕行已废弃，根因见准备清单第 3 条。）

### R4 准备清单

1. 默认 `gf` 入口切换到 native runtime（完成）；host/s7 由显式 `gf-host` 入口调用，gf0/s7 仍待移除。
2. 删除 `src/s7*`、s7 构建目标与 vendored s7；清理 `gf0` bridge 与
   `bootstrap_compatibility` 中仅过渡用的 LegacyLet 分支。
3. [完成 2026-09-24] 根因不在 install.scm，而在 prelude 的**定义顺序**：
   `define-syntax` 的 transformer 在 prelude 加载时即展开，而 `let` 定义在
   `cond`/`case` 之后，transformer 里的 named let 因此原样漏进 core
   evaluator（core `let` 不支持 named let）→ `expected proper list`。
   修复：`let` 提到 prelude 首位并在文件头写明顺序约束，prelude 恢复
   10 宏；bootstrap-prelude 占位宏（会展开成 `#t` 的假 `cond`）随之
   清空，native 冷启动与 host 均绿。
4. [完成] `tools/warm-bootstrap-cache.sh` 使用默认 native `gf` 预热 native
   cache；`tools/test-native.sh` 覆盖 native reader、library source 和
   cold-bootstrap 检查。R4 默认切换后，预热和冷启动仍须在不调用 host `gf`
   的情况下成立。
5. [待切换前复核] 汇总现有 C2 strict parity、M3 lowered-program 检查和
   C3 native gate；明确历史结果的范围及未覆盖项。`diff-gf0-m2a.sh` 是
   s7/gf0 迁移期工具，删除前需先确认它的剩余用途和调用者，不将其 skip
   清单误作 native C3 验收结果。
6. [C1 收尾 2026-09-26] tests/expander 目录级常规 18/19 绿：唯一失败
   host-abi-load（`1.5` 字面量）归浮点桶；带浮点/复数字面量的测试共
   220 个文件（含 liii/reader-test 的53处字面量、lib-cache-all-libs
   的 `sqrt`/`random-state` 缺口——`random` 在 primitive-variables 中
   但 native 未实现）。s7 rootlet 面（with-let/sublet/unlet/let-set!/
   *s7*/load-expanded/le-rootlet-copy）在 native 以解析桩满足
   %internal-names 审计，调用即报错，实现归 s7-compat 桶；hook、
   stacktrace、setter 已补齐（4056aa7c）。GC：BDWGC 默认、
   GOLDFISH_GC=precise 回退，单文件峰值由 ~8.4GB 降至几十 MB
   （0cb38820）。待补原语余项：random 族、read-u8 族、
   g_path-read-bytes 返回类型（Vector→Bytevector）。
   目录级终验：export-strict-audit PASS（36m50s）；lib-cache-all-libs
   冷加载 112/115（余3个为 srfi-27/scheme inexact/liii random，属
   浮点/随机桶），liii logging 经 hook 修复后转绿；keyed unbound 错误
   携带消息作首 irritant（修 loader 诊断形状回归）。

### R4 中途修复（2026-09-24）

- `cond-expand` 的 else 分支失效：clause head 是包着 symbol 的 syntax
  对象，重写时没做归一化，`eq?` 永远不中 → 一切用到 `cond-expand` 的
  库加载报 "no matching feature requirement"。已修（tests/expander 的
  `lib-cache` / `lib-cache-all-libs` 随之转绿）。
- `base-functions.scm` 把 `number?` 写成 `(integer? x)` 桩：浮点/有理/
  复数全判否，`finite?`、`rational?` 连带失效。已删桩——host 回到 s7
  实现，native 用自己的 `number?` 原语（`number-p` / `host-abi-load` 转绿）。
- `base-functions.scm` 的组合实现缺错误契约：`boolean=?` 非变参、
  `odd?`/`even?` 不查整数、`make-list`/`list-tail` 遇负数无限递归
  （单进程吃 15GB 的元凶）、`list-ref` 越界错误 key 不对、
  `vector->list`/`vector-fill!` 缺 start/end。已按测试契约补齐。
- char 表：`45e0336d` 删掉 Scheme 侧 Chez 契约表后 C++ 表没跟上，host
  也从未注册 `g_char-foldcase/numeric?/whitespace?`。表已程序化移植进
  `src/runtime/unicode_char.cpp`（host/native 同源），host
  `scheme_char.cpp` 注册三者；`char.scm` 的 memv fallback 补布尔化。
- `(scheme r5rs)` 导出的 `eval` 来自 `(goldfish)`（host eval 本尊），不认
  program 环境：改为 `(except (goldfish) eval)` + re-export
  `(scheme eval)` 的分发版。
- 收口：`tests/scheme` 321/321（改前 304）、`tests/expander` 21/21
  （改前 18/21）、changed-since 13/13、native 门禁空缓存全绿。

## 目标

Goldfish 的语言语义由 Scheme 层和 core IR 定义。C++ 只提供一个独立、
可测试、可替换的 runtime：对象、内存、控制流、求值、reader、primitive
边界和平台能力。

新 runtime 不得依赖 s7 的对象、调用、异常或 loader 协议。最终构建中，
删除 vendored s7 后，以下路径必须仍然成立：

```text
native runtime
  -> lowered core program
  -> kernel artifact
  -> expander/compiler
  -> library loader
  -> user program
```

## 边界

### C++ runtime 负责

- `Value` 及其对象表示；
- heap、GC 和显式 root 管理；
- symbol intern；
- core evaluator 和控制状态；
- Scheme 异常、多值、continuation、`dynamic-wind`；
- primitive 注册和调用 ABI；
- 最小 reader、内存/文件输入 port 和模块加载接口；
- OS、文件、时间、网络等平台原语。

### Scheme 层负责

- 派生语法和宏；
- record、`guard`、`error-object` 等语言库语义；
- module/import 的高层规则；
- SRFI、liii 和其他用户库；
- compiler passes、tree-il 变换和 cache 逻辑。

C++ 不为单个高层库增加业务语义。新增 C++ primitive 必须说明为何不能
用 Scheme 实现，并归入 platform 或 runtime 合同。

### schemify 原则

可行的工作一律优先落在 Scheme 层（"C++ is the executor and the pipe,
Scheme is the language" 的升格命名）：符合 Scheme 哲学与同像性——
语言定义本身可以是程序、可检查可组合——并把语义收敛在单一语言里，
维护成本最低。边界即上两节清单加三条豁免：引擎自有物（GC、栈/尾
调用、call/cc）、实测热路径（eval/apply，除非测量翻案）、启动地板
（冷启动 reader 与原语装载）。**mode → lang protocol 抽取是本原则的
首要范例**：mode 长成 Scheme 数据（import 集 + 可选 module-begin），
C++ 只剩 `-m` 分发，同时为"后续方向"的语言协议预铺形状。

当前迁移期的 primitive 分层：

- `standard_primitives.cpp`：对象/类型、pair/vector 原子操作、整数数值、
  reader/port、environment/eval 等 runtime substrate。platform、Unicode 和
  migration 入口分别位于 `platform_primitives.cpp`、
  `unicode_primitives.cpp` 和 `migration_primitives.cpp`，避免把过渡层伪装
  成核心 runtime。
- `bootstrap_primitives.cpp`：`length`、`reverse`、`append`、`memq`、`assq`、
  `filter`、`fold`、`map`、`for-each` 的临时 kernel bootstrap fallback；库层
  加载后应由 Scheme 定义覆盖，最终从 C++ 删除。
- `bootstrap.cpp`：native bootstrap 的安装顺序和 lowered library artifact
  入口；它不读取 source Scheme，也不承担 library expansion。
- `goldfish/expander/lib/base-functions.scm`：已迁移的组合 list/vector 和
  数值谓词语义。

native library `.gfo` 的 payload 约定为
`(bindings lowered-defs transformer-records)`；tiny reader 只读取 envelope
和 lowered datum。native bootstrap 负责依赖图、toplevel/primitive binding
恢复和可直接求值的 transformer 恢复；带 `stx*` 的序列化 transformer 仍需
native syntax-object 反序列化后才能进入完整 cache 热路径。

## 依赖规则

新 runtime 的依赖方向固定为：

```text
       value / heap
       ↙    ↓    ↘
   symbol  port  control
       ↘    ↓    ↙
         reader
            ↓
        evaluator
            ↓
       module / loader
```

`platform` 只被 `port` 和平台 primitive adapter 依赖；它不依赖 evaluator、
module 或 Scheme library。

各层通过显式 `Runtime`/`Context` 传递状态，不使用隐式的 s7 rootlet、
全局 inlet 或跨 evaluator 的 callback。新 runtime 源码禁止包含或引用：

- `s7.h`、`s7_*`、`s7_pointer`；
- `gf::pointer` 及 s7 专用 wrapper；
- HOF registration table；
- s7 的 `throw`、`catch`、longjmp 协议。

## 最小求值合同

第一阶段只要求执行 `CORE-SEMANTICS.md` 中的 lowered core：

```text
quote define lambda if begin let let* letrec letrec* set!
values call-with-values module-ref module-set
```

其中：

- `Value` 不暴露底层对象地址作为语言身份；
- closure 捕获定义时环境；
- continuation 保存完整的 runtime 控制状态；
- 多值和单个 unspecified 不得被静默折叠；
- `letrec` 未初始化读取是明确错误；
- 异常通过 runtime 控制协议传播，不穿越 C++ 未管理的 longjmp；
- core evaluator 不解析未展开源码，也不实现高层宏。

## Value 和内存的初版约束

为了优先保证正确性和可读性，第一版允许牺牲性能。GC backend 已定：
native 默认 BDWGC（vendored `third_party/bdwgc`，GC_BUILTIN_ATOMIC、
ALL_INTERIOR_POINTERS、单线程 stop-the-world），host 保留 exact tracing
backend，native 上 `GOLDFISH_GC=precise` 可切回。约束与结论：

- `Value` 使用 tagged handle 或等价的稳定表示；
- heap object 统一 header 和类型 tag；
- GC 必须通过 `Heap`/`RootScope`/`Tracer` 接口提供（backend 可替换）；
- exact backend 下所有跨调用保存的对象必须经过显式 root scope；
  BDWGC backend 下全局 `operator new` 落在 GC 堆上，栈与容器即根，
  但新增三条硬约束：
  - 析构不随回收运行 —— 资源必须显式释放（端口在 close-port 关闭，
    不依赖析构）；
  - 只活在异常 payload 里的 Value 不被扫描 —— catch 站点先把它落到
    自己的栈帧；
  - 顶层形态边界（expand-eval 入口）主动 collect，脏堆约束在单形态
    内，这是保守误保留级联保持廉价的前提；`operator delete` 的
    GC_free 急回收同理，不可退回 no-op。
- 允许非移动 GC、额外分配和保守的数据结构；
- 不在第一版引入 generational GC、压缩指针、JIT 或 bytecode ABI。

BDWGC 评估结论（替代原"后续单独评估"）：conservative 扫描消除了逐帧根
保护的永久纪律成本；代价是保守误保留与分配热点开销。已知后续优化项：
small-vector 参数表、字符串 atomic 分配、parallel mark。资源释放与
finalization 按上述显式 close 约定，不走 C++ destructor。

`Kont` 的持久化、对象布局压缩和分配优化属于后续性能工作，不能改变
第一版的语义合同。

## 迁移阶段

### R0：独立 core runtime

新 runtime 不链接 s7，能读取并执行一个 lowered core program。测试只依赖
Goldfish 的 semantic expected output，不把 s7 作为规范。

### R1：最小 bootstrap

加入 tiny reader、gfo reader、基础 port、primitive registry 和 kernel
artifact 加载。目标是执行现有 `kernel-combined.scm`，不要求马上重建它。

### R2：expander bootstrap

迁移 module registry、expander runtime、tree-il 和 compiler，使新 runtime
能通过 tiny reader 加载并运行对应的 lowered `.gfo` library artifacts；源码
reader、源码展开和 artifact 生成仍属于 R3 的 Scheme/compiler 自举入口。

### R3-A：稳定 native bootstrap 基座

native bootstrap 具备幂等初始化、明确的失败状态和可重试的依赖加载；tiny
reader 可通过 `open-input-string` 和 `open-input-file` 逐个读取 lowered
datum，并支持 EOF 和关闭端口错误。此阶段仍保留 migration-only 的
bootstrap/legacy primitive 层，不把它们误当作最终语言库实现。

### R3-B：native source bootstrap 闭环

在 R3-A 之上，native loader 能恢复 bootstrap 期间生成的 module/program
artifact（包括旧版 program cache 的兼容读取），并让 Scheme 层的
`read-forms`、expander 和 `compile-file` 接管源码入口。验收路径是：

```text
kernel artifact -> expander artifacts -> reader artifact
                 -> tiny reader 读源码 -> Scheme compiler -> native evaluator
```

这里的兼容 alias 只用于 bootstrap installer 生成的 implementation-module
artifact；普通库仍通过显式 module identity 和 artifact loader 恢复。tiny
reader 继续只负责 lowered datum/artifact，不扩展成完整的源码 reader。

### R3：自举和库迁移

新 runtime 能从 kernel source 重建 kernel artifact，并逐步迁移普通 Scheme
库、cache、CLI 和 REPL。

### R4：删除过渡层

默认入口切换到新 runtime；删除 gf0/s7 bridge、s7-specific compatibility
层、s7 构建文件和 vendored s7 源码。

#### 切换顺序（2026-09-28）

1. **默认入口已切换**：`bin/gf` 是 native runtime，`bin/gf-host` 保留原
   s7 host。C2 和 gf0/s7 差分工具明确调用 host oracle；普通测试和 native
   工作流使用默认 `gf`。
2. **稳定默认路径**：验证 CLI、测试运行器、source bootstrap、warm/cold cache
   和代表性库工作流均由 native 执行，同时保留 host 回退入口。切换与删除
   vendored s7 不合并为一个不可回退的改动。
3. **关闭删除 s7 前的审核项**：取得 `match-capability-test.scm` 的 host/native
   成对结果；复核 C2 记录的时效性，并把已知 53 项 defer 和 2 项 exclude
   明确作为首轮 native cutover 的接受范围。
4. **删除过渡层**：在 native 默认路径稳定且 host 不再是构建/运行依赖后，
   移除 s7/gf0 目标、vendored 源码、bridge 和只服务于旧路径的工具/测试。
   同步更新文档与剩余测试入口。

本仓库当前没有实际运行的 CI，因此 R4 不以 CI 接入为前置；切换验收由
明确记录的本地命令完成。最近一次默认切换前的 host 全量测试为 1555/1555；
默认切换后 native C3 固定工作流为 9/9、call/cc/dynamic-wind strict 差分为
2/2。前者不是 native 全量通过证据。C2 记录的 1276 个
host/native agreement 和 55 个显式 skip 是 2026-09-27 的汇总，不是本轮
重新执行的完整差分。call/cc 已实现，不能再列为未完成前置。

### 后续方向（占位）

- **lang protocol 研究**（Racket #lang 式三面：reader / module-begin /
  binding）：**E 阶段正式着手**——差分工具即 expander API 的第一个
  外部消费者，skip 三分类提供 surface 边界输入；D 后以"mode 系统统一
  为三个 language"作采用试点。现阶段只预留形状（mode 不长成特判、
  保持 read/expand/eval 缝隙、不提前形式化 kernel API）；reader 面
  除非出现非 S-表达式需求不启用。
- **REPL / CLI 提升 Scheme**（更长远）：循环、分发、命令、history
  政策、补全逻辑全归 Scheme（消灭 gf.cpp 与 native_main.cpp 的双份
  分发）；**readline 提供者留 C**——isocline 收窄为最小行编辑原语
  （读行/编辑/补全钩子），终端字节流属平台原语豁免，不做纯 Scheme
  行编辑器。研究窗口同 E；采用在 D 后单目标重写。可提前的独立小件：
  native 接 isocline（现 repl 分支为裸 getline，无编辑无 history，
  与 host 有 parity 缺口）。

## 验收原则

每个阶段都必须同时满足：

1. 新增 runtime 单元测试通过；
2. 受影响的 core/expander/library 测试通过；
3. 新 runtime 不新增 s7 依赖；
4. 失败能定位到 runtime、Scheme library 或 loader，而不是依赖“另一边也失败”；
5. 对象、异常、多值和 module identity 的行为有明确测试。

现有 s7 differential gate 在迁移期继续使用，但只作为迁移参照，不是最终
语义合同。新模块迁移后，应删除相应的 s7 bridge 测试和 HOF entry。

C2 的验收范围和明确排除项记录在
[`tests/C2-ACCEPTANCE.md`](tests/C2-ACCEPTANCE.md)，机器可读的 skip 台账在
[`tests/c2-skip.tsv`](tests/c2-skip.tsv)。被分桶的测试不计为通过；双端同错
也仍然可见，必须修复或按规范明确裁决后才能通过 strict gate。
C3 的 native readiness 范围和验收标准记录在
[`C3-ACCEPTANCE.md`](C3-ACCEPTANCE.md)；切换默认运行时和删除 s7/gf0
仍属于后续 R4，不由 C3 自动触发。

### 语义 oracle 层级

1. **R7RS-small 是最高规范**：spec 文本优先；实现间有分歧时以 spec
   裁决，成熟实现（Chibi、Guile/Racket 的 r7rs 模式）仅作交叉验证。
   例外细化：宏卫生的算法选择 spec 不作规定——本实现采用 sets of
   scopes（Flatt）+ home-library 兜底，capture/ellipsis 逃逸等边角
   以 sets-of-scopes 论文与 Racket 行为为参照；核心宏全卫生与方言层
   `define-macro` 的非卫生（defmacro.scm 文档化）并存是设计而非不一致。
2. **Goldfish 方言面**（liii、s7 兼容层）以 "goldfish-on-s7 的既有
   行为" 为兼容基线——仅迁移期有效，随 D 删除 s7 一并退役；其语义
   规格可直接查 s7 文档。
3. 差分门（E 阶段）的每个差异必须三分类：
   - 违背 R7RS → native 修正，偏离 s7 是修正而非回归；
   - 方言所需 → 保持 s7 行为；
   - spec 未定义 → 在本文裁决并记录，成为后续规范。
   skip 名单理由与该分类对齐。
4. **方言面扬弃**（2026-09-26 定）：native surface 不继承 s7 器官。
   `with-let`/`sublet`/`unlet`/`let-set!`/`*s7*` 列为删除项——新宏系统
   （syntax-case + 显式模块环境 + eval-when 区域）不需要环境拷贝形式；
   hooks 不保留对象协议，logging 的 exit-flush 以退出时调用注册
   thunk 的朴素机制替代；`stacktrace` 保留需求、实现归 native 栈迹。
   执行时机分层：native-only 面（C++ 桩、%internal-names 条目）可即刻
   删；共享 Scheme 源码的使用点改写须 host/native 双端可跑，随 C3/D 收口。
   当前的解析桩是待执行的删除，不是永久状态。
5. **数值塔处置**（2026-09-26 定）：
   - 表示约束：Value 的 union 已可容纳 double（float 落地不改 Value
     布局）；bignum/ratio 以堆对象加入 ObjectType，不进 immediate 层。
   - 溢出策略：int64 精确运算溢出在 bignum 落地前**显式报错**
     （当前 `+`/`*` 未检查、静默回绕，属待修缺陷），bignum 后改自动提升。
   - 顺序：float + inexact 函数族（阶段3，按塔级 exact/inexact 传染
     规则一次设计，不作临时补丁）→ bignum/ratio 合规补全。塔后半段的
     oracle 是 spec + Guile/Chibi——s7 本身无 ratio/bignum，**不受 D
     约束**，可后置于 D。
   - host（s7）同样无 ratio/bignum：方言缺口记录在此，差分门测不到
     塔的后半段，须以 R7RS spec 测试为准。
