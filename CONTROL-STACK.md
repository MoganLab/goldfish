# T0 控制栈设计（call/cc＋dynamic-wind＋TCO 的前提）

状态：设计决定，不写代码。reference 求值器（`src/gf0_eval.cpp`）保持
C++ 递归直 walk；本页是其继任者（M-VM）的开工令。

## 为什么必须自有控制栈（三条证据，都已实测）

- `call/cc` 拒收 gf0 闭包（wrong-type-arg）：s7 continuation 只捕获
  s7 栈，gf0 的 C++ 求值帧对其不可见，转交即错。
- 即使递入 s7 闭包，escape 也会丢 gf0 帧：continuation 重入恢复的是
  s7 栈，C++ 侧帧已返回，语义断裂。
- 无 TCO 的直 walk 在 8M 栈下约 5000–10000 尾调用即触 C-stack guard
 （已量化，干净报错但不可用）。TCO 与 continuation 是同一问题的
  两面：谁拥有"调用的延续"，谁就必须显式管理它。

## 二选一

### A. trampoline＋显式 control stack（推荐）

- `eval` 不再递归：tail position 返回 `Thunk{closure, args, env}`，
  中央循环驱动；非尾位置压帧到堆上 control stack。
- call/cc＝捕获 control stack 切片（堆对象，可恢复/多次调用）；
  dynamic-wind＝栈帧附 winder 链表，进出执行 pre/post。
- TCO 免费获得（tail position 不压栈）；C-stack guard 可退役；
  每层开销从 ~1KB C 栈变为一次堆分配（需 GC 配合，见下）。
- 代价：重写 eval 循环（约 600 行量级）；错误协议从 longjmp 改为
  沿 control stack  unwind（winders 必须跑，这是 longjmp 做不到的）。

### B. 分段拷贝（Chicken 式）

- 保留 C++ 递归求值（改动小）；call/cc 时把 C 栈段拷到堆，
  重入时拷回。
- 代价：拷贝点必须手埋（每个可能 escape 的边界）；与 s7call 的
  可重入 s7 eval 交织时，C 栈里混着 s7 帧——拷贝 s7 帧无意义且危险；
  TCO 仍需另做；dynamic-wind 同样要帧链表。省事是假象。

**决定：A。** B 与 s7call 共存无解（C 栈混帧），A 把栈干净地收归
引擎所有。

## 与现有设计的交互点（A 方案下）

- `s7call` eval-wrap 是天然栈切换边界：进 s7 前把 control stack
  指针存入 continuation 捕获点；s7 侧 escape（call/cc 在 s7 闭包内）
  抛回 C++ 时 unwind 到边界。s7call 内部不需要理解 control stack。
- V 多值协议不变（single|multi 照单搬运，只是承载体从 C++ 返回值
  变为 control stack 槽）。
- `g_gf0-apply` trampoline 语义不变：s7 回调进入时新建 control
  stack 段（s7 调用 gf0 闭包＝一次"进入"，返回即 unwind）。
- 错误：`gf::error` longjmp 只在"无 winder 需要跑"的 fast path 保留；
  一般错误沿 control stack unwind，跑完 winders 再交 s7 catcher。
- GC：control stack 整体 pin（现有 pin 机制直接复用）；堆帧分配
  节奏与 s7 GC 的交互是 M-VM 实现时的第一风险，需单列验证。

## 不变项

- env 模型（frame 对象共享）、CORE-SEMANTICS（含两处 R7RS 修正）、
  冻结基线四表、差分门。控制栈只换"调用如何进行"，不换语义。

## 开工条件（M-VM 启动门）

- M2a ≥8 程序全绿（含本页的 trampoline 回调路径）。
- 本页无异议。即启动：先把 eval 改写为 thunk 循环（call/cc 先
  `error "not implemented"` 占位），差分门全绿后再接 continuation。

## M-VM-1 落地（thunk 循环，done）

- `step()` 单步求值→值或 `Resume{env, body}`；`finish()`／`step_seq()`
  平坦驱动。尾调用零 C++ 嵌套（10 万尾调用正确返回 5000050000）；
  非尾静态嵌套照常递归，C-stack guard 留任。
- call/cc 仍占位（未实现）；差分门 13/13 零修改通过，基线不动。

## M-VM-2b 落地第一斧（CEK＋call/cc，done）
- 全部 15 处嵌套 `eval()` 改为显式 Kont 帧（Seq/If/CallP/CallA/
  Define/Set/LetB/RecB/ValsB/Vals/CwvP/CwvQ/CwvC/CatchG/CatchR/
  Mod/CcK）；`runLoop()` 单循环驱动，无 C++ 递归求值。
- `call/cc` 原生 multi-shot（捕获＝拷贝 Kont＋env，调用＝安装拷贝；
  单 datum 循环验证 `(done 3)`；escape 丢弃正确；catch 区 abandoned
  时 handler 不跑——符合 call/cc 语义）。
- `dynamic-wind` 显式 stub（干净 error，後半段接 winder）。
- 差分门 13/13 零修改通过；TCO 保持（10 万尾调用）；C-stack guard
  留任（s7call 叶路径）。基线不动（无新原语）。
- 修的两个移植 bug：首 init 少一层 car；cwv 漏 producer 零参调用。

## M-VM-2b 落地第二斧（dynamic-wind，done）

- DwK 帧 6 阶段（before 求值→调 before→after 求值→推 winder→thunk
  求值→调 thunk→弹 winder→调 after→回 thunk 值）；winder 栈全局，
  depth 标记，capture 拷贝、invoke 按公共前缀 splice（弃段 afters
  内→外、进段 befores 外→内）；GfEx unwind 逐帧弹并跑 due afters。
- 验证：正常序 b,t,a＋回值；escape 穿 winder 跑 after；重入转移
  轨迹 `(done a b a t b)` 逐字正确。
- 排查记录：一次"挂起"实为测试本身的无限重入（invoke 点在续体
  内，无状态推进）——引擎行为正确；另有一次构建期花括号失衡，
  用自写 C 计数器定位到 plugInto 缺闭合（教训：大改写后先数括号）。
- 基线不动（无新原语）。

## 参考与立场

- Oleg Kiselyov, "An argument against call/cc"
  （https://okmij.org/ftp/continuations/against-callcc.html）：
  反对的是"call/cc 为王"的架构，不是 call/cc 本身。本计划与之
  一致——escape（exceptions/guard）走独立原生路径，call/cc 只是
  显式栈上的又一个原生操作，而非一切控制的地基。
- 引擎边界即天然 delimiter（业界 REPL-delimit 实践的对应物）：
  continuation 不跨 s7call 边界，捕获即明确 error。
- call/cc 落地后的必测用例：generator→stream 尾递归枚举。
  预期与 s7 **同泄漏**（显式栈拷贝语义 pin 住 suffix）——先对齐，
  再谈优化（优化＝prompt，已超 R7RS，不做）。

## Native runtime 的 call/cc 原型（2026-09-27）

上面的 M-VM 实现位于 `src/gf0_eval.cpp`，不能视为 C3 native runtime
已经支持 continuation。C3 使用另一套对象/GC 和 evaluator：
`src/runtime/evaluator.cpp` 与 `src/runtime/core_evaluator.cpp` 仍通过
C++ 递归执行非尾表达式；`dynamic-wind` 在
`src/runtime/standard_primitives.cpp` 中是普通调用包装，无法感知
continuation 跳转。R4 的 `call/cc` 工作必须在这条 native 路径独立落地。

首版采用 evaluator 自有的 CEK 控制循环和可复制 Kont 快照，不操作或
复制 C++ 栈。捕获时复制当前控制帧；调用 continuation 时从不可变捕获
快照构造当前工作帧栈，并以传入的完整 `Values` 恢复求值。这样先直接
覆盖 R7RS 所需的 unlimited extent 和 multi-shot 行为，避免工作栈后续
变更污染已捕获快照。线性拷贝是首版明确接受的成本，不预设 Chez 的
内部实现等同于 copy-on-write。

实现边界：

- native continuation 必须是 native heap object；其 `trace()` 覆盖快照
  中的所有 `Value`，捕获环境通过既有 `Environment::trace` 保持可达。
- Kont frame 将表达式、环境、部分参数/多值结果和返回阶段显式化；
  普通调用、`apply`、`call-with-values`、定义/赋值、异常 handler 等
  所有会跨越求值点的路径都必须经由同一个机器，不能留下递归求值旁路。
- continuation 捕获/恢复先覆盖多值、tail context、closure tail call、
  重复调用与变量共享语义。变量环境仍是共享可变环境，不随 Kont 快照
  回滚。
- `dynamic-wind` 单列为后续实现：动态 winder 链必须随 continuation
  捕获，并在跳转时按退出/进入顺序运行 thunk；不能复用当前普通
  primitive 包装来宣称完成。
- 先记录普通求值、深栈、单次捕获/恢复和 SRFI-158 generator 的成本；
  只有测量显示快照复制是实际瓶颈，才评估不可变共享段或 COW。COW
  会增加写屏障/分支恢复复杂度，不作为正确性实现的先决条件。

本节不改变 gf0 M-VM 的现有结论。native 路径已落下第一版 CEK machine：
Kont frame 与表达式求值共用一个中央循环；闭包调用在机器内展开；
`call/cc`/`call-with-current-continuation` 保存可多次安装的快照；快照
保留共享可变环境和全部多值。continuation 带求值机身份，跨嵌套 native
primitive callback 的跳转会传播到捕获它的活动求值机，避免 C++ primitive
在非局部跳转后继续执行。

后续边界审查把会调用 Scheme 回调的常见迭代原语也迁入机器帧：`map`、
`for-each`、`fold`、`filter`、`any`、`every`、`member`、`assoc`、
`string-for-each`、vector filter 和第一类 `call-with-values`。因此在
这些操作内部捕获并稍后恢复时，迭代游标、累积值和外层 Scheme 帧一同恢复。
重入测试覆盖 `for-each`、`map`、`fold`。

`dynamic-wind` 使用 winder 身份链；普通返回、`raise`、`throw` 和
continuation 进入/退出都会按顺序调用对应 thunk。当前快照使用线性拷贝，
不做 COW。GC 通过 continuation 对象追踪快照中的 Values、帧环境和 winder。
定向验证覆盖 call/cc、多值、重复调用、primitive callback escape、
异常退出和 dynamic-wind continuation 重入；全量测试尚未运行。

后续核验：三个原先因 engine-callcc 跳过的 C2 文件
（call/cc、完整名称、SRFI-158）已纳入 C2/C3 manifest。严格 host/native
差分 3/3 一致，C3 native workflow 8 项通过。性能探针位于
`bench/native/continuations.scm`；当前样本中 2,000 次浅捕获约为对应
tail loop 的 1.62 倍；深度 500 和 5,000 的单次快照分别比普通返回高约
2.8% 和 3.8%。该结果受背景负载影响，只作为 profiling 基线，不作发布
性能门槛，也不足以支持引入 COW。

### continuation 副作用探针（已核清）

捕获 continuation 后，在捕获点之外将共享变量改为 40，再调用 continuation；
host 与 native 都返回 `(resumed 41 41)`。这确认该路径恢复控制状态但保留捕获后的赋值。
最初 `vector-filter` 探针记录到 11 次访问，也在两端一致：
`goldfish/liii/vector.scm` 的实现先用 `vector-count` 遍历谓词，再遍历一次
填充结果；continuation 捕获在计数遍历中，恢复时继续计数，然后执行填充遍历。
原测试把访问次数误当成单遍原语的行为。修订后的回归用例只断言最终向量与恢复完成，
不绑定到该库实现的遍历次数。

这仍是 native 端原型，不宣称替换 vendored s7 或覆盖所有引擎交界：
continuation 不能安全穿越任意外部/s7 调用帧；`catch` 的 C++ callback
边界仍需单独处理。下一步做 native 与 gf0 的差分验证，并测量
generator/continuation 的快照分配成本；只有数据表明复制是瓶颈时再评估
共享段或 COW。

### 资源型 Scheme callback 边界：端口部分已迁入机器

`call-with-input-file`、`call-with-output-file`、`with-input-from-file`、
`with-output-to-file`、`with-input-from-string`、`with-output-to-string`、
`call-with-input-string` 和 `call-with-output-string` 现在由 evaluator
控制帧直接调用 Scheme thunk，不再依赖已返回的 C++ 包装栈。动态端口绑定
与端口资源状态保存在特殊 winder 中；continuation 退出时恢复外层端口并
关闭文件输出流，重新进入时恢复绑定，文件输出流以追加模式重开。当前文件
输入端口是内存缓冲端口，恢复时保留其读取位置并重新开放。

native C3 工作流覆盖字符串和文件端口的 escape/re-entry、恢复期间的读写、
端口异常退出，以及传端口给 callback 的 call-with-* 变体。共享 C2 测试仍
只验证 host/native 两边都应满足的普通行为；host 的 s7 包装器不能正确续接
已返回的 C++ 端口包装帧，因此 continuation 专项放在 native-only C3。

`catch` 已迁入 evaluator machine：body 由 Catch frame 包围，异常路由在机器
内匹配 tag 并调度 handler，因此 body/handler 中捕获的 continuation 都能保留
本机帧。匹配异常时先通过 continuation transfer 退回 catch 入口的 winder
链，再调用 handler；guard 与 catch 同时有效时，异常按帧栈中的最近边界
处理。transfer 完成异步 winder thunk 后，主循环会继续消费目标快照里的
pending apply。C3 workflow 覆盖 body capture/resume、handler capture/reentry、
嵌套 tag mismatch、C++ 错误分类以及动态风格退出后再调用 handler。模块加载
期间的 `apply_values` 属于引擎内部扩展流程，本轮不纳入 Scheme continuation
保证范围。
