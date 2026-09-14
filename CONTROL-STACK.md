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
