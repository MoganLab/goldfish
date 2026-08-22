# Layers

Goldfish 采用 8 层分层架构 `L0..L7`，依赖方向严格为 `Ln -> L_{<n}`（仅可依赖更低层）。

分层的权威表达是**目录结构**：目录即层级，在不同目录下工作时持有对应层级的心理模型。
`tools/lint-layer.sh` 只是开发期快速检查的便利工具——它不可能覆盖所有情况，无需如产品代码般尽心维护。

```
L7 loader ─> L6 vm ─> L5 compiler ─> L4 expander-lib ─> L3 expander-rt ─> L2 core-format ─> L1 tiny ─> L0 host
```

引导序列（裸码集合的加载顺序）：宿主求值 boot（`L1`）──> boot 加载 gfo（`L2`）──> 宿主加载内核产物（`L3`）──> expander 上线，此后一切经 gfo 缓存按需展开。

## 两条基本原则

1. **expander 尽早自举**：让尽可能多的 Scheme 文件享受完整展开能力（宏/模块）；其可度量形式是裸码集合封闭且有预算。
2. **C++ 极小核心**：大部分工作在 Scheme 完成；C++ 只接触非常小、非常核心、可控的结构/语言（三契约，见 `L0`）。

## 裸码集合（冻结 ABI）

- **定义**：expander 上线之前由宿主直接求值的代码——boot 脚本、gfo 格式模块、内核产物。无宏、无模块系统。
- **约束**：集合封闭、可枚举、总量有预算；新增成员须显式决策。
- **护栏**：内核产物必须可由源码再生产（CI 校验逐字节一致）；宿主语义变更时强制重展开。

## L0 host —— 三契约

- **对象模型**：vendored s7（`src/s7*` 定制树属本层），值/GC/闭包，冻结不动。
- **核心语言**：C++ 执行的只有两样——读取路径上的「完全展开后 sexp 子集」，执行路径上的 VM 指令集。C++ 永远不需要解析未展开的源码。
- **原语表 `g_*`**：无状态、单一职责的宿主调用，不含业务编排。
- 每份契约**有计数、有版本**（opcode 数、原语数、格式标签数、格式版本），预算接口面而非文件内容。
- **文件**：`src/gf.h` / `src/gf.cpp` / `src/gf_glue.hpp` 为唯一可 `#include "s7.h"` 的位置；对外仅暴露 `gf::` 命名空间与 `gf::host_version`/`host_date`。`src/liii_*.cpp`、`scheme_*.cpp` 是按主题拆分的原语实现文件（属本层原语表，同样只经 `gf.h` 访问 s7）。

## L1 tiny

- **文件**：`src/liii_reader.cpp`（最小闭包读取器，只读已展开数据）+ `goldfish/liii/boot.scm`（裸码集合成员）。
- **不变式**：禁止依赖 expander；boot 前 `160` 行内必须加载 gfo 模块。

## L2 core-format

- **文件**：`goldfish/core/gfo.scm`（`L2` 单源）。
- **职责**：编译缓存契约——缓存布局（key 由源路径派生）、时效戳（须记录全部输入：源文件与内核产物）、读写与回退策略。
- **不变式**：全系统最早运行的 Scheme 模块，在内核之前加载，只依赖 `L0` 原语，不得使用内核特性；格式必须带版本号，未知版本视为缓存未命中并再生成（永不要求用户清缓存）。当前版本 `0`（开发期），`1` 保留给首个发布格式，发布时开发缓存自然失效。

## L3 expander-rt

- **文件**：内核源码（`include` 清单）+ 自包含展开产物（生成物，属裸码集合）。
- **职责**：展开时内核；源码由**上一代** expander 提前展开（分阶段自举），故源码层面全功能、仅受限内核标准库。
- **不变式**：不依赖 compiler 与更上层；`include` 清单与加载清单保持同步。

## L4 expander-lib

- **文件**：`goldfish/expander/lib/*.scm`、`goldfish/liii/reader.scm`。
- **职责**：`cond-expand`/`syntax-case` 等用户态库与完整 reader；`vm` 以宿主原语回退（`vm-load`/`vm-enter` 经 `gf::` 调用），不直接依赖 `L6`。
- **不变式**：禁止 `import (goldfish compiler)`；`gfo` 单源。

## L5 compiler

- **文件**：`goldfish/compiler/*.scm`、`goldfish/compiler.scm`、`goldfish/expander/syntax-ir.scm`。
- **职责**：record IR 纯变换与 `lower`/`run-passes`；依赖面收窄为 `L2` 的格式契约，不依赖 `L4` 用户态库。
- **不变式**：禁止 `s7_` 与 `goldfish/core|expander/lib` 导入。

## L6 vm

- **文件**：`src/goldfish_vm.cpp`（`gf::` only，per-program VM）。
- **职责**：执行**位置编码字节码**——四槽一组（opcode/payload/i0/i1）的扁平向量，操作码数字是与 `bytecode.scm` 的 `vm-opcodes` 共享的 ABI；标签解析在 Scheme 侧完成，C++ 不认识任何符号指令拼写。VM 单值传递：多值是宿主派生形式（`values`/`call-with-values` 为宿主过程，values 对象作为单值流经 VM 栈、由宿主 apply 拼接展开），无专用 opcode。
- **不变式**：禁止 `s7_` 类型拼写与 `#include "goldfish/"` Scheme 文件；操作码编号与 `bytecode.scm` 的 `vm-opcodes` 保持同步——发布前可自由重编号（两侧一起改），首个发布版起冻结为 ABI。

## L7 loader

- **文件**：`src/goldfish.hpp`（胶水注册）与 `src/goldfish.cpp` 的 CLI/REPL/`*load-path*` 分发；`src/goldfish_repl.cpp` 为 wasm REPL 入口（同规则约束）。
- **职责**：仅参数解析与模块加载分发，不含展开/编译逻辑；业务编排优先下沉至 `liii/*`。
- **不变式**：禁止 `#include expander/compiler` 与 `(import goldfish/compiler)`；胶水数预算 `<=64`。

## 用户库与业务脚本（非内核层）

- **文件**：`goldfish/scheme/*.scm`（r7rs）、`goldfish/srfi/`、`goldfish/guenchi/`、`goldfish/match.scm`、`goldfish/repro-hygiene.scm`；根目录 `gfproject.scm`、`node-rules.json` 为构建/工程元数据。
- **定位**：经 expander 加载的普通用户库，享受完整宏/模块能力；不属于引导闭包与裸码集合，无预算约束。`liii/*` 同属此类（boot 除外，其属裸码集合，见 L1）。

## 依赖与降级契约

- **方向**：`L7->L6->L5->L4->L3->L2->L1->L0` 单向；`L4` 的 `vm` 通过宿主原语回退而非直接依赖 `L6`；`L2` 不感知 `L3` 以上。
- **示例**：
  - 允许：`L4` 调用 `g_listdir`（`L0`）；`L5` 消费 `L2` 的 gfo 结构。
  - 禁止：`L1` 依赖 expander；`L5` 出现 `s7_` 或依赖用户态库；非 `L0` 包含 `s7.h`。
- **机检**：`sh tools/lint-layer.sh` 为开发期便利，覆盖常见违规即可（含 L5/L6 opcode ABI 同步检查）；`xmake.lua` 的文件清单注释按层线性分组，便于审阅。

## 演进债

- **时效戳演进**：mtime/size 改为内容哈希（git 操作导致 mtime 漂移只会浪费缓存，不会出错，但哈希可消除误失效）。另：程序级缓存不追踪依赖库变更，库更新后陈旧程序字节码仍可能命中。
- **缓存阶段 2A/2B**：值库缓存与宏数据化，见 `CCACHE_NOTES.md`。
- **VM 多值**：多值是宿主派生形式（`8a0c14c7` 移除早期 list 包装——全局重定义 `values` 会混用协议破坏引导链），VM 单值栈上 values 对象由宿主 apply 拼接；n≥1 各形态（cwv/let-values/apply/VM 闭包 producer）已验证正确。已知边界：s7 把 `(values)` 与无 else `if` 的 void 归于同一 unspecified 对象，`call-with-values` 以 eq? 探测将其视为零值（R7RS 零值语义，Guile/Racket 对照一致）；代价是 void 值流入多值语境记零个值（Guile 记一个）——边缘情形，记录在案。若未来需要严格值数寄存器化（Op::Values 记录 arity），属性能/语义精化而非正确性需求。另：曾疑似多值渗入的案例（auto-compile 下 `gfproject-load-config` 返回错位值）已查明与多值无关：根因是延迟求值 C 特殊形式（`catch`、`call/cc`、`dynamic-wind`、`call-with-*`/`with-*`）经 `s7_apply_function` 的 c_function 快速路径调用时只返回占位符 `#f`，VM 误当结果；已修（`62c228ae`，`s7_gf_apply_eval` 强制 eval 循环）。
- **C++ 业务收尾**：`find_function` 等残留宿主业务迁至纯 Scheme，进一步收敛胶水阈值。
- **产物再生产校验**：`xmake b verify-kernel` 已落地本地护栏——两次冷缓存 from-artifact 自举逐字节一致（fixpoint）且与已提交的 `kernel-combined.scm` 一致（再生产；不一致时提示 `xmake kernel` 重建）。重建入口 `xmake kernel`。剩余待办：接入 CI workflow。
- **错误穿越 VM 帧的重放（未修）**：VM 编译代码中，`(catch ...)` 的 body 经**嵌套 VM 库闭包**调用抛错时（如 srfi-165 的 `computation-environment-ref`、njson 的 `g_njson-ref` 穿过多层帧），s7 的跨层错误恢复与 VM 帧状态交互产生**重放**——body 前的顶层副作用执行两遍、二次执行时 catch 失效、栈错位派生怪异错误（assv 收到符号等）。宿主原语在同层直接抛错则正常；库 defs 走 s7 eval（GOLDFISH_NO_VM_DEFS=1）正常。修复需设计 VM 帧感知 s7 错误展开的同步协议，独立任务。
