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
- **文件**：`src/gf.h` / `src/gf.cpp` / `src/gf_glue.hpp` 为唯一可 `#include "s7.h"` 的位置；对外仅暴露 `gf::` 命名空间与 `gf::host_version`/`host_date`。

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
- **职责**：执行**位置编码字节码**——四槽一组（opcode/payload/i0/i1）的扁平向量，操作码数字是与 `bytecode.scm` 的 `vm-opcodes` 共享的 ABI；标签解析在 Scheme 侧完成，C++ 不认识任何符号指令拼写。`Op::Values (n>=2)` 才置多值标记，`Op::CallWithValues` 委托宿主完成多值展开。
- **不变式**：禁止 `s7_` 类型拼写与 `#include "goldfish/"` Scheme 文件；操作码编号与 `bytecode.scm` 的 `vm-opcodes` 保持同步——发布前可自由重编号（两侧一起改），首个发布版起冻结为 ABI。

## L7 loader

- **文件**：`src/goldfish.hpp`（胶水注册）与 `src/goldfish.cpp` 的 CLI/REPL/`*load-path*` 分发。
- **职责**：仅参数解析与模块加载分发，不含展开/编译逻辑；业务编排优先下沉至 `liii/*`。
- **不变式**：禁止 `#include expander/compiler` 与 `(import goldfish/compiler)`；胶水数预算 `<=64`。

## 依赖与降级契约

- **方向**：`L7->L6->L5->L4->L3->L2->L1->L0` 单向；`L4` 的 `vm` 通过宿主原语回退而非直接依赖 `L6`；`L2` 不感知 `L3` 以上。
- **示例**：
  - 允许：`L4` 调用 `g_listdir`（`L0`）；`L5` 消费 `L2` 的 gfo 结构。
  - 禁止：`L1` 依赖 expander；`L5` 出现 `s7_` 或依赖用户态库；非 `L0` 包含 `s7.h`。
- **机检**：`sh tools/lint-layer.sh` 为开发期便利，覆盖常见违规即可；`xmake.lua` 的文件清单注释按层线性分组，便于审阅。

## 演进债

- **liii 定位**：`liii/*` 为高于核心层的业务脚本层（boot 除外，其属裸码集合）。
- **时效戳演进**：mtime/size 改为内容哈希（git 操作导致 mtime 漂移只会浪费缓存，不会出错，但哈希可消除误失效）。
- **缓存阶段 2A/2B**：值库缓存与宏数据化，见 `CCACHE_NOTES.md`。
- **VM 多值**：当前以 `list/cons` 规避，待 `Op::Values` 寄存器化后移除包装；在此之前须防止其渗入用户可见语义。
- **C++ 业务收尾**：`find_function` 等残留宿主业务迁至纯 Scheme，进一步收敛胶水阈值。
- **产物再生产校验**：CI 校验内核产物 == 从源码重新生成的结果（逐字节一致）。
