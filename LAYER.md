# Layers

Goldfish 采用 7 层分层架构 `L0..L6`，依赖方向严格为 `Ln -> L_{<n}`（仅可依赖更低层），所有不变式由 `tools/lint-layer.sh` 机检。

```
L6 loader ──> L5 vm ──> L4 compiler ──> L3 expander-lib ──> L2 expander-rt ──> L1 tiny ──> L0 host
                │            │                   │                  │               │            │
                └──── gf:: ──┘                  └─ gfo 单源 ───────┘               └─ s7.h 单点 ─┘
```

## L0 host
- **文件**：`src/gf.h` / `src/gf.cpp` / `src/gf_glue.hpp` 为唯一可 `#include "s7.h"` 的位置；对外仅暴露 `gf::` 命名空间与 `gf::host_version`/`host_date`。
- **职责**：封装 `s7` 原语为 `g_*` 胶水（`g_listdir`/`g_goldfish-library` 等），不含业务编排。
- **不变式**：`L0` 以外禁止出现 `s7_` 类型/函数与 `s7.h` 包含（`lint` 第 6/9/10 项）。

## L1 tiny
- **文件**：`src/liii_reader.cpp`（仅 bootstrapping 子集） + `goldfish/liii/boot.scm`（首行即 `load-source-file "cache/gfo.scm"`）。
- **职责**：最小闭包读取器，供 `kernel-combined.scm` 生成前自举。
- **不变式**：`L1` 禁止依赖 `expander`（`lint` 第 11 项），`boot.scm:1-160` 必须首载 `cache/gfo.scm`。

## L2 expander-rt
- **文件**：`goldfish/expander/kernel-combined.scm`（由 `goldfish/expander/build-combined.scm` 生成，`753 forms`） + `goldfish/expander/kernel.scm`（`include` 清单）。
- **职责**：自包含的展开时内核，`kernel.scm` 的 `include` 与 `expander/kernel/load-kernel.scm` 清单需 `lint` 同步校验。
- **不变式**：禁止依赖 `goldfish/cache`/`goldfish/compiler`（`lint` 第 13/14 项）。

## L3 expander-lib
- **文件**：`goldfish/expander/lib/*.scm`、`goldfish/liii/reader.scm`、`goldfish/cache/gfo.scm`（`gfo-*` 单源，禁止他处 `define (gfo-`）。
- **职责**：`cond-expand`/`syntax-case` 等用户态库；`vm` 以宿主原语回退（`vm-load`/`vm-enter` 经 `gf::` 调用，`L3` 不直接 `import (goldfish compiler)`）。
- **不变式**：禁止 `import (goldfish compiler)`（`lint` 第 19 项），`gfo` 单源（第 8 项）。

## L4 compiler
- **文件**：`goldfish/compiler/*.scm`、`goldfish/compiler.scm`、`goldfish/expander/syntax-ir.scm`。
- **职责**：`record IR` 纯变换与 `lower`/`run-passes`，无 `s7`/`cache`/`lib` 依赖。
- **不变式**：`L4` 禁止 `s7_` 与 `goldfish/cache|expander/lib` 导入（`lint` 第 20/21 项）；`values` 多值当前以 `list/cons` 规避 `VM` 多值栈（见 `goldfish_vm.cpp:617` 注释）。

## L5 vm
- **文件**：`src/goldfish_vm.cpp`（`gf::` only，`per-program VM`，预解码分发）。
- **职责**：字节码执行，`Op::Values (n>=2)` 才置 `set_multiple_value`，`Op::CallWithValues` 委托 `s7` 的 `call-with-values` 完成多值展开。
- **不变式**：禁止 `s7_` 类型拼写与 `#include "goldfish/"` Scheme 文件（`lint` 第 7/22 项）。

## L6 loader
- **文件**：`src/goldfish.hpp`（含 `glue_goldfish`/`glue_vm` 注册）与 `src/goldfish.cpp` 的 CLI/REPL/`*load-path*` 分发。
- **职责**：仅做参数解析与模块加载分发，不包含展开/编译逻辑；业务编排优先下沉至 `liii/*` Scheme。
- **不变式**：禁止 `#include expander/compiler` 与 `(import goldfish/compiler)`（`lint` 第 23 项），胶水数 `grep -c glue_ <=64`（第 26 项）。

## 依赖与降级契约
- **方向**：`L6->L5->L4->L3->L2->L1->L0` 单向；`L3` 的 `vm` 通过宿主原语回退而非直接依赖 `L5`，`L2` 不感知 `L3/L4`。
- **示例**：
  - 允许：`L3 expander/lib` 调用 `g_listdir`（`L0`）、`L4 compiler` 消费 `L3` 的 `gfo` 结构。
  - 禁止：`L1 liii_reader.cpp` 出现 `expander`、`L4` 出现 `s7_` 或 `goldfish/cache`。
- **机检**：`sh tools/lint-layer.sh` 覆盖上述 11 项，`xmake.lua` 的 `add_files`/`add_installfiles` 注释按 `L0-L6` 线性分组，便于审阅。

## 演进债
- `VM` 多值当前以 `list` 规避，待 `Op::Values` 完整寄存器化后移除 `path.scm:124` 等 `cons` 包装。
- `L0` 胶水的 `find_function_libraries`/`load_gfproject` 等业务待迁至 `(liii project)` 纯 Scheme 后进一步收敛胶水阈值。
