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
- **护栏**：内核产物必须可由源码再生产（CI 经 `tools/canonicalize.scm` 做 gensym 盲的规范化比较——fresh 名无语义，不参与一致性判定）；宿主语义变更时强制重展开。

## L0 host —— 三契约

- **对象模型**：vendored s7（`src/s7*` 定制树属本层），值/GC/闭包，冻结不动。
- **核心语言**：C++ 执行的只有两样——读取路径上的「完全展开后 sexp 子集」，执行路径上的 VM 指令集。C++ 永远不需要解析未展开的源码。
- **原语表 `g_*`**：无状态、单一职责的宿主调用，不含业务编排。
- 每份契约**有计数、有版本**（opcode 数、原语数、格式标签数、格式版本），预算接口面而非文件内容。
- **文件**：`src/gf.h` / `src/gf.cpp` / `src/gf_glue.hpp` 为唯一可 `#include "s7.h"` 的位置；对外仅暴露 `gf::` 命名空间与 `gf::host_version`/`host_date`。`src/liii_*.cpp`、`scheme_*.cpp` 是按主题拆分的原语实现文件（属本层原语表，同样只经 `gf.h` 访问 s7）。

## L1 tiny

- **文件**：`src/liii_reader.cpp`（最小闭包读取器，只读已展开数据）+ `goldfish/liii/boot.scm`（裸码集合成员）。
- **同层成员（物理住在 `liii/`，引导期加载）**：`goldfish/liii/host-abi.scm`（seed 经 liii_reader.cpp 载入 rootlet 的 R7RS 值面 + `*vm-deferred-forms*` 宿主行为面清单）。
- **不变式**：禁止依赖 expander；boot 前 `160` 行内必须加载 gfo 模块。

## L2 core-format

- **文件**：`goldfish/core/gfo.scm` + `goldfish/core/ir.scm`（`L2` 双源：缓存契约与 tree-il IR 定义）。
- **职责**：`gfo.scm` 负责编译缓存契约——缓存布局、时效戳、读写与回退；`ir.scm` 是权威的 tree-il/核心 IR 定义（record IR + `core-language` 表），供 `L4` expander 直出与 `L5` compiler 共享，避免 `L3->L5` 层违背。两者皆在内核之前加载，只依赖 `L0` 原语与 `liii` 基建。
- **版本目录**：缓存根按管线指纹分段——`ccache/v<hash12>/…`。指纹是 sha256 聚合：s7 版本 + 引导链与内核工件（boot/core/gfo+ir/prelude/reader/host-abi/kernel-combined/expander/tree-il/compiler.scm/expander-lib/compiler 全部 .scm）。任何一项变更 → 新目录自然隔离。
- **不变式**：全系统最早运行的 Scheme 模块，在内核之前加载，只依赖 `L0` 原语，不得使用内核特性；格式必须带版本号，未知版本视为缓存未命中并再生成（永不要求用户清缓存）。当前版本 `0`（开发期），`1` 保留给首个发布格式，发布时开发缓存自然失效。

## L3 expander-rt

- **文件**：内核源码（`include` 清单）+ 自包含展开产物（生成物，属裸码集合）。
- **职责**：展开时内核。源码由**上一代** expander 提前展开（自举唯一路径：已构建的 gf 加载提交的 artifact 启动 expander，运行 `expander/build-combined.scm` 重展内核源码；`tools/verify-kernel.sh` 校验不动点与再生产）。
- **不变式**：不依赖 compiler 与更上层；`include` 清单与加载清单保持同步。

## L4 expander-lib

- **文件**：`goldfish/expander/lib/*.scm`、`goldfish/expander/tree-il.scm`（expander 直出 tree-il 桥）、`goldfish/liii/reader.scm`。
- **职责**：`cond-expand`/`syntax-case` 等用户态库与完整 reader；`tree-il.scm` 在展开后的 `syntax` 上重建 `IR`，直接产 `<primitive-ref>/<lexical-ref>` 且前置计算 `depth/index`，让 expander 成为 tree-il 的权威发射器；`vm` 以宿主原语回退（`vm-load`/`vm-enter` 经 `gf::` 调用），不直接依赖 `L6`。
- **缓存**：库缓存（`capture-library-cache`）存 `syntax->ir/sexp` 的 record tree-il（可序列化 vector），使缓存写路径的 passes 保留 `<primitive-ref>` binding kind；读缓存时统一降为 lowered sexp 供 s7/VM 求值。expander 输出对 primitive 引用带 `(primitive-ref name)` 标记（`lower` 还原为裸名）。
- **不变式**：禁止 `import (goldfish compiler)`（`core/ir` 除外，`core/ir` 为 L2 共享）；`gfo` 单源。

## L5 compiler

- **文件**：`goldfish/compiler/*.scm`（`syntax-ir.scm` 现为对 `expander/tree-il` 的薄包装；`ir.scm` 是 `core/ir` 的纯转发 + `$` 模式，无 legacy 节点 API）、`goldfish/compiler.scm`。
- **职责**：record IR 纯变换与 `run-passes`；`core->ir/ir->core` 仅为 `s7` 回退路径保留（非缓存 program 的 `optimize-on-load`、VM 的 `vm-load-defs`、`compile-defs` 测试面），权威 IR 定义已下沉至 `L2`，`syntax->ir` 直出路径由 `L4` 提供，`L5` 仅消费 `L2` 的 `IR` 契约。pass 集合不含 `lower-let`：它从 0 重启槽编号，会错位 `syntax->ir` 的真实 `(depth . index)` 词法地址（`lower-let` 只与 `core->ir` 的占位地址兼容，且字节码的 `compile-let` 已直接处理 `<let>`）。`optimize-on-load` 因输入是 lowered program（`compile-program` 产物）走 `core->ir`，故仍可安全附带 `lower-let`。
- **不变式**：禁止 `s7_`；允许 `goldfish/core/ir`（L2 IR），仍禁止其他 `goldfish/core|expander/lib` 导入。

## L6 vm

- **文件**：`src/goldfish_vm.cpp`（`gf::` only，per-program VM）。
- **定位（2026-08）**：**实验性优化层，默认关闭**——库 defs 与宏 transformer 默认以 lowered 核心 sexp 交 s7 直评，`GOLDFISH_VM_DEFS=1` opt-in。依据：调用密集 fib(26) 经 VM 慢 ~19%，库 import 慢 ~14%，启动持平（跨界成本超过字节码收益；复跑 `benchmarks/measure-vm.sh`）。保留为未来自研引擎的经验资产。
- **职责**：执行**位置编码字节码**——四槽一组（opcode/payload/i0/i1）的扁平向量，操作码数字是与 `bytecode.scm` 的 `vm-opcodes` 共享的 ABI；标签解析在 Scheme 侧完成，C++ 不认识任何符号指令拼写。VM 单值传递：多值是宿主派生形式，无专用 opcode。错误传播经帧感知展开协议（见演进债）。严格 arity 寄存器化属可选精化而非正确性需求。
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

## 宿主 ABI 规格（换宿主的交接合同）

未来以自研 VM 替换 s7 时，新宿主须按以下四档提供能力。此表是「所有能 Scheme 的都在 Scheme」的审计底稿：T3 是下沉行动清单，T0/T1/T2 是引擎的最小实现面。

### T0 引擎本体（重写项）

- 数据表示 + GC、求值循环/VM、错误传播协议（jump buffer 链、catcher 扫描，及 goldfish 的**帧感知展开钩子**三件套 `goldfish_vm_push_boundary/pop_boundary/s7_gf_vm_unwind`）。
- `vm-load` / `vm-enter` 位置编码字节码 ABI（L6 契约）；tiny reader C 面（`g-tiny-read`/`g-read-token`/`g-read-string`/`g-delimiter?`/`g-tiny-load`/`g-undefined`）。

### T1 语言必需原语（语义等价即可）

- s7 `initialize_misc` 三件套：`make-hook`、`call-with-values`、`multiple-value-bind`（注意 cwv 的 unspecified 折叠差异，见演进债）。
- 延迟形式名单：`catch call/cc call-with-current-continuation dynamic-wind apply` 及 8 个 `call-with-*` / `with-*` I/O 组合子。
- **核心语言**（tree-il 的语法面）：10 个 special forms——`quote define lambda if begin let let* letrec letrec* set!`——权威定义在 `goldfish/core/ir.scm` 的 `core-language` 表（含到 IR 节点的映射与文法）；其余一切形式按定义皆为调用。校验器 `validate-core-sexp` 供管线变更时执法。
- 其余为底层语言内建（算术/string/vector/hash-table/port…），按 R7RS-small + 必要扩展对齐，不逐一枚举。

### T2 平台能力（非语言；来自 OS/C 标准库，共 ~57 个 `g_*`）

- fs/path/env/process（34）、time（7）、hash/base64（8）、http（8，可选编译）、subprocess/uuid/misc。

### T3 下沉候补（空）

- 初版曾列 njson、`g_string-split`、`g_char-*` 为可下沉项；逐项调查后否决——`g_char-*` 是 Unicode 全码点范围表驱动（表数据天然属于底层，s7 无等价物），`g_string-split` 是 UTF-8 感知热路径，njson 是明确的性能取舍。**T0-T2 即宿主的完整合同面**；后续新增 C 原语前应先论证为何不能用 Scheme 表达。
- 已下沉实例：`liii/host-abi.scm`（108 个定义——exact/inexact 包装、list 系集合操作等 R7RS 语义修正层）。

## 演进债

- **活跃债（仅此一项）——s7 的 values/unspecified 折叠**：s7 求值参数循环丢弃 no_value 值（`if (val != sc->no_value)` 才拼接参数），故 `(values unspecified)` 到达 `values` 时已是零参——折叠发生在 `values` 之前，cwv 层无法区分「真零值」与「单 unspecified」。修复需改 s7 最热的求值路径，风险不可接受。移植代码以 unspecified 进多值协议时会踩坑：SRFI-165 的 `computation-with!` 已绕过（产 `'unspecified` 占位）。
- **宏层前置工程——syntax-case 已就绪**（2026-08 排查）：
  1. pattern 层 `dotted+ellipsis` 已修。
  2. template 层 `generate-temporaries` 已修：`with-syntax` 的 `inner-patvars` 提取与 `pattern-match` 对 plain list 的 `with-syntax` 支持；`let-values/define-values` 等 `gensym` 类宏可 `syntax-case` 化。
- **已归档（2026-08）**：VM 错误穿越帧重放（帧感知展开协议，见 L6）；缓存依赖追踪（deps 指纹入 gfo 第 5 字段，见 L2/L4）；from-source 双自举路径（删除后 kernel 可用全语言设施）；工具分发 JSON 层；dispatch 缺 return 导致的间歇 segfault。

