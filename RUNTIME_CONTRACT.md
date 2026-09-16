# Goldfish Runtime Contract

状态：R0–R2 已实现并通过 native artifact 链验证；R3/R4 尚未开始。
本文定义替换 vendored s7 后的宿主边界；现有 `gf0`/s7 bridge 不是此合同
的一部分，只是过渡实现。

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
- 最小 reader、port 和模块加载接口；
- OS、文件、时间、网络等平台原语。

### Scheme 层负责

- 派生语法和宏；
- record、`guard`、`error-object` 等语言库语义；
- module/import 的高层规则；
- SRFI、liii 和其他用户库；
- compiler passes、tree-il 变换和 cache 逻辑。

C++ 不为单个高层库增加业务语义。新增 C++ primitive 必须说明为何不能
用 Scheme 实现，并归入 platform 或 runtime 合同。

当前迁移期的 primitive 分层：

- `standard_primitives.cpp`：对象/类型、pair/vector 原子操作、整数数值、
  reader/port、environment/eval 等 runtime substrate。
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

为了优先保证正确性和可读性，第一版允许牺牲性能。GC backend 暂不冻结；
`src/runtime/heap.hpp` 当前只是 reference backend，不能成为继续扩展的
自研 GC 项目：

- `Value` 使用 tagged handle 或等价的稳定表示；
- heap object 统一 header 和类型 tag；
- GC 必须通过 `Heap`/`RootScope`/`Tracer` 接口提供；
- 所有跨调用保存的对象必须经过显式 root scope；
- 允许非移动 GC、额外分配和保守的数据结构；
- 不在第一版引入 generational GC、压缩指针、JIT 或 bytecode ABI。

在 runtime 对象合同稳定后，单独评估成熟 GC backend（优先 BDWGC）和当前
exact tracing backend。BDWGC 的 conservative 扫描、C++ destructor、foreign
resource finalization 和可测试性必须先有明确结论，不能仅因接入简单就成为
默认依赖。

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

### R3：自举和库迁移

新 runtime 能从 kernel source 重建 kernel artifact，并逐步迁移普通 Scheme
库、cache、CLI 和 REPL。

### R4：删除过渡层

默认入口切换到新 runtime；删除 gf0/s7 bridge、s7-specific compatibility
层、s7 构建文件和 vendored s7 源码。

## 验收原则

每个阶段都必须同时满足：

1. 新增 runtime 单元测试通过；
2. 受影响的 core/expander/library 测试通过；
3. 新 runtime 不新增 s7 依赖；
4. 失败能定位到 runtime、Scheme library 或 loader，而不是依赖“另一边也失败”；
5. 对象、异常、多值和 module identity 的行为有明确测试。

现有 s7 differential gate 在迁移期继续使用，但只作为迁移参照，不是最终
语义合同。新模块迁移后，应删除相应的 s7 bridge 测试和 HOF entry。
