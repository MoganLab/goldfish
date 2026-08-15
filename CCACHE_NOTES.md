# ccache 现状、阶段二计划与 read/write duality 调查

## 状态
- 分支 `jinser/sos-expander`，领先 main 94 提交。
- 本次会话已提交 `10cb5f96`：修复 expander ccache 的 round-trip 缺陷（cacheable-expansion? letrec 误判、reader 复数 2i、write-readable 特殊符号、guard→catch），并澄清 load-library! 不缓存的原因。

## ccache 当前架构（已实现）

- `compile-file-cached`（goldfish/expander/lib/install.scm）：展开结果缓存到 `$XDG_CACHE_HOME/goldfish/ccache/`，key=sha256(源路径)，按源文件 mtime+size 失效。
- `load`（goldfish/liii/reader.scm）：无宏文件整文件缓存 eval；含宏定义文件逐 form 回退；宏使用文件经 `cacheable-expansion?` 检测，不可缓存则 `.disabled` 回退。
- `load-library!`（module.scm）：**不缓存**（原因见下）。
- 验证：6301 行大文件冷 84.5s → 热 2.3s（36 倍）。scheme/base 215 PASS、srfi 13 PASS。

## load-library! 为何不缓存（已实证）

- 展开产物（lower core）只含运行时信息（`module-ref` + gensym），丢失 **expand-time registry**。
- `expand-define-library` 在展开时执行 `library-registry-set!`，把 `(exp-library . exports)` 存入 registry；import 的库靠 `library-record` 查 registry，从 exp-library 复制 binding 到目标库。
- 缓存命中若跳过展开 → registry 缺失 → import 的库展开期找不到该库 → `import: unknown library`。
- 因此依赖库必须每次从源码展开。ccache 只用于不被 import 的整文件。

## 阶段二方向（用户确认：缓存必须含宏编译产物）

用户明确指出：功能上宏也必须被编译，缓存不能仅仅是 expanded scheme。这要求缓存包含 phase 1 的宏编译产物（Guile 把宏编译成 `make-syntax-transformer` 存 .go，Racket 把宏存 .zo，加载时执行重建 transformer）。

### 调查发现（已实证）

1. **宏定义可重放**：`expand-eval '(define-syntax m ...)` 能把宏注册进 base library（transformer binding），注册后可用。→ 缓存命中时可用"重放宏定义"重建宏环境，无需序列化 transformer closure。
2. **但宏藏在 define-library 里**：srfi-2 顶层是 `(define-library ...)`，宏在 body。重放单元是 define-library 整体，展开必须执行（宏注册是展开的副作用）→ **宏库的展开无法跳过**。
3. **syntax-rules 编译产物依赖活 exp-library**：`syntax-case-dispatch` 的模式/模板是 syntax record，含 `(form context library)`，其中 library 是**整个 exp-library record**（bindings 表，可含 transformer closure、可递归共享 `#1=`）。s7 write 后 read 回来 record-type 不匹配、共享引用丢失 → **无法序列化**。
4. **值库的 registry 可序列化**：值库（liii error）的 binding 全部是 `toplevel-binding`（值 = `toplevel-ref(gensym home original)`，4 个符号字段）或 `primitive-binding`（symbol）→ 可完整重建。

### 分阶段计划

**阶段 2A：值库缓存**（低风险，先落地）
- 缓存命中时跳过展开（值库无宏）。
- 写缓存时把该库的 registry 信息（exports + 每个导出的 binding 描述）序列化进缓存；加载时重建 exp-library + `library-registry-set!` + 设置 runtime-registered。
- binding 描述编码：toplevel → `(toplevel <gensym> <home-libname> <original>)`；primitive → `(primitive <name>)`；遇到 transformer/core-form/module-form → 整库回退逐 form。
- 预计成功率 ~85%。收益：srfi-13/19 等大值库热加载归零。

**阶段 2B：宏数据化**（高难度，需先解决 read/write）
- 把 syntax-rules 宏编译成可序列化数据（模式/模板用 datum + scope 集表示，或存宏定义的源码 + 展开后代码），加载时重建 transformer。
- 涉及 syntax-runtime（parse-template）、transformer.scm 核心改造。
- 预计成功率 ~40-50%。收益：混合库（liii base）也能省展开。

### 关键障碍

- s7 `write` 不保证 read/write duality，与我们的 R7RS reader 不兼容。已发现的具体问题：
  - 特殊符号：`hello'` 裸写；`a b` 写成 `(symbol "a b")`（读回是列表）——已用 `write-readable` 打补丁（竖线转义）。
  - syntax record / exp-library record：write 输出 record 结构，read 回 record-type 不匹配。
  - closure：无法序列化。
  - 复数：s7 write `2i`，reader 原来不识别（已修）。
- 这使"缓存宏编译产物"不可行——阶段 2B 的前提是先解决 read/write duality。

## 下一步：先解决 read/write duality（用户决定）

前置工作：让 read 和 write 满足 duality（write 出的数据可被 read 读，且与 write 的数据相等）。

### 待调查的问题清单

1. s7 的 `write` 对哪些类型输出不可被我们的 reader 读回 / 读回不等？
   - symbol（特殊字符、含 `|`、非 ASCII）
   - record（syntax/context/exp-library/binding/toplevel-ref）
   - closure / 不可序列化对象
   - 数字边界（复数、nan/inf、精度）
   - 共享引用 / 循环引用（`#1=`）
2. 我们的 reader（goldfish/liii/reader.scm）支持哪些语法？
3. 方案选择：
   - A. 用 s7 的 `write` 并逐类型验证/修补（现状思路，遇 record/closure 必失败）。
   - B. 自定义完整 `write`（为我们的 reader 保证 duality），覆盖 symbol/pair/vector/bytevector/string/char/number/record。
   - C. 为 record 实现专门的序列化协议（syntax record 数据化），这才是阶段 2B 的真正需求。

### 已保留的回归测试

- `tests/scheme/base/cache-roundtrip-test.scm`：复数 2i/-2.5i/1.0+1.0i、特殊符号 `hello'` 竖线转义、cacheable-expansion? letrec 递归。

### 相关代码文件

- `goldfish/expander/lib/install.scm`：compile-file-cached / compile-cache-hot? / write-readable / compile-write-cache。
- `goldfish/liii/reader.scm`：R7RS reader（read / read-forms / pure-imaginary-number / cacheable-expansion? / load）。
- `goldfish/expander/kernel/transformer.scm`：eval-transformer（宏编译 seam，`(eval (lower sexp))` 即宏定义代码的产出点）。
- `goldfish/expander/lib/module.scm`：expand-define-library（registry-set!）、import-*（复制 binding）、load-library!。
- `goldfish/expander/kernel/context.scm`：binding 记录类型（toplevel/primitive/transformer/core-form/module-form）。
- `goldfish/expander/kernel/exp-library.scm`：exp-library（binding 表）。
- `goldfish/expander/lib/syntax-runtime.scm`：parse-template / syntax-case-dispatch（模板编译产物）。

## read/write duality 实现（已提交 10cb5f96 之后新增）

### 问题
s7 的 write 面向 s7 自己的 reader，不保证被我们的 R7RS reader 读回：
- 特殊符号：`hello'` 裸写不可读；`a b` 写成 `(symbol "a b")` 读回是列表
- record：输出 `#(#(record-type <name> ...) ...)`，read 回 record-type 身份丢失
- closure：输出 `#<lambda>`，不可序列化

### 实现（reader.scm + install.scm）
1. **write-roundtrip**（reader.scm）：保证与 read duality 的 writer
   - 数据层：symbol 竖线转义（`|hello'|`）、pair（含 dotted）、vector、bytevector、string、char、number、boolean
   - record 层：`#g(tag field ...)` 序列化 syntax/exp-library/binding/toplevel-ref，read-sharp 加 `#g` 分派重建
   - **图感知**：两遍算法（参考 Racket print-graph），共享/循环用 `#n=`/`#n#` 标记——exp-library 的 bindings 自引用（toplevel-ref home 指回库）必须用此处理，否则无限递归
   - **快路径**：has-record? 扫描，纯数据（展开产物）走单遍线性输出，避免图遍历的 O(n²)
   - **procedure 拒绝**：遇到 closure 抛错（明确失败而非静默损坏缓存）
2. **read-goldfish-record**（reader.scm）：`#g` 读取，用 read-expr（非 read，避免重置 label 表）
3. **install.scm**：compile-write-cache 改用 write-roundtrip，删除 write-readable

### 验证
- 数据层：特殊符号/复数/字符串/vector 等全部 round-trip（nan 例外：nan≠nan 是固有属性）
- record 层：binding/toplevel-ref/exp-library round-trip，自引用 exp-library 的 home 恢复指向重建库
- 缓存：大文件冷 100s → 热 2.35s（42 倍）；scheme/base 215 PASS、srfi 13 PASS
- 回归测试 `cache-roundtrip-test.scm`：18 correct（复数/符号/record/图标记/letrec）

### 已知限制
- closure（macro transformer）不可序列化——含宏库无法用此 writer 缓存（load 已用 any-macro-def? 过滤）
- 图遍历对含 record 的大数据仍慢（展开产物无 record 不受影响）
