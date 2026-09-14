# Core 语义（新引擎的求值合同）

求值对象：`ir->core`（`goldfish/core/ir.scm`）输出的 lowered sexp，
即冻结基线表 D 的 14 个 form。前端负责把其他一切 desugar 掉；
引擎只实现本页规则。所有 oracle 输出均以当前 s7 后端实测为准
（`bin/gf /tmp/kilo/probe-*.scm`，2026-09）。

## 失真 lowering（引擎必须知道的三件事）

- **裸符号三合一**：`primitive-ref`/`lexical-ref`/`toplevel-ref`
  lower 后都是裸符号，解析顺序＝词法帧链 → toplevel cell →
  primitive 表。warm-cache 曾出现冷 `(module-ref …)` / 热裸符号
  两种形状——两种都必须解析到同一绑定（基线回归用例 M1）。
- **void**：`<void>` lower 为 `(quote void)`；`if` 无 else 分支、
  空 `begin` 求值为 `#<unspecified>`。
- **`let-values` → `call-with-values`**；named-let 已降为
  `letrec`+call；`primcall` 即裸调用；`case-lambda` 在前端按
  arity 分发，core `lambda` 只有单 formals（`arity->formals` 规则）。

## 逐 form 规则

- `quote`：datum 原样返回（含 improper list）。
- `define`：只 `(define name exp)`（函数糖前端已脱）。
  顶层重定义允许（后写覆盖）；先求值后绑定。
- `lambda`：单 formals（proper/improper/rest）+ body… 为隐式
  `begin`；闭包捕获定义时环境。
- `if`：test 非 `#f` 取 consequent；无 else 且 test 为 `#f`
  得 `#<unspecified>`。
- `begin`：顺序求值，空得 `#<unspecified>`，只取末值。
- `let`：init 在**外**环境求值后绑定（不可自指）；
  `let*` 嵌套 `let`；body… 隐式 `begin`。
- `letrec`：槽先占位再求 init；**init 求值期间读未赋值槽
  按 R7RS 报错**（R7RS-small 4.2.2：init 不得引用任一被绑变量，
  违者为 error）。s7 后端此处宽容（给出 `#<undefined>`），
  新引擎不跟随——全量测试集无对此宽容的依赖（`#<undefined>`
  断言全在 reader 字面量测试），差分门中此条以 R7RS 为准；
  `letrec*` 按序求值+绑定，后 init 可见前绑定
  （`(letrec* ((a 1) (b (+ a 1))) b)` → `2`），仅前向引用报错。
- `set!`：词法帧 → toplevel 顺序找 cell；缺失则**报错**
  （实测 `set!-undefined-error` 可被 guard 捕获）。
- `values`：产多值；`(values)` 为零值。
- `call-with-values`：producer 多值喂给 consumer。
  **主动修正**：s7 把单个 unspecified 在到达 consumer 前折叠
  成零值（实测 `(values (if #f #f))` → 消费端得 `()`），
  新引擎按 R7RS 交付**单值**（消费端得 `(unspecified)`）。
  SRFI-165 的占位 workaround 在新语义下继续通过。
- `module-ref`/`module-set`：**loader 协议，非用户语法**
  （用户顶层求值报 unbound）。`(module-ref 'lib 'name)` 查
  已加载库注册表；`module-set` 形式为
  `(set! (module-ref 'lib 'name) exp)`。引擎须实现 s7 兼容的
  运行时模块/inlet 注册表（属 T0，未在旧 LAYER.md 点名，现补入）。
- **continuation 边界**：捕获不跨 s7call 边界（跨即明确 error）。
  引擎边界即天然 delimiter（对应业界 REPL-delimit 实践）；R7RS
  单引擎语义内行为完整，跨宿主调用不在语义承诺内。
- **s7 continuation 不跨 s7call 帧寿命**（同规则另一方向，已实证）：
  s7 原生 call/cc 在库内部捕获、经 gf0 的 s7call 多次调用后重入，
  会 longjmp 进已返回的 C++ 帧（use-after-return，表现为垃圾值
  20/unspecified 而非崩溃）。因此 s7-call/cc 库（如 srfi-158
  make-coroutine-generator）只能从 s7 求值侧调用，不进 M3 双门；
  gf0 原生 call/cc（堆 Kont，无 C 帧）不受此限（m2a-generator 为证）。
  这正是 Filinski 框架条件被违背的实例：两运行时控制模型不复合。
- **stale fence（已实现，fail-closed）**：`g_gf0-import-inlet` 在 rootlet
  装 serial-guard wrapper 覆盖 `call/cc` 双名；每次 s7call 压新 token，
  invoke 时与当前最内 token 比对，不等即 `gf0-stale-continuation` 错误。
  用户回调是 gf0 box（进 s7 必嵌新 s7call），故协程首 yield 即错——
  这是对的：membership 谓词会放行 longjmp 穿活 C++ 帧（pin 泄漏，
  1M yield 即 OOM）。定序铁律：fence 先行（`(gf0-import inlet #f)`，
  不碰快照）→load（call/cc 装线期）→全量 import（frame-0 快照须覆盖
  已载库；level-0 load 的 view 直接落在 rootlet）。见
  `tools/check-gf0-guards.sh`（s7 侧 10/20/eof，gf0 侧 stale×3）。

## 实测 oracle（差分门，M2 照单验收）

s7 列为当前后端实测；guile 3.0.11 / racket 9.2 双方验证
（`/tmp/kilo/probe-guile.scm`、`/tmp/kilo/probe-racket.rkt`），
两处分歧双参考实现均站 R7RS：

| 程序 | s7 | guile / racket | 新引擎 |
|---|---|---|---|
| `(cwv (λ () (values 1 2)) list)` | `(1 2)` | `(1 2)` | 同 |
| `(cwv (λ () (values)) list)` | `()` | `()` | 同 |
| `(cwv (λ () (values (if #f #f))) list)` | `()`（bug） | `(#<unspecified>)` / `(#<void>)` | 单值（按 R7RS，本页第一处主动修正） |
| `(cwv (λ () 7) list)` | `(7)` | `(7)` | 同 |
| `(if #f 1)` / `(begin)` | `#<unspecified>` | 未单测（R7RS 文本：结果为 unspecified） | 同（单 unspecified 值） |
| `(define r 1)(define r 2)` | `2` | `2` | 同 |
| `(set! missing 1)` | error | 双 ERROR | error |
| `(letrec ((a b) (b 1)) a)` | `#<undefined>`（s7 宽容） | 双 ERROR | error（按 R7RS，本页第二处主动修正） |
| `(letrec* ((a 1)(b (+ a 1))) b)` | `2` | `2` | 同 |
| `(define (f x) (+ x 1))(f 41)` | `42`（前端脱糖） | `42` | 同 |
| `(module-ref …)` 用户顶层 | unbound-variable | 不适用（loader 协议） | 同（loader 内才可用） |
| 裸 `set-union` vs `(module-ref '(srfi srfi-113) 'set-union)` | 同一过程 | 不适用 | 同一过程 |

## 错误 key 对等（check-catch 只比 key）

- 未绑定（查/改）：`unbound-variable`（与 s7 同键；guile/s7 双 oracle 实测）。
- 闭包元数、`values` 错位：`wrong-number-of-args`。
- 非过程调用：**委托 s7 的 apply**，原生错误对象原样穿过
  （如 `(apply 1 …)` 的 `syntax-error`），check-catch 逐字匹配。
- `letrec` 未初始化读：`gf0-error`（s7 无此错，保持 R7RS-strict 分歧）。
- 多值调用位统一 splice（实测 oracle）：实参值表 concat 后做元数检查。
  `list`/`+`/`vector` 等原语接受展开（`(+ (values 1 2) 3)` → 6）；
  闭包得展开后的实参（元数不符即 `wrong-number-of-args`）；
  `(values <multi>)` 展开；`apply` 非尾实参加入展开；
  `if` 取首值（零值判真）。`define`/`set!` 位仍须单值。

## 未决

- `validate-core-sexp` 现为桩（非 pair 返 `#f`），管线执法靠
  `core-form?`/`core-node-of`；新引擎上线时二选一：补全它或删之。
- `call/cc`/`dynamic-wind` 不可委托：s7 continuation 只捕获 s7 栈，
  gf0 闭包传给 s7 的 `call/cc` 被拒（实测 wrong-type-arg），且即使
  递入 s7 闭包、escape 也会丢 gf0 的 C++ 求值帧。须引擎自有控制栈
  （M-VM），在此之前 gf0 高阶互操作止于 trampoline 回调
  （`g_gf0-apply`，限单值）。
