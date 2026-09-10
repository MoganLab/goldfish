# PHASE_DESIGN — 相位实例化模型(v5,own 绑定严格相位)

状态: v4 已落地(per-level 实例化、N 层闭环)。v5 已实施:own 值
绑定只在 phase 0 可见(Racket 一致);ewx-5 场景变为编译期错误,
整编译与 per-form 不再有分歧。生态迁移实录见 §3。v1 补丁(回退链、
min-merge、visit-only 判定位)保持废弃;phase≥2 缓存分段
deferred(无性能用户)。

目标:**Racket 式语义、正确实现、无 workaround、无回退多路、
单线代码。**

## 1. 核心原则(三条,各只有一条规则)

### 原则一:绑定身份 = (实例, 相位, 名字)  [已落地]

一次导入把库**实例化**为一个 unit inlet(s7 sublet)里的 cell 集。
同一库被以两个 level 导入 = **两个独立实例** = 体各运行一次、
gensym 各自独立。这是 Racket 的标准行为(required 两次,体跑两次),
不是副作用 bug,是语义。

机制已存在:`call-with-fresh-expand-unit` + 在 inlet 里求值
缓存产物;registry/runtime/being-loaded 按 level 键(level 0 裸名,
level ≥ 1 为 (level . name)),同库豁免冲突检查;冷捕获踩掉的
bare 项按快照恢复;预存 level-0 运行时模块在 load 后恢复。

### 原则二:解析公式(唯一一条)  [已落地]

在相位 q 解析名字 x 时,对 x 的各导入视图:

> **取 level ≤ q 中 level 最高者;若无任何视图,视为未绑定。**

- plain 导入(level 0):0 ≤ q 恒成立 → **所有相位可见**(substrate
  规则,ergonomics,文档化分歧——见 §6,是唯一保留分歧);
- `(for X expand)`(level 1):只在相位 1 可见;
- 同库 multi-level:高 level 在其相位内遮蔽 level 0。

Region store 只做精确相位:store[k] 的定义仅在相位 k 可见。

### 原则三:own 绑定严格相位(v5,替换 v4 的"实例内 trans-phase 保留")

同实例 own 绑定按种类定相位:

- 值 `define` → **仅 phase 0 可见**;
- 宏 `define-syntax` → phase ≥ 1 可见;
- region define(eval-when (expand) / begin-for-syntax)→ 仅
  home 相位可见(已落地)。

即 transformer 体不再能看到本库的 phase-0 值定义。展开期需要的
助手有两条相位正确的来路:本库 region(begin-for-syntax,精确
home 相位),或**另一库的 plain 导入**(substrate,全相位)。
**没有相位上限,也没有逐相位塔**:transformer 体内的标识符在
定义相位解析一次,之后在任意相位执行命中同一 binding(ewx-cross
的 phase-2 RHS 已验证);"任意相位可用"由解析公式的 ∀q 性质统一
给出,不靠逐相位实例化。

实现库自身不豁免:它的展开期装置(parse-template 等)拆为
一等库,实现库像任何普通库一样导入它(§4)。ewx-5 场景在包括
实现库在内的所有库统一为编译期错误。

## 2. 由严格化直接得到的语义(单一行为,各路径一致)

| 场景 | 语义 |
|---|---|
| transformer 体引用本库 phase-0 值 define | 展开期 unbound-variable(编译期错误;与 case 4 的 region→运行期引用对偶) |
| ewx-5(eval-when (expand) 中 set! 运行期变量) | 同上,编译期拒绝——两执行路径不再有分歧,冷/热翻 flip 消失 |
| define-macro m 某标识符(体引用 use-site 库 own define) | 展开期 unbound;经 plain 导入的助手仍可用(substrate) |
| REPL / per-form 路径 | 同一规则(行为单一;Racket REPL 同性) |

不需要的东西(相对 v1/v4 全部删除):

- ~~visit-only 模式与判定位~~;~~add-use min-merge~~;
- ~~region-lookup 跨 store 回退~~;
- ~~resolve 中 program/library 双分支(expand.scm)~~ —— 两函数
  合一,严格性不分 program/library;
- ~~"own 定义跨相位可见(实例内)"分歧行~~。

## 3. 承重普查与迁移实录(v5 落地)

不受影响(纯模板宏,模板引用在使用点 phase 0;或纯 substrate):

- kernel:唯一宏 `define-public` 是 syntax-rules(core forms 均为
  过程式,无 phase-1 代码);
- liii/match.scm:约 50 个宏全部 syntax-rules;
- liii/prelude.scm、liii/reader.scm 的 lambda 宏:体只调 s7 原语
  与 substrate 可见的 syntax API。

需迁移(展开期调用了 own 值定义),三种机制各按其场景:

- **实现库 boot 层(install.scm)**:`install-expansion-helper!` --
  parse-template、syntax-case-dispatch、fast-instantiate、
  sr-build-transformer、subst-ellipsis、cond-expand 助手、dr-*
  助手,注册为 primitive binding(相位无关,裸名发射,与内核
  syntax API 同类);boot 文件的 define 以 gensym 落 module,源名
  从不在 host 可见,故须同时 module-define! 源名供求值期解析;
- **liii/check.scm、liii/njson.scm**:小助手集合,内联为
  transformer 的 letrec*(syntax-case.scm 自包含惯用法;boot 文件
  与普通库的缓存都不携带 region 内容,region-ify 不可用);
- **goldfish/match.scm**:拆出 `(goldfish match expansion)`
  (goldfish/match/expansion.scm,约 35 个展开期函数),match.scm
  plain 导入它(substrate 规则 → 全相位),Racket 的
  match/expander + match/runtime 同型切分。

已否决的方案,勿复活:**逐相位塔**(region-ify 后按嵌套 bfs 补
相位 2、3、…)——给语义设相位上限,是 adhoc;"任意相位可用"
由解析公式的 ∀q 性质给出(原则三)。

## 4. 实现面(已落地)

1. **解析单线化**:expand.scm resolve 中 program/library 双分支
   合一;exp-library-ref-at-phase / ref-strict-at-phase 两函数
   合一,own 子句加 kind×相位门。门语义的最终形态:值(toplevel)
   绑定仅 phase 0;**transformer 绑定是派发关键字,全相位可见**
   (phase-0 形体经它派生,transformer 体在 phase ≥1 也用它 --
   最初版把 transformer 限到 ≥1,boot 的 let*-values 立即失展开,
   已纠正);region 绑定仅 home 相位(已落地);
2. **strict 变体删除**:exp-library-ref-strict(-at-phase) 是
   从未实现的占位,删;
3. **迁移**:见 §3;
4. **顺手修复**:make-fresh-name 在 kernel.scm 导出表中但缺席
   internal-names 注册,(import (goldfish)) 实际交付不了 --
   已补注册;
5. **缓存**:记录格式零变化(严格性在解析端);kernel 重建 +
   ccache 清。

## 5. 测试

- ewx-5 改写为 `check-catch 'unbound-variable`(与 case 4 对偶,
  翻 flip 消失);eval-when-test 的旧语义用例(set! phase-0 变量)
  同步改写为 v5 合法形,并附同断言;
- 新增(case 12/13):transformer 引 own 值 define → 错,program
  与 library 两路径同断言;region 助手 → 成;substrate 导入 → 成;
- per-level 双实例(体跑两次、变异隔离、`(meta 2)` 第三实例)不变;
- 全量 --all 为最终门。

## 6. 与 Racket 的分歧清单(唯一一条)

| 分歧 | 形式 |
|---|---|
| level 0 视图全相位可见 | substrate 规则(一条) |

own 绑定严格相位、region 精确相位、导入 level 公式、多 level =
多实例(体多次运行)均与 Racket 一致。移除 substrate 分歧即完全
一致,但它承托全部 transformer ergonomics(所有 lambda 宏依赖
plain 导入的 syntax API),移除 = 全生态改写,明确不取。
