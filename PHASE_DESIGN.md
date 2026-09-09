# PHASE_DESIGN — 相位实例化模型(v5,own 绑定严格相位)

状态: v4 已落地(per-level 实例化、N 层闭环)。v5 为已定方向、
待实施:移除最后的 own-defs 分歧——**own 值绑定只在 phase 0
可见**(Racket 一致);ewx-5 场景随之变为编译期错误,整编译与
per-form 不再有分歧。v1 补丁(回退链、min-merge、visit-only
判定位)保持废弃;phase≥2 缓存分段 deferred(无性能用户)。

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

## 3. 承重普查(2026-09-10,决定迁移面)

不受影响(纯模板宏,模板引用在使用点 phase 0;或纯 substrate):

- kernel:唯一宏 `define-public` 是 syntax-rules(core forms 均为
  过程式,无 phase-1 代码);
- liii/match.scm:约 50 个宏全部 syntax-rules;
- liii/prelude.scm、liii/reader.scm 的 lambda 宏:体只调 s7 原语
  与 substrate 可见的 syntax API(make-fresh-name 在 kernel)。

需迁移(展开期调用了 own 值定义):

- lib/syntax-case.scm + syntax-runtime.scm:parse-template 在
  transformer 执行期被调用(build-instantiate-call)→ 装置拆为
  一等库(§4),实现库 plain 导入;
- lib/cond-expand.scm:1 个助手(cond-expand-feature-satisfied?)
  → bfs region-ify;
- lib/define-record-type.scm:dr-* 助手 → bfs region-ify(纯展开期
  用途,无运行期用户,机械);
- lib/defmacro.scm:install-defmacro-transformer 只进输出(phase 0)
  安全;by-identifier 形态按 §2 文档化,仓库内无用户。

已否决的方案,勿复活:**逐相位塔**(region-ify parse-template 后
按嵌套 bfs 补相位 2、3、…)——给语义设相位上限,是 adhoc;
"任意相位可用"必须由解析公式的 ∀q 性质给出(原则三)。

## 4. 实现面

1. **装置成为一等库**:syntax-runtime.scm(本就是 core-forms-only,
   引导梯最底层)在 driver 处注册为独立的库;实现库对它
   plain 导入(level 0 视图 → substrate → 全相位可见),并把
   输出侧名字(syntax-case-dispatch、fast-instantiate 等)并入
   实现库导出表,用户侧零迁移。顺序:先拆库(纯增量,boot
   不变),后上严格门,否则引导中断;
2. **解析单线化**:expand.scm resolve 中 program/library 双分支
   (ref-strict-at-phase vs ref-at-phase)合一;own 子句加
   binding-kind × 相位门(值→0,宏→≥1),region 查找不变;
   exp-library.scm 两个同名函数合一;
3. **cond-expand / define-record-type**:助手 region-ify;
4. **缓存**:记录格式零变化(严格性在解析端);kernel 重建 +
   ccache 清。

## 5. 测试

- ewx-5 改写为 `check-catch 'unbound-variable`(与 case 4 对偶,
  翻 flip 消失);
- 新增:transformer 引 own 值 define → 错;引 region define → 成;
  引 substrate 导入 → 成;program 与 library 同规则(双分支已合);
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
