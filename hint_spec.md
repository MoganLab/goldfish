# Goldfish Scheme Hint 类型接口规范

## 1. 状态

本文档是 Goldfish Scheme 类型接口与静态类型检查机制的草案。

## 2. 目标

Hint 机制为 Scheme 库提供独立于实现的类型信息，并服务于：

- 通过 `bin/gf doc` 展示函数类型签名。
- 通过 `bin/gf check` 生成、校验和批量加载类型接口。
- 在源码文件中逐步支持参数、返回值和局部表达式的静态类型检查。
- 让类型信息可以被编辑器和其他开发工具复用。

## 3. 核心约束

### 3.1 不影响程序运行

类型信息不得改变原库的运行行为：

- 不修改 `define`。
- 不使用宏包装函数定义。
- 不生成函数的代理或检查版本。
- 不在函数调用时执行类型谓词。
- 不要求业务代码导入 hint 库。
- 正常运行库代码时不加载 hint 库。

Hint 库只由文档、检查和其他开发工具显式加载。

### 3.2 静态检查保持保守

`gf check` 只报告能够确定的类型冲突。无法静态确定的表达式具有分析类型
`unknown`，不得仅因无法确定类型而报错。

`unknown` 是静态分析过程中的状态，不是业务代码需要处理的运行时值。

## 4. 库命名约定

原库与 hint 库通过在原库名前添加 `hint` 建立一一对应关系：

| 原库 | Hint 库 | 标准查询函数 |
| --- | --- | --- |
| `(scheme base)` | `(hint scheme base)` | `hint-scheme/base` |
| `(scheme write)` | `(hint scheme write)` | `hint-scheme/write` |
| `(liii list)` | `(hint liii list)` | `hint-liii/list` |
| `(liii xyz)` | `(hint liii xyz)` | `hint-liii/xyz` |

查询函数名由原库各部分的名称用 `/` 连接，并添加 `hint-` 前缀。

### 4.1 文件路径

Hint 库集中存放在 `goldfish/hint/` 下：

```text
(scheme base)    -> goldfish/hint/scheme/base.scm
(scheme write)   -> goldfish/hint/scheme/write.scm
(liii list)      -> goldfish/hint/liii/list.scm
(liii xyz)       -> goldfish/hint/liii/xyz.scm
```

多级库名按目录展开。例如 `(liii foo bar)` 对应：

```text
goldfish/hint/liii/foo/bar.scm
```

## 5. Hint 库接口

每个 hint 库必须导出该原库专属的标准查询函数，可以选择导出单个过程的
签名数据，但不得重新导出原库过程。

Hint 库之间零依赖：任何 `(hint ...)` 库都不得导入其他 hint 库。示例中的
`(scheme base)` 仅提供 `define`、`case` 等普通 Scheme 设施，不构成 hint
依赖。所有 hint 库都可以按任意顺序独立加载。

以 `(hint liii xyz)` 为例：

```scheme
(define-library (hint liii xyz)
  (import (scheme base))
  (export hint-liii/xyz)
  (begin
    (define hint-buzz
      '((string? -> integer?)))

    (define hint-fizz
      '((integer? integer? -> string?)
        (string? string? -> string?)))

    (define (hint-liii/xyz name)
      (case name
        ((buzz) hint-buzz)
        ((fizz) hint-fizz)
        (else #f)))))
```

### 5.1 标准查询函数

```scheme
(import (hint liii xyz))

(hint-liii/xyz 'buzz)
;; => ((string? -> integer?))

(hint-liii/xyz 'not-exported)
;; => #f
```

标准查询函数是强制导出的接口。它接受一个符号：

- 若符号对应该库自己实现且已声明类型的过程，返回其签名列表。
- 即使过程只有一个签名，也返回只含一个元素的列表。
- 若符号没有本地类型声明，或不属于原库导出范围，返回 `#f`。
- 查询函数不调用被查询过程，不执行类型检查，也不修改任何过程。

### 5.2 单函数签名数据的可选导出

每个过程使用一个普通变量保存签名，例如 `hint-buzz`。这些变量只使用普通
`define`，不需要宏，默认仅在 hint 库内部可见。

Hint 库可以选择导出 `hint-buzz` 等单函数签名数据，但这种导出是可选的。例如：

```scheme
(export hint-liii/xyz hint-buzz)
```

`hint-liii/xyz` 必须导出，`hint-buzz` 是否导出由库维护者决定。`gf doc`、
`gf check` 和其他通用工具不得依赖这些可选绑定，只能依赖标准查询函数和
后文定义的统一解析接口。

### 5.3 只在实现库中标记

每个过程的签名只在实际实现该过程的库中标记一次。再导出库不复制签名，也
不需要为再导出过程定义 `hint-xxx` 变量。

例如 `every` 由 `(srfi srfi-1)` 实现，其签名只存放在
`(hint srfi srfi-1)`。即使 `(liii list)` 再导出 `every`，
`(hint liii list)` 也不添加 `every` 条目。

工具通过统一的 `hint-ref` 接口查询完整导出接口。`hint-ref` 位于工具侧，
不会在各 hint 库之间建立导入或加载依赖：

```scheme
(hint-ref '(liii list) 'every)
```

`hint-ref` 的解析过程为：

1. 确认 `every` 确实由 `(liii list)` 导出。
2. 查询 `(hint liii list)` 的本地签名；若存在则直接返回。
3. 若过程来自再导出，根据原库的导入、导出和重命名关系确定来源库。
4. 递归查询来源库，最终调用 `(hint-srfi/srfi-1 'every)` 获取唯一签名。

无法自动确定来源时，可以补充少量 re-export 来源元数据，但仍不得复制类型
签名，也不得让再导出库的 hint 导入来源 hint。`gf doc` 和 `gf check` 查询库
接口时统一使用 `hint-ref`，因此再导出过程与本地过程都能获得类型信息。

## 6. 类型签名格式

### 6.1 固定参数和单返回值

完整签名由参数类型、`->` 和返回类型组成：

```scheme
(string? integer? -> string?)
```

例如：

```scheme
(hint-liii/xyz 'buzz)
;; => ((string? -> integer?))
```

表示 `buzz` 接受一个字符串参数，并返回一个整数。

多个参数按顺序列出：

```scheme
(string? integer? -> string?)
```

无参数过程使用：

```scheme
(-> string?)
```

### 6.2 一个函数的多种签名

同一个函数可以具有多种签名。Hint 数据按函数名组织，每个函数名对应一个签名
列表：

```scheme
'((convert ((string? -> integer?)
            (integer? -> string?)
            (string? integer? -> integer?))))
```

查询结果始终保留这一层列表结构：

```scheme
(hint-liii/xyz 'convert)
;; => ((string? -> integer?)
;;     (integer? -> string?)
;;     (string? integer? -> integer?))
```

多种签名表示同一过程的不同调用方式，不表示运行时重载分派。签名顺序不影响
静态检查结果。

检查调用时：

- 先保留参数数量匹配的签名。
- 只要仍存在可能匹配的签名，就不能仅因其他签名不匹配而报错。
- 只有实参与所有候选签名都发生确定冲突时，才报告类型错误。
- 若所有可行签名的返回类型相同，调用结果使用该返回类型。
- 若可行签名具有不同返回类型，首版可将结果保守标记为 `unknown`，后续再使用
  联合类型表达。

例如上述 `convert` 的以下调用可以匹配：

```scheme
(convert "42")   ; 匹配 (string? -> integer?)
(convert 42)     ; 匹配 (integer? -> string?)
(convert "x" 2)  ; 匹配 (string? integer? -> integer?)
```

以下调用与所有签名冲突：

```scheme
(convert #t)
```

### 6.3 类型名称

首版类型名称采用 Scheme 谓词风格的符号，例如：

- `string?`
- `integer?`
- `number?`
- `boolean?`
- `char?`
- `symbol?`
- `list?`
- `pair?`
- `vector?`
- `procedure?`
- `any?`

这些符号首先是类型接口数据。静态检查器根据类型名称和已知字面量进行匹配，
不会在程序运行时调用对应谓词。

`any?` 表示允许任意类型。自动生成接口骨架时可以使用 `any?` 作为占位类型，
之后由开发者逐步收紧。

### 6.4 联合类型与 `#f`

很多过程的返回值是“`#f` 或其他类型”。使用 `or` 表示联合类型：

```scheme
(or #f string?)
(or #f pair?)
(or #f number?)
```

例如：

```scheme
string->number : (string? -> (or #f number?))
assoc         : (any? alist? -> (or #f pair?))
```

联合类型可以包含两个或更多成员。`any?` 是顶层类型，因此 `(or #f any?)` 等
包含 `any?` 的联合类型直接等价于 `any?`。

类型兼容采用集合包含语义：

- `string?` 可以传给期望 `(or #f string?)` 的参数。
- `(or #f string?)` 可以传给期望 `any?` 的参数。
- `(or #f string?)` 不能直接传给期望 `string?` 的参数，因为它可能为 `#f`。
- 实际联合类型的每个成员都被期望类型接受时，整体才兼容。

Scheme 中只有 `#f` 为假，因此对 `(or #f T)` 类型的值进行普通真值判断后，
可以在真分支中消去 `#f`：

```scheme
(let ((n (string->number text)))
  (if n
    (+ n 1)  ; 此分支中 n : number?
    0))
```

记录谓词和其他类型谓词的收窄规则与联合类型规则可以组合使用。

多种签名与联合类型承担不同职责：多种签名描述不同参数形式及其返回类型之间
的对应关系；联合类型描述一个值可能具有的多种类型。

### 6.5 `define-record-type` 记录类型

记录类型直接复用 `define-record-type` 生成的谓词名作为名义类型。例如原库中有：

```scheme
(define-record-type <person>
  (make-person name age)
  person?
  (name person-name)
  (age person-age set-person-age!))
```

对应 hint 数据可以声明为：

```scheme
'((make-person     ((string? integer? -> person?)))
  (person?         ((any? -> boolean?)))
  (person-name     ((person? -> string?)))
  (person-age      ((person? -> integer?)))
  (set-person-age! ((person? integer? -> any?))))
```

其中：

- 构造器返回记录类型 `person?`。
- 记录谓词 `person?` 自身作为过程时，接受 `any?`，返回 `boolean?`。
- 访问器接受 `person?` 类型的值，返回对应字段类型。
- 修改器接受 `person?` 类型的值和对应字段类型。R7RS 修改器的返回值未规定，
  首版可用 `any?` 表示。
- `person?` 在函数名位置表示被描述的过程，在类型位置表示该谓词识别的记录
  类型，两者不会混淆。
- 不要求原库导出记录定义中的 `<person>` 句法标识。

记录类型采用名义类型语义。静态检查器内部应以“定义库加记录谓词名”区分不同
库中同名的记录类型。

构造器调用结果可以传播为记录类型：

```scheme
(person-name (make-person "Ada" 42))
```

上例中构造器返回 `person?` 类型，因此访问器参数匹配，整个表达式可推导为
`string?`。

以下调用存在确定的类型冲突：

```scheme
(make-person "Ada" "42") ; age 需要 integer?
(person-name 42)           ; 参数需要 person? 类型
```

静态检查器还应识别记录谓词，并在明确的分支中进行类型收窄：

```scheme
(if (person? x)
  (person-name x)
  #f)
```

在真分支中，`x` 可以从 `unknown` 或较宽泛类型收窄为 `person?` 类型。首版不
要求在假分支中推导排除记录类型后的精确类型。

### 6.6 多返回值和复杂类型

后续版本需要支持：

- 多返回值。
- 可选参数和 rest 参数。
- 高阶函数。
- 参数化类型，例如列表元素类型。
- 更精确的返回路径和返回值约束。
- 记录类型继承或其他记录系统扩展。

这些能力不得破坏固定参数签名和记录名义类型的基本格式。

## 7. `gf doc` 集成

`gf doc` 查询函数文档时，通过 `hint-ref` 查询原库的完整导出接口。对于本地
实现过程，`hint-ref` 调用对应 hint 库的标准查询函数；对于再导出过程，则由
工具侧追踪到唯一的实现库签名。

例如查询 `(liii xyz)` 中的 `buzz`：

```bash
bin/gf doc liii/xyz "buzz"
```

若存在类型信息，输出中包含：

```text
;; 类型: (string? -> integer?)
```

若一个函数具有多种签名，则逐项展示：

```text
;; 类型:
;;   (string? -> integer?)
;;   (integer? -> string?)
```

若原库没有可用签名，或 `hint-ref` 返回 `#f`，则保持现有文档输出，不将缺少
类型信息视为文档查询错误。

## 8. `gf check` 集成

`gf check` 新增 `type-hint` 相关能力，负责接口生成、接口校验和源码静态检查。
普通检查模式保持只读；生成接口必须使用显式生成模式。

### 8.1 生成接口骨架

工具可以根据原库的导出过程和可确定的参数列表，生成对应 hint 库骨架：

```bash
bin/gf check --only type-hint --generate liii/list
```

目标文件为：

```text
goldfish/hint/liii/list.scm
```

生成器负责：

- 创建标准 `define-library` 结构。
- 生成唯一的标准查询函数。
- 为可识别的本地实现导出过程生成签名条目。
- 为本地 `define-record-type` 生成的构造器、谓词、访问器和修改器生成签名条目。
- 对可识别的再导出过程记录来源关系供 `hint-ref` 解析，而不是复制签名数据或
  生成重复的单函数声明。
- 尽可能保留参数数量、记录类型和字段对应关系。
- 对无法确定的参数和字段类型使用 `any?` 占位。

生成器不承诺自动推导准确的语义类型。开发者需要审查并收紧生成结果。

### 8.2 校验类型接口

接口校验包括：

- 原库自身实现的导出过程是否具有本地类型声明。
- 再导出过程是否保持零本地签名，并能由 `hint-ref` 解析到实现库签名。
- 是否存在重复的函数声明或完全相同的重复签名。
- 是否声明了不属于原库导出范围的过程。
- Hint 库是否违规导入了其他 hint 库。
- 签名语法是否合法。
- 同一函数中可确定的签名参数数量是否与原过程参数列表一致。
- 记录构造器签名是否与原构造器参数列表一致。
- 记录访问器和修改器是否引用已定义的记录类型及字段。
- 标准查询函数的名称和导出是否符合约定。

新增本地实现的导出过程或记录接口后，遗漏对应 hint 信息应能被检查发现。

### 8.3 批量建立类型环境

检查源码文件时，`gf check` 可以读取文件的 `import`，将原库名映射为 hint
库名，并通过 `hint-ref` 获取所需签名。`hint-ref` 负责组合本地声明和再导出
来源声明。

例如：

```scheme
(import (liii xyz))
```

对应查询：

```scheme
(hint-ref '(liii xyz) 'buzz)
```

对于 `(liii xyz)` 自己实现的 `buzz`，`hint-ref` 最终调用：

```scheme
(hint-liii/xyz 'buzz)
```

多个 hint 库使用不同的查询函数名，因此可以同时导入而不会因统一查询函数名
发生冲突。

### 8.4 文件级静态检查

首版文件级检查逐步覆盖：

1. 数字、字符串、布尔值、字符、符号和空列表等字面量类型。
2. 固定参数过程的实参数量和实参类型。
3. 过程调用返回值的类型传播。
4. 简单 `define` 和局部绑定中的类型传播。
5. 可确定的函数返回值与声明返回值之间的冲突。
6. `define-record-type` 的构造结果、字段访问、字段修改和谓词分支收窄。
7. 同一函数的多种签名候选筛选及返回类型传播。
8. 联合类型兼容判断，以及普通真值分支对 `(or #f T)` 的收窄。

例如，已知：

```scheme
(hint-liii/xyz 'buzz)
;; => ((string? -> integer?))
```

则：

```scheme
(buzz 42)
```

应报告类似错误：

```text
type-hint: example.scm:10: buzz 第 1 个参数需要 string?，实际为 integer?
```

返回值类型参与后续表达式检查：

```scheme
(string-length (buzz "hello"))
```

若 `string-length` 的接口为 `(string? -> integer?)`，则上例存在确定的类型
冲突，因为 `buzz` 返回 `integer?`。

对于无法确定的表达式：

```scheme
(buzz (read))
```

若 `(read)` 的结果无法确定，则其分析类型为 `unknown`，首版不直接报告参数
类型错误。

### 8.5 没有任何错误的判定条件

`(check-hint '(liii alist))` 表示利用 `(liii alist)` 及其依赖库的 hint，对
`(liii alist)` 的实现代码进行静态类型检查。检查通过时返回 `#t`。

“没有任何错误”的准确含义是：

> 在现有 hint 和静态分析能力范围内，所有可判定的类型约束均满足，没有发现
> 确定的类型冲突。

具体包括：

1. 每次过程调用至少能匹配一个候选签名。
2. 实参数量符合至少一个候选签名。
3. 实参类型与相应候选签名的参数类型兼容。
4. 函数所有可分析的返回路径都符合声明的返回类型。
5. 已分析的分支、局部绑定和记录操作不存在确定冲突。

检查通过不等于已经从数学上证明程序不会发生运行时类型错误。`unknown` 默认
不算错误，详细模式可以列出未能完成验证的位置；严格模式可以将其标记为
“检查不完整”，但不得称为已证实的类型错误。

### 8.6 `and` 推导与谓词收窄示例

假设依赖 hint 为：

```scheme
list? : (any? -> boolean?)
pair? : (any? -> boolean?)
every : (procedure? list? -> boolean?)
```

被检查实现为：

```scheme
;; alist? : (any? -> boolean?)
(define (alist? l)
  (and (list? l) (every pair? l)))
```

初始环境中 `l : any?`，推导过程为：

| 表达式 | 推导结果 | 依据 |
| --- | --- | --- |
| `(list? l)` | `boolean?` | 参数 `any?` 合法 |
| `pair?` | 签名为 `(any? -> boolean?)` 的过程 | 查询依赖 hint |
| `(every pair? l)` | `boolean?` | `pair?` 是过程，真分支中 `l` 收窄为 `list?` |
| 整个 `and` | `boolean?` | 假分支返回 `#f`，真分支返回 `boolean?` |

`and` 的后续操作数处于前面条件的真值上下文中。因此在第二个操作数中，
`(list? l)` 为真，可以将 `l` 从 `any?` 收窄为 `list?`。该规则同样适用于
`string?` 和记录谓词等其他可识别谓词。

上述实现的所有关键表达式都有明确类型，且实现返回类型与声明一致，因此没有
任何错误。

### 8.7 确定出错的条件

只有实际类型与期望类型确定不兼容时才报告类型错误，包括：

1. **参数数量错误**：没有任何候选签名接受实际参数数量。
2. **参数类型错误**：实参与所有候选签名均发生确定类型冲突。
3. **返回值错误**：存在可分析的返回路径，其实际返回类型与声明确定不兼容。
4. **高阶参数错误**：过程参数位置传入了确定为非过程的值，或过程签名确定不
   匹配。
5. **记录操作错误**：构造器字段、访问器或修改器参数与记录类型或字段类型确定
   不兼容。

- 实际联合类型的每个成员都被期望类型接受时，整体才兼容；若联合类型中保留
  了不被期望类型接受的成员，则不能在未收窄的情况下视为兼容。

一个调用只要仍有可行签名，就不能因为其他签名不匹配而报错。只有所有候选
签名都无法匹配时才报告错误。`integer?` 可以满足 `number?` 等较宽泛类型；
`any?` 和 `unknown` 不得被当作确定冲突处理。

### 8.8 错误定位与问题辨别

诊断应定位到最小的责任表达式，而不是只报告整个 `define`。例如：

```scheme
(define (alist? l)
  (and (list? l) (every pair? 42)))
```

应定位到 `42`：

```text
goldfish/liii/alist.scm:6:27:
every 的第 2 个参数类型不匹配
  期望: list?
  实际: integer?
  表达式: 42
```

若实现为：

```scheme
;; alist? : (any? -> boolean?)
(define (alist? l)
  (and (list? l) 42))
```

则真分支会返回 `integer?`，应报告函数返回类型与声明冲突，并定位到返回表达式。

每项结构化诊断至少应包含：

- 文件、行号和列号。
- 原库和所在函数。
- 诊断类别。
- 最小责任表达式。
- 期望类型和实际推导类型。
- 调用参数位置或返回路径等上下文。
- 必要时的签名候选和简要推导链。

静态检查器只能确定“源码与 hint 不一致”，不能武断地认为永远是源码错误。
返回类型冲突既可能来自实现错误，也可能来自 hint 声明错误。诊断应展示推导
依据，由开发者判断修改哪一侧。

诊断类别至少区分：

| 类别 | 含义 | 是否属于确定类型错误 |
| --- | --- | --- |
| 源码与 hint 冲突 | 二者确定不一致 | 是 |
| Hint 格式或接口错误 | 签名或 hint 库自身不合法 | 单独报告 |
| 分析信息不足 | 结果为 `unknown`，无法完成验证 | 否 |

### 8.9 检查边界

`gf check` 不执行被检查的业务源码。Hint 库只提供类型数据查询，静态分析、
类型推导和诊断均由工具侧完成。

首版不追求覆盖 Scheme 的全部动态行为，也不以完整、可靠的类型推导为目标。
优先保证确定冲突的诊断准确，避免对动态代码产生大量误报。

## 9. 性能与兼容性验收

实现必须满足：

- 原库 `.scm` 文件无需因 hint 机制而修改。
- 不导入 hint 库时，原库加载和执行路径不变。
- Hint 库之间没有导入依赖，可以按任意顺序批量加载。
- 原过程的定义、调用、多返回值和尾调用行为不变。
- 类型检查只在显式运行开发工具时发生。
- 没有 hint 信息的现有库和文件仍可正常使用与检查。

## 10. 推荐实施顺序

1. 实现 hint 库命名、签名数据和标准查询接口。
2. 为一个小型库自身实现的导出过程手工编写完整 hint 接口及单元测试。
3. 让 `gf doc` 展示单个函数的类型签名。
4. 让 `gf check` 校验 hint 接口完整性和签名格式。
5. 实现 hint 接口骨架生成。
6. 实现文件级字面量参数检查和返回类型传播。
7. 支持 `define-record-type` 的记录签名生成、字段检查和谓词分支收窄。
8. 逐步支持局部绑定、多返回值、可变参数和复合类型。

每个阶段先增加可回归的单元测试，再实现对应功能。
