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
| `(scheme base)` | `(hint scheme base)` | `hint-scheme-base` |
| `(scheme write)` | `(hint scheme write)` | `hint-scheme-write` |
| `(liii list)` | `(hint liii list)` | `hint-liii-list` |
| `(liii xyz)` | `(hint liii xyz)` | `hint-liii-xyz` |

查询函数名由原库各部分的名称用 `-` 连接，并添加 `hint-` 前缀。

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

每个 hint 库只导出该原库专属的标准查询函数，不重新导出原库函数。

以 `(hint liii xyz)` 为例：

```scheme
(define-library (hint liii xyz)
  (import (scheme base))
  (export hint-liii-xyz)
  (begin
    (define %hints
      '((buzz (string? -> integer?))
        (fizz (integer? integer? -> string?))))

    (define (hint-liii-xyz name)
      (let ((entry (assq name %hints)))
        (if entry (cadr entry) #f)))))
```

### 5.1 查询单个函数

```scheme
(import (hint liii xyz))

(hint-liii-xyz 'buzz)
;; => (string? -> integer?)

(hint-liii-xyz 'not-exported)
;; => #f
```

查询函数接受一个符号：

- 若符号对应原库中已声明类型的导出过程，返回其签名数据。
- 若符号没有类型声明，或不属于该接口覆盖范围，返回 `#f`。
- 查询函数不调用被查询过程，不执行类型检查，也不修改任何过程。

### 5.2 查询范围

`(hint liii xyz)` 只描述 `(liii xyz)` 导出的过程。

一个完整的 hint 库应覆盖原库导出的所有过程。语法绑定和非过程常量不属于首版
函数类型接口的覆盖范围。

Hint 库内部使用普通 Scheme 数据保存签名。签名数据的具体内部表示不改变公开
查询接口。

## 6. 类型签名格式

### 6.1 固定参数和单返回值

完整签名由参数类型、`->` 和返回类型组成：

```scheme
(string? integer? -> string?)
```

例如：

```scheme
(hint-liii-xyz 'buzz)
;; => (string? -> integer?)
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

### 6.2 类型名称

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

### 6.3 多返回值和复杂类型

后续版本需要支持：

- 多返回值。
- 可选参数和 rest 参数。
- 高阶函数。
- 参数化类型，例如列表元素类型。
- 联合类型和更精确的返回值约束。

这些能力不得破坏固定参数签名的基本格式。

## 7. `gf doc` 集成

`gf doc` 查询函数文档时，根据原库名找到对应 hint 库，并调用其标准查询函数。

例如查询 `(liii xyz)` 中的 `buzz`：

```bash
bin/gf doc liii/xyz "buzz"
```

若存在类型信息，输出中包含：

```text
;; 类型: (string? -> integer?)
```

若原库没有 hint 库，或查询函数返回 `#f`，则保持现有文档输出，不将缺少类型
信息视为文档查询错误。

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
- 为可识别的导出过程生成签名条目。
- 尽可能保留参数数量信息。
- 对无法确定的类型使用 `any?` 占位。

生成器不承诺自动推导准确的语义类型。开发者需要审查并收紧生成结果。

### 8.2 校验类型接口

接口校验包括：

- 原库导出的过程是否具有类型声明。
- 是否存在重复声明。
- 是否声明了不属于原库导出范围的过程。
- 签名语法是否合法。
- 固定参数数量是否与原过程参数列表一致。
- 标准查询函数的名称和导出是否符合约定。

新增原库导出过程后，遗漏对应 hint 信息应能被检查发现。

### 8.3 批量建立类型环境

检查源码文件时，`gf check` 可以读取文件的 `import`，将原库名映射为 hint
库名，并通过标准查询函数获取所需签名。

例如：

```scheme
(import (liii xyz))
```

对应加载：

```scheme
(import (hint liii xyz))
```

随后通过以下查询获得类型信息：

```scheme
(hint-liii-xyz 'buzz)
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

例如，已知：

```scheme
(hint-liii-xyz 'buzz)
;; => (string? -> integer?)
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

### 8.5 检查边界

`gf check` 不执行被检查的业务源码。Hint 库只提供类型数据查询，静态分析、
类型推导和诊断均由工具侧完成。

首版不追求覆盖 Scheme 的全部动态行为，也不以完整、可靠的类型推导为目标。
优先保证确定冲突的诊断准确，避免对动态代码产生大量误报。

## 9. 性能与兼容性验收

实现必须满足：

- 原库 `.scm` 文件无需因 hint 机制而修改。
- 不导入 hint 库时，原库加载和执行路径不变。
- 原过程的定义、调用、多返回值和尾调用行为不变。
- 类型检查只在显式运行开发工具时发生。
- 没有 hint 信息的现有库和文件仍可正常使用与检查。

## 10. 推荐实施顺序

1. 实现 hint 库命名、签名数据和标准查询接口。
2. 为一个小型库手工编写完整 hint 接口及单元测试。
3. 让 `gf doc` 展示单个函数的类型签名。
4. 让 `gf check` 校验 hint 接口完整性和签名格式。
5. 实现 hint 接口骨架生成。
6. 实现文件级字面量参数检查和返回类型传播。
7. 逐步支持局部绑定、多返回值、可变参数和复合类型。

每个阶段先增加可回归的单元测试，再实现对应功能。
