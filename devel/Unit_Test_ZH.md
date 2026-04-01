# 单元测试规范

本文档约定 `tests/` 目录下以后所有新增单元测试的目录放置、文件命名和文件内容风格。

## 适用范围

本规范适用于 `tests/` 下以后新增的所有单元测试，包括但不限于：

- `tests/scheme`
- `tests/liii`
- `tests/srfi`
- `tests/goldfish`

本规范不适用于测试资源文件，例如：

- `tests/resources`

`tests/resources` 中的文件属于测试夹具、输入样本或回归数据，不要求按 `-test.scm` 规则命名。

## 目录结构

新增某个库、模块或命名空间节点的单元测试时，统一使用“两层结构”：

```text
tests/<group>/<library>-test.scm
tests/<group>/<library>/<case>-test.scm
```

其中：

- `tests/<group>/<library>-test.scm` 是顶层入口说明文件
- `tests/<group>/<library>/` 目录下放实际的单元测试文件

例如：

```text
tests/scheme/eval-test.scm
tests/scheme/eval/environment-test.scm
tests/scheme/eval/eval-test.scm

tests/liii/hash-table-test.scm
tests/liii/hash-table/hash-table-ref-test.scm
tests/liii/hash-table/hash-table-update-bang-slash-default-test.scm
```

## 顶层入口文件写法

顶层入口文件不再只是一个纯注释 stub，而应当写成“模块说明 + 用法示例 + 函数分类索引”的入口文件。

可以把它理解为这个库在 `tests/` 目录下的“模块导览页”。

它的职责是：

- 用几句话说明这个库是什么、适合做什么
- 给出 1 到 3 个最常见、最能代表该库风格的用法示例
- 告诉读者如何用 `gf doc` 继续查看具体函数文档
- 用分类索引列出该库的主要导出函数，方便快速导航

它的职责不是：

- 承担具体函数的断言测试
- 代替 `tests/<group>/<library>/` 目录下的细粒度测试文件
- 把整个库的所有导出都在这里做一遍 `check`

约定如下：

- 文件名固定为 `<library>-test.scm`
- 文件主题是“模块说明入口”，而不是“单函数测试文件”
- 可以写 `import`
- 可以写少量示例代码
- 可以写少量辅助 `define`，用于支撑示例
- 可以写函数分类索引
- 不写 `check`
- 不写 `check-report`
- 不在这里放具体函数断言测试
- 不把多个独立函数的测试逻辑堆在这个文件里

换句话说，顶层入口文件允许“可执行示例”，但不允许“测试断言”。

### 建议结构

建议按下面这个顺序组织内容：

1. 文件开头用一行标题说明这个模块是什么
2. 用几行文字说明该模块的核心用途、与相近库的区别，或使用场景
3. 写 `==== 常见用法示例 ====`
4. `import` 被说明的库
5. 放 1 到 3 个短小、稳定、代表性强的示例
6. 写 `==== 如何查看函数的文档和用例 ====`
7. 给出一两条 `bin/gf doc ...` 示例命令
8. 写 `==== 函数分类索引 ====`
9. 按类别罗列该模块的主要函数

### 推荐样式

推荐直接参考 [tests/liii/range-test.scm](/home/wumo/work/work/goldfish/tests/liii/range-test.scm) 这种模式：

```scheme
;; (<namespace> <library>) 模块函数分类索引
;;
;; 一两段简短说明：
;; 这个库解决什么问题、有哪些特点、与相近库有什么差别。
;; 如果有特别典型的设计点，可以在这里点明。

;; ==== 常见用法示例 ====
(import (<namespace> <library>))

;; 示例1：最常见的基本用法
;; ...

;; 示例2：稍微体现模块特性的用法
;; ...

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc <namespace>/<library> "function-name"

;; ==== 函数分类索引 ====
;;
;; 一、某一类函数
;;   foo        - 简短说明
;;   foo?       - 简短说明
;;
;; 二、另一类函数
;;   bar        - 简短说明
;;   bar-baz    - 简短说明
```

### 参考实例说明

[tests/liii/range-test.scm](/home/wumo/work/work/goldfish/tests/liii/range-test.scm) 当前体现的模式是：

- 文件标题直接写成“模块函数分类索引”
- 开头先简要说明 `range` 的定位，以及它和 `iota` 这类相近能力的区别
- 接着给出几个最典型的示例，如遍历、折叠、切片
- 再给出 `gf doc` 的调用方式
- 最后按“构造函数、谓词函数、属性访问、元素访问、子范围操作、遍历操作、过滤操作、类型转换”等类别列出函数

这种写法的优点是：

- 新读者打开文件就能知道模块用途
- 不必先跳到子目录，先有一个总览
- 文档入口、用法示例和函数导航在一个地方集中呈现

### 示例代码该怎么把握尺度

顶层入口文件中的示例代码应当是“文档示例”，不是“测试断言”。

因此建议遵循这些原则：

- 示例数量少而精，通常 1 到 3 个就够
- 示例应当稳定、可重复、可读
- 示例尽量选择最常见的使用方式
- 可以用注释标出期望结果，例如 `; => 55`
- 可以有少量输出，但不要制造大量噪音
- 可以有少量辅助定义，例如 `define r`
- 避免依赖网络、随机数、当前时间、外部文件状态或复杂环境
- 避免长时间运行、死循环或重副作用逻辑

### 明确禁止的内容

虽然顶层入口文件现在允许写示例，但以下内容仍然不应该出现在这里：

- `check`
- `check-true`
- `check-false`
- `check-catch`
- `check-report`
- 为了测试某个具体函数而写的一整组断言
- 大量局部测试夹具和复杂辅助逻辑
- 将多个独立 API 的真实测试混在这个文件里

一旦代码的目的已经从“说明如何使用”变成“验证行为是否正确”，它就应该移到 `tests/<group>/<library>/` 下对应的单个测试文件中。

### 什么时候适合这样写

以下类型的库尤其适合这种顶层入口模式：

- 导出函数较多，需要按类别浏览的库
- 有明确使用风格或心智模型的库
- 容易让读者和相邻库混淆，需要先说明区别的库
- 有代表性工作流，适合用短示例展示的库

例如：

- `(liii range)`
- `(liii bag)`
- `(liii alist)`
- `(liii hash-table)`

### 和子目录中的单个测试文件怎么分工

可以把分工理解为：

- 顶层入口文件负责“告诉你这个库是什么、怎么开始用、有哪些函数”
- 子目录中的 `*-test.scm` 文件负责“验证某个具体函数或语法入口的行为是否正确”

二者不能互相替代。

如果某个库同时有“模块导览文件”和“同名过程测试文件”，这是完全允许的。例如：

- `tests/liii/bag-test.scm` 负责 `(liii bag)` 的总览、示例和分类索引
- `tests/liii/bag/bag-test.scm` 负责过程 `bag` 的具体断言测试

## 单个测试文件放哪

实际测试文件放在对应库目录下，一般一份文件测试一个导出过程、构造器、谓词或语法入口。

这里的默认规则是：

- 一个测试文件通常只测试一个对应函数
- 如果测试目标是构造器、谓词或语法入口，也同样按“一个文件对应一个目标”处理
- 不要把多个不直接相关的导出过程混写在同一个测试文件里

例如：

- `environment-test.scm` 只测试 `environment`
- `eval-test.scm` 只测试 `eval`
- `hash-table-ref-test.scm` 只测试 `hash-table-ref`

允许的例外情况只有两类：

- 某个测试目标依赖少量局部辅助函数、测试夹具或错误消息捕获函数
- 某个导出目标天然是成组入口，但文件仍然应围绕一个“主测试目标”展开

也就是说，辅助定义可以有，但测试断言应始终围绕文件名对应的那个目标，不要把它扩展成多个独立 API 的混合测试文件。

例如：

- `tests/scheme/char/char-ci-eq-p-test.scm`
- `tests/liii/base/plus-test.scm`
- `tests/liii/vector/vector-eq-test.scm`

如果某个库同时有“库入口说明文件”和“同名构造器/过程”的测试，这是允许的：

- `tests/liii/bag-test.scm` 是 `(liii bag)` 的入口说明
- `tests/liii/bag/bag-test.scm` 是过程 `bag` 的测试

## 单个测试文件怎么写

单个测试文件建议遵循以下结构：

1. 导入 `liii check` 与被测库
2. 需要时导入依赖库
3. 调用 `(check-set-mode! 'report-failed)`
4. 写函数说明块
5. 写实际 `check` / `check-true` / `check-false` / `check-catch`
6. 结尾调用 `(check-report)`

### 单个测试文件中的注释文档怎么写

单个测试文件中的注释文档，默认就是该文件对应测试目标的“函数说明块”。

约定如下：

- 注释文档写在导入语句和 `(check-set-mode! 'report-failed)` 之后
- 注释文档围绕文件名对应的测试目标来写
- 注释文档描述的是“被测函数 / 构造器 / 谓词 / 语法入口”
- 注释文档后面紧跟该目标的实际测试断言

推荐的注释顺序为：

1. 被测目标名称
2. 一句简短说明
3. `语法`
4. `参数`
5. `返回值`
6. `描述`、`注意` 或 `示例`
7. `错误处理`

推荐样式：

```scheme
;; function-name
;; 一句简短说明。
;;
;; 语法
;; ----
;; (function-name arg1 arg2 ...)
;;
;; 参数
;; ----
;; arg1 : type
;; 参数说明。
;;
;; 返回值
;; ----
;; return-type
;; 返回值说明。
;;
;; 描述
;; ----
;; 对行为做简短说明。
;;
;; 错误处理
;; ----
;; type-error
;; 当输入不合法时抛出。
```

补充约定：

- 注释块标题应直接写被测目标名
- 如果文件测试的是带符号名字的目标，标题仍写原始 Scheme 名字，例如 `char-ci=?`、`njson-set!`
- 注释文档应尽量简洁、可扫描，不写成长篇模块综述
- 如果某一项没有意义，可以省略

补充约定：

- 文件中的说明块应与文件名对应的测试目标一致
- 文件中的主要 `check` / `check-catch` 也应围绕同一个目标展开
- 如果发现一个文件需要系统性地测试第二个独立导出过程，应该拆成新的 `xxx-test.scm`

典型骨架：

```scheme
(import (liii check)
        (<namespace> <library>)
) ;import

(check-set-mode! 'report-failed)

;; function-name
;; 一句简短说明。
;;
;; 语法
;; ----
;; (function-name arg1 arg2 ...)
;;
;; 参数
;; ----
;; arg1 : type
;; 参数说明。
;;
;; 返回值
;; ----
;; return-type
;; 返回值说明。
;;
;; 描述
;; ----
;; 对行为做简短说明。
;;
;; 错误处理
;; ----
;; type-error
;; 当输入不合法时抛出。

(check ...)

(check-report)
```

## 文件命名规则

实际测试文件统一使用：

```text
<exported-name-normalized>-test.scm
```

其中 `normalized` 的转换规则如下。

### 普通标识符

- 普通字母、数字、连字符 `-` 保持原样

例如：

- `environment` -> `environment-test.scm`
- `digit-value` -> `digit-value-test.scm`

### 特殊符号转义

- `?` -> `-p`
- `!` -> `-bang`
- `/` -> `-slash-`
- `->` -> `-to-`
- `*` -> `-star`

例如：

- `char-upper-case?` -> `char-upper-case-p-test.scm`
- `njson-set!` -> `njson-set-bang-test.scm`
- `hash-table-update!/default` -> `hash-table-update-bang-slash-default-test.scm`
- `string->number` -> `string-to-number-test.scm`
- `trie-ref*` -> `trie-ref-star-test.scm`

### 运算符名称转义

纯运算符或比较符，按可读英文缩写转义：

- `+` -> `plus`
- `-` -> `minus`
- `*` -> `star`
- `/` -> `slash`
- `=` -> `eq`
- `<` -> `lt`
- `<=` -> `le`
- `>` -> `gt`
- `>=` -> `ge`

例如：

- `+` -> `plus-test.scm`
- `vector=` -> `vector-eq-test.scm`
- `isubset>=` -> `isubset-ge-test.scm`


## 新增测试时的检查清单

新增测试前请确认：

1. 测试是否放在了正确的 `tests/<group>` 目录下
2. 是否已经存在对应的顶层 `<library>-test.scm` 入口文件
3. 单个测试文件是否放在 `<library>/` 子目录下
4. 单个测试文件是否只围绕文件名对应的一个目标展开
5. 文件名是否按照导出名字做了统一转义
6. 顶层入口文件是否保持为说明 stub，而不是混入具体断言
7. 测试文件末尾是否调用了 `(check-report)`
