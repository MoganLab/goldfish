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

顶层入口文件只负责说明，不写实际断言。

约定如下：

- 文件名固定为 `<library>-test.scm`
- 文件内容保持为说明 stub
- 不写 `check`
- 不写 `check-report`
- 不在这里放具体函数测试

推荐样式：

```scheme
;;
;; Copyright (C) 2024-2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; (<namespace> <library>) 中相关的测试用例都在 tests/<group>/<library> 目录中
```

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
