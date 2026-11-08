# (liii lang) 模块函数分类索引

`(liii lang)` 是纯语法糖库：`$` 为原生 `syntax-rules` 宏，把 Scala 式链式调用
在**宏展开期**降解为 `(liii list)` / `(liii vector)` 原生函数的嵌套调用，
零装箱、零闭包对象、零方法分发。

使用时导入：

```scheme
(import (liii lang))
```

## 核心宏 `$`

### 去糖语义

```scheme
($ xs)                     =>  xs                     ; 返回裸值，不装箱
($ xs :map f)              =>  (map f xs)
($ xs :filter p :map f)    =>  (map f (filter p xs))  ; 从右往左嵌套
($ xs :map (* _ 2) :sum)   =>  (%$sum (map (lambda (_) (* _ 2)) xs))
($ xs :take 3 :drop 1)     =>  (list-drop (list-take xs 3) 1)
```

### 规则

1. `($ x)` 返回 `x` 本身（不装箱）；
2. 白名单算子从左到右逐个去糖，前算子结果作为后算子输入；
3. 谓词位含 `_` 自动提升为单参 lambda（见下节）；
4. 链尾天然是裸值，**无 `:collect`**；
5. 未命中白名单的算子触发人性化报错（列出全部支持的算子）。

## `_` 占位符自动提升

```scheme
($ '(1 2 3) :map (+ _ 2))                    => (3 4 5)   ; List(1,2,3).map(_ + 2)
($ '(1 2 3) :map (- 10 _))                   => (9 8 7)   ; _ 在第 2 位
($ '(1 2 3) :map (+ (* _ 2) 1))              => (3 5 7)   ; 深层嵌套
($ '(1 2 3) :map (max 7 _ 1))                => (7 7 7)   ; _ 在中间
($ '(1 2 3) :map _)                          => (1 2 3)   ; 恒等
($ '(1 2 3 4) :filter odd?)                  => (1 3)     ; 无 _ 原样传递
($ '(1 2 3) :map (if (odd? _) 'o 'e))        => (o e o)   ; if 表达式
```

- `_` 是单占位符语义（等价 Scala 单参 `_`）；双占位 `_ + _` 需显式
  `(lambda (a b) (+ a b))`；
- 扁平覆盖 `_` 在第 1~5 实参位；深层覆盖含 `_` 子表达式在第 1~3 位
  （子表达式内 `_` 也在第 1~3 位）；
- 超出枚举位的形态报 `unbound variable _`，请改用显式 lambda。

## 算子白名单（list / vector 双路径）

| 类别 | 算子 | list 路径 | vector 路径 | string 路径 (UTF-8) |
|---|---|---|---|---|
| 变换 | `:map` | `map` | `vector-map` | UTF-8 字符变换 |
| 扁平化 | `:flat-map` | 展平一层 | 展平一层 | 字符拼接展平 |
| 过滤 | `:filter` | `filter` | `vector-filter` | 字符过滤 |
| 遍历 | `:for-each` | `for-each`（可链） | `vector-for-each` | `string-for-each-cursor` |
| 前后子集 | `:take` / `:drop` | 宽容版 | 宽容版 | 宽容 clamp |
| 尾部子集 | `:take-right` / `:drop-right` | 宽容版 | 宽容版 | 宽容 clamp |
| 条件子集 | `:take-while` / `:drop-while` | 满足前缀 | 满足前缀 | 满足前缀 |
| 元素二分 | `:split-at` | `(take n, drop n)` | 同构 | 同构 |
| 切片截取 | `:slice` | `[from, until)` clamp | 同构 | 字符序 clamp |
| 去掉首位 | `:tail` | 空集报错 | 空集报错 | 空集报错 |
| 首末元素 | `:head` / `:last` | 空集报错 | 空集报错 | 空集报错 |
| 安全首末 | `:head-option` / `:last-option` | 空集 (none) | 空集 (none) | 空集 (none) |
| 判空 | `:empty?` / `:not-empty?` | `#t`/`#f` | 同构 | 同构 |
| 归约求和 | `:sum` | 累加（空集 0） | 累加 | 不支持报错 |
| 种子归约 | `:reduce` / `:reduce-option` | 左结合 / (none) | 同构 | 同构 |
| 左折叠 | `:fold-left` (`:fold`) | 左结合 (acc elem) | 同构 | (acc char) |
| 计数与长 | `:count` / `:length` | 满足个数 / 长度 | 满足个数 / 长度 | 字符计数 / 字符数 |
| 反转 | `:reverse` | `reverse` | `vector-reverse-copy` | UTF-8 字符反转 |
| 元素去重 | `:distinct` | 保留首现顺序 | 保留首现顺序 | 字符去重 |
| 查找元素 | `:find` | 返回 `(option v)` | `(option v)` | `(option c)` |
| 下标查找 | `:index-of` | 下标（未命中 -1） | 同构 | 字符序（未命中 -1） |
| 配对组合 | `:zip` | 短侧对齐，元组链表 | 元组向量 | 字符交错拼接 |
| 下标配对 | `:zip-with-index` | `((elem idx) ...)` | `#(#(elem idx) ...)` | `((char idx) ...)` |
| 条件拆分 | `:partition` | `(满足 不满足)` | 同构 | 同构 |
| 前后匹配 | `:starts-with?` / `:ends-with?` | 序列等长比对 | 同构 | `string-prefix?/suffix?` |
| 索引区间 | `:indices` | `(0 1 ... len-1)` | `#(0 1 ... len-1)` | `(0 1 ... len-1)` |
| 拼接转串 | `:mk-string` | 0/1/3 参拼接 | 0/1/3 参拼接 | — |
| 多核并行 | `:par-map` / `:par-filter` / `:par-for-each` | CSP 多核加速 | 同构 | — |
| Option管线 | `:map` / `:filter` / `:flat-map` / `:get-or-else` / `:or-else` / `:exists` / `:forall` / `:contains` | — | — | — |

> `:take`/`:drop` 采用 `(liii list)` / `(liii vector)` 的宽容版
> （越界/负数不抛错），与 Scala 语义逐项一致（Scala Oracle 验证）：
> `take(10)` => 全部、`take(-1)` => 空、`drop(10)` => 空、`drop(-1)` => 全部。

## 明确排除（by design）

- `:collect` —— 链尾已是裸值，无需拆箱（调用会得到未命中报错）；
- `@` 位置参数部分应用 —— 属于 `(mogan oop)`，不在本库范围；
- `for/yield`、`try` —— 不做（见 spec.md）。

## 文档查询

`bin/gf doc` 可以查看各算子的文档与测试示例（文档来源于测试文件头部注释）：

```bash
bin/gf doc liii/lang                    # 查看库概览与算子白名单
bin/gf doc liii/lang '$'                # 查看核心宏 $ 的文档（$ 需单引号防 shell 展开）
bin/gf doc liii/lang 'list%head'        # 查看指定算子的文档
bin/gf doc 'string%take'                # 在所有库中模糊查找算子
```

注意：`$` 在 shell 中需用单引号包裹（`'$'`）防止变量展开。

## 测试组织

本目录一算子一文件：`list%map-test.scm`、`vector%take-test.scm` 等；
横切断言（卫生性、未命中报错、链式组合）在 `lang-test.scm`。

```bash
gf test tests/liii/lang/            # 全部
gf test tests/liii/lang/list%map    # 单算子
```
