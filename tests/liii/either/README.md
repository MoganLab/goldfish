# (liii either) 模块函数分类索引

`(liii either)` 提供表达「成功值或错误值」二选一结果 Either 类型：
Right 表示成功结果，Left 表示失败或提前返回的信息，
适合错误处理、解析结果、可失败查找等需要显式传递失败语义的场景。

## 常见用法示例

```scheme
(import (liii either))

;; 构造与提取
(define ok (from-right 21))          ; Right 值（成功）
(define bad (from-left "network-error")) ; Left 值（失败）
(to-right ok)                        ; => 21
(to-left bad)                        ; => "network-error"

;; 只对 Right 生效的映射与遍历
(to-right (either-map (lambda (x) (* x 2)) ok))   ; => 42
(either-left? (either-map (lambda (x) (* x 2)) bad)) ; => #t（Left 原样返回）
(either-for-each display ok)                      ; 打印 21，无返回值

;; 失败时回退
(either-get-or-else bad 0)           ; => 0（Left 时取默认值）
(to-right (either-or-else bad (from-right 0)))    ; => 0（Left 时换用备用 Either）

;; 过滤与判断
(to-left (either-filter-or-else (lambda (x) (> x 100)) "too-small" ok)) ; => "too-small"
(either-contains? ok 21)             ; => #t
(either-every even? bad)             ; => #t（Left 视为真）
(either-any even? bad)               ; => #f（Left 视为假）
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/either "函数名"
```

## 函数分类索引

### 一、构造与提取
- `from-left` - 创建 Left 值（通常代表错误）
- `from-right` - 创建 Right 值（通常代表成功）
- `to-left` - 提取 Left 中的值
- `to-right` - 提取 Right 中的值

### 二、谓词
- `either?` - 判断对象是否为 Either（Left 或 Right）
- `either-left?` - 判断对象是否为 Left
- `either-right?` - 判断对象是否为 Right

### 三、高阶操作
- `either-map` - 仅对 Right 中的值应用函数
- `either-flat-map` - 对 Right 应用返回 Either 的函数（链式调用）
- `either-for-each` - 仅对 Right 中的值执行副作用
- `either-filter-or-else` - Right 不满足谓词时转换为 Left
- `either-contains?` - 判断 Right 中的值是否与目标相等
- `either-every` - Right 时应用谓词，Left 时返回 #t
- `either-any` - Right 时应用谓词，Left 时返回 #f

### 四、回退
- `either-get-or-else` - Left 时返回默认值
- `either-or-else` - Left 时返回备用的 Either

## 错误处理约定

- 参数不是 Either 时抛出 `type-error`
- `either-map` / `either-flat-map` / `either-for-each` /
  `either-filter-or-else` / `either-every` / `either-any` 的函数参数
  不是过程时抛出 `type-error`
- 对 Right 调用 `to-left`、对 Left 调用 `to-right` 时抛出 `value-error`
