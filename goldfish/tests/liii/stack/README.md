# (liii stack) 模块函数分类索引

`(liii stack)` 提供后进先出（LIFO）的栈数据结构，
支持构造、访问、原地修改、函数式映射与列表互转等操作，
适合表达式求值、回溯算法、括号匹配等需要栈语义的场景。

## 常见用法示例

```scheme
(import (liii stack))

;; 构造与基本操作
(define s (make-stack))        ; => 空栈
(stack-push! s 1)              ; 栈顶: 1
(stack-push! s 2)              ; 栈顶: 2
(stack-top s)                  ; => 2
(stack-pop! s)                 ; => 2，栈顶回到 1
(stack-size s)                 ; => 1

;; 从列表创建（列表第一个元素成为栈顶）
(define s2 (stack 1 2 3))
(stack->list s2)               ; => (1 2 3)
(list->stack '(a b))           ; 栈顶为 a

;; 映射与遍历
(stack-map (lambda (x) (* x 2)) s2)  ; 返回新栈 (2 4 6)
(stack-for-each display s2)          ; 遍历副作用，无返回值
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/stack "函数名"
```

## 函数分类索引

### 一、构造函数
- `make-stack` - 创建空栈或从列表创建栈
- `stack` - 从任意数量参数创建栈（变参构造）
- `list->stack` - 从列表创建栈
- `stack-copy` - 复制栈

### 二、谓词
- `stack?` - 判断对象是否为栈
- `stack-empty?` - 判断栈是否为空

### 三、访问器
- `stack-top` - 获取栈顶元素（不弹出）
- `stack-size` - 获取栈中元素个数（O(1)）
- `stack->list` - 将栈转换为列表（从栈顶到栈底）

### 四、修改器
- `stack-push!` - 将元素压入栈顶
- `stack-pop!` - 从栈顶弹出元素并返回

### 五、映射与遍历
- `stack-map` - 对每个元素应用函数，返回新栈
- `stack-map!` - 对每个元素应用函数，原地修改
- `stack-for-each` - 遍历栈中每个元素，执行副作用

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 对空栈调用 `stack-top` / `stack-pop!` 时抛出 `value-error`
