# (srfi srfi-41) 模块函数分类索引

SRFI 41 提供 Scheme 的实用流（stream）库：惰性、记忆化的函数式数据结构，
天然表达无限序列与多趟数据处理。`goldfish/srfi/srfi-41.scm` 为完整移植，
`(liii streams)` 门面库全量重导出。

## 常见用法示例

```scheme
(import (srfi srfi-41))
;; 或等价的 (import (liii streams))

;; 示例1：无限算术流
(stream-ref (stream-from 0) 100)          ; => 100

;; 示例2：流推导式
(stream->list (stream-of (* x x)
              (x in (stream-range 1 4)))) ; => (1 4 9 16)

;; 示例3：安全过滤无限流
(stream->list 4 (stream-filter odd? (stream-from 0)))  ; => (1 3 5 7)

;; 示例4：用 define-stream 定义斐波那契流
(define-stream (fibs a b) (stream-cons a (fibs b (+ a b))))
(stream->list 8 (fibs 0 1))               ; => (0 1 1 2 3 5 8 13)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc srfi/srfi-41 "stream-map"
```

## 函数分类索引

### 一、流类型与构造子（primitive）
- `stream-null` - 空流常量
- `stream-cons` - 流版本的 cons，头尾均惰性
- `stream?` / `stream-null?` / `stream-pair?` - 类型谓词
- `stream-car` / `stream-cdr` - 访问头元素与尾流
- `stream-lambda` - 构造返回流的惰性过程
- `define-stream` - 定义流过程的语法糖
- `_` - `stream-match` 通配符字面量

### 二、流与列表/端口转换
- `list->stream` - 列表转流
- `port->stream` - 输入端口转字符流
- `stream` - 元素列表构造流
- `stream->list` - 流（前 n 个元素）转列表

### 三、流的组合与变换
- `stream-append` - 顺序拼接
- `stream-concat` - 流的流展平
- `stream-constant` - 无限循环元素
- `stream-map` - 多流同步映射
- `stream-filter` - 谓词过滤（可用于无限流）
- `stream-zip` - 多流按位置合并
- `stream-reverse` - 反转有限流

### 四、流的截取与丢弃
- `stream-take` / `stream-drop` - 按个数截取/丢弃
- `stream-take-while` / `stream-drop-while` - 按谓词截取/丢弃

### 五、折叠与遍历
- `stream-fold` - 从左折叠有限流
- `stream-for-each` - 多流同步遍历（副作用）
- `stream-length` - 有限流长度
- `stream-ref` - 按索引取元素

### 六、流的生成
- `stream-from` - 无限算术级数
- `stream-range` - 有限算术区间
- `stream-iterate` - 迭代函数序列
- `stream-scan` - 累积部分序列
- `stream-unfold` / `stream-unfolds` - 按种子展开（多流）

### 七、语法形式
- `stream-let` - 流的命名循环（迭代惰性算法关键形式）
- `stream-match` - 流的模式匹配
- `stream-of` - 流推导式（in / is / 谓词子句）
