# (srfi srfi-13) 模块函数分类索引

SRFI-13 是字符串处理库，提供了丰富的字符串操作函数，
包含字符串谓词、构造器、选择器、比较、裁剪、查找、删除、替换、过滤、折叠等功能。
本实现基于 SRFI-13 标准规范。

## 常见用法示例

```scheme
(import (srfi srfi-13))

;; 示例1：取子串
(string-take "MathAgape" 4) ; => "Math"

;; 示例2：连接字符串列表
(string-join '("a" "b" "c") "") ; => "abc"

;; 示例3：按分隔符切分
(string-tokenize "a,b,c" #\,) ; => ("a" "b" "c")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-13 "函数名"
```

## 函数分类索引

### 一、基本谓词与构造
- `string-null?` - 判断字符串是否为空
- `string-copy` - 复制字符串（或其部分）
- `string-join` - 连接字符串列表

### 二、字符串选择器（切片）
- `string-take` / `string-take-right` - 取前 n 个 / 后 n 个字符
- `string-drop` / `string-drop-right` - 跳过前 n 个 / 后 n 个字符

### 三、字符串填充与修整
- `string-pad` / `string-pad-right` - 向左 / 向右填充字符串到指定长度
- `string-trim` / `string-trim-right` / `string-trim-both` - 去掉左 / 右 / 两侧满足条件的字符

### 四、字符串前缀与后缀
- `string-prefix?` - 判断字符串是否以指定前缀开始
- `string-suffix?` - 判断字符串是否以指定后缀结束

### 五、字符串查找与搜索
- `string-index` / `string-index-right` - 查找满足条件的位置
- `string-skip` / `string-skip-right` - 跳过满足条件的字符
- `string-contains` - 判断字符串是否包含子串
- `string-count` - 统计满足条件的字符个数

### 六、字符串反转与分词
- `string-reverse` - 反转字符串
- `string-tokenize` - 按分隔符拆分字符串为列表

### 七、字符串折叠与迭代
- `string-fold` / `string-fold-right` - 左折叠 / 右折叠字符串
- `string-for-each-index` - 带索引遍历字符
- `string-any` / `string-every` - 判断是否存在 / 所有字符都满足条件
