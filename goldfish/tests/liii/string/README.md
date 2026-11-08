# (liii string) 模块函数分类索引

`string` 提供常见字符串查询、切片、变换、折叠和分词能力。
它补齐了 Goldfish 在文本处理上的高频接口，适合命令行、配置和轻量解析场景。

本模块导出的函数来源于：

- `(srfi srfi-13)`：`string-null?`、`string-join`、`string-every`、`string-any`、`string-take`、
  `string-take-right`、`string-drop`、`string-drop-right`、`string-pad`、`string-pad-right`、
  `string-trim`、`string-trim-right`、`string-trim-both`、`string-index`、`string-index-right`、
  `string-skip`、`string-skip-right`、`string-contains`、`string-count`、`string-fold`、
  `string-fold-right`、`string-for-each-index`、`string-reverse`、`string-tokenize`
- Liii 扩展：`string-trim-left`、`string-starts?`、`string-ends?`、`string-contains?`、
  `string-split`、`string-replace`、`string-remove-prefix`、`string-remove-suffix`、
  `string-position`、`char-position`、`pyfmt`

## 常见用法示例

```scheme
(import (liii string))

;; 示例1：切分字符串
(string-split "a,b,c" #\,) ; => ("a" "b" "c")

;; 示例2：替换子串
(string-replace "hello" "l" "L") ; => "heLLo"

;; 示例3：判断前后缀
(string-starts? "goldfish" "gold") ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/string "函数名"
```

相关库文档：

```bash
gf doc srfi/srfi-13   # 查看 SRFI-13 字符串库
gf doc scheme/base    # 查看 scheme base 库
```

## 函数分类索引

### 一、基本谓词与访问
- `string-null?` - 判断字符串是否为空
- `string-contains` / `string-contains?` - 判断是否包含子串
- `string-index` / `string-index-right` - 查找满足条件的位置

### 二、切片与边界处理
- `string-take` / `string-take-right` - 取前 n 个 / 后 n 个字符
- `string-drop` / `string-drop-right` - 跳过前 n 个 / 后 n 个字符
- `string-pad` / `string-pad-right` - 向左 / 向右补齐字符串
- `string-trim` / `string-trim-left` / `string-trim-right` / `string-trim-both` - 修整字符串
- `string-remove-prefix` / `string-remove-suffix` - 移除前缀 / 后缀

### 三、转换与替换
- `string-reverse` - 反转字符串
- `string-join` - 连接字符串列表
- `string-split` - 按分隔符切分字符串
- `string-replace` - 替换子串
- `string-tokenize` - 按分隔规则拆分为 token

### 四、迭代与归约
- `string-for-each-index` - 带索引遍历字符
- `string-fold` / `string-fold-right` - 左折叠 / 右折叠
- `string-any` / `string-every` - 判断是否存在 / 所有字符都满足条件

### 五、前后缀与匹配
- `string-starts?` - 判断是否以指定前缀开头（推荐）
- `string-ends?` - 判断是否以指定后缀结尾（推荐）
- `string-skip` / `string-skip-right` - 跳过前方 / 后方满足条件的字符
- `string-count` - 统计满足条件的字符个数

### 六、位置与排版
- `string-position` - 查找字符在字符串中的位置
- `char-position` - 查找字符在字符串中的位置
- `pyfmt` - Python 风格格式化

> 注意：`string-starts?` 和 `string-ends?` 比 `string-prefix?` 和 `string-suffix?` 更推荐使用，
> 因为参数顺序更直观：源字符串始终在前，模式在后。
>
> ```scheme
> (string-starts? "goldfish" "gold")  ; 直观：检查 "goldfish" 是否以 "gold" 开头
> (string-prefix? "gold" "goldfish")  ; 不直观：参数顺序相反
> ```
