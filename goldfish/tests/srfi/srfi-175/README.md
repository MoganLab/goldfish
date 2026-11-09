# (srfi srfi-175) 模块函数分类索引

`(srfi srfi-175)` 提供 ASCII 字符集的完整支持：类型判定、字符分类、大小写转换、
控制字符与图形字符互转，以及大小写无关的比较。
所有函数都接受字符、码点或字符串等多种输入形式，非 ASCII 输入按规范报错。

## 常见用法示例

```scheme
(import (srfi srfi-175))

;; 示例1：判断一个字符是否为 ASCII 字母
(ascii-alphabetic? #\a)

;; 示例2：把 ASCII 字符串转换为大写
(ascii-upcase "hello")

;; 示例3：进行大小写无关的字符串比较
(ascii-string-ci=? "Hello" "hello")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-175 "函数名"
```

## 函数分类索引

### 一、类型判定
- `ascii-codepoint?` - 判断是否为 ASCII 码点
- `ascii-bytevector?` - 判断是否为 ASCII 字节向量
- `ascii-char?` - 判断是否为 ASCII 字符
- `ascii-string?` - 判断是否为 ASCII 字符串

### 二、字符分类
- `ascii-control?` - 判断是否为控制字符
- `ascii-non-control?` - 判断是否为非控制字符
- `ascii-whitespace?` - 判断是否为空白字符
- `ascii-space-or-tab?` - 判断是否为空格或制表符
- `ascii-other-graphic?` - 判断是否为其他图形字符
- `ascii-upper-case?` - 判断是否为大写字母
- `ascii-lower-case?` - 判断是否为小写字母
- `ascii-alphabetic?` - 判断是否为字母
- `ascii-alphanumeric?` - 判断是否为字母或数字
- `ascii-numeric?` - 判断是否为数字

### 三、取值与转换
- `ascii-digit-value` - 返回数字字符对应的数值
- `ascii-upper-case-value` - 返回大写字母对应的数值
- `ascii-lower-case-value` - 返回小写字母对应的数值
- `ascii-nth-digit` - 返回从 0 开始计数的第 n 个数字字符
- `ascii-nth-upper-case` - 返回从 0 开始计数的第 n 个大写字母
- `ascii-nth-lower-case` - 返回从 0 开始计数的第 n 个小写字母
- `ascii-upcase` - 转换为大写
- `ascii-downcase` - 转换为小写
- `ascii-control->graphic` - 控制字符转换为图形字符
- `ascii-graphic->control` - 图形字符转换为控制字符
- `ascii-mirror-bracket` - 返回镜像括号字符

### 四、大小写无关比较
- `ascii-ci=?`、`ascii-ci<?`、`ascii-ci>?`、`ascii-ci<=?`、`ascii-ci>=?` - 字符比较
- `ascii-string-ci=?`、`ascii-string-ci<?`、`ascii-string-ci>?`、`ascii-string-ci<=?`、`ascii-string-ci>=?` - 字符串比较
