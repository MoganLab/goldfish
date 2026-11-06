# (scheme char) 模块函数分类索引

R7RS `scheme char` 提供字符的大小写转换、字符类别判断以及大小写无关的字符/字符串比较能力，
适合词法分析、文本规范化和基于字符属性的处理逻辑。

## 常见用法示例

```scheme
(import (scheme char))

;; 字符大小写转换
(char-upcase #\a)      ; => #\A
(char-downcase #\Z)    ; => #\z

;; 字符类别判断
(char-alphabetic? #\A) ; => #t
(char-whitespace? #\space) ; => #t

;; 大小写无关比较和数字值转换
(char-ci=? #\a #\A)    ; => #t
(digit-value #\7)     ; => 7
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc scheme/char "函数名"
```

## 函数分类索引

### 一、字符大小写转换 (R7RS 6.3 Character case)
- `char-upcase` - 将字符转换为大写
- `char-downcase` - 将字符转换为小写
- `char-foldcase` - 将字符转换为大小写无关的折叠形式

### 二、字符类别判定 (R7RS 6.3 Character case)
- `char-upper-case?` - 判断是否为大写字符
- `char-lower-case?` - 判断是否为小写字符
- `char-numeric?` - 判断是否为数字字符
- `char-alphabetic?` - 判断是否为字母字符
- `char-whitespace?` - 判断是否为空白字符

### 三、字符大小写无关比较 (R7RS 6.3 Character case)
- `char-ci=?` - 判断两个字符是否大小写无关相等
- `char-ci<?` - 判断两个字符是否大小写无关小于
- `char-ci>?` - 判断两个字符是否大小写无关大于
- `char-ci<=?` - 判断两个字符是否大小写无关小于等于
- `char-ci>=?` - 判断两个字符是否大小写无关大于等于

### 四、数字转换函数
- `digit-value` - 获取数字字符对应的数值

### 五、字符串大小写无关比较
- `string-ci=?` - 判断两个字符串是否大小写无关相等
- `string-ci<?` - 判断两个字符串是否大小写无关小于
- `string-ci>?` - 判断两个字符串是否大小写无关大于
- `string-ci<=?` - 判断两个字符串是否大小写无关小于等于
- `string-ci>=?` - 判断两个字符串是否大小写无关大于等于

### 六、字符串大小写转换
- `string-upcase` - 将字符串转换为大写
- `string-downcase` - 将字符串转换为小写
- `string-foldcase` - 将字符串转换为大小写无关的折叠形式
