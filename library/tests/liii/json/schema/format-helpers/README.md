# (liii json schema format-helpers) 模块函数分类索引

`(liii json schema format-helpers)` 提供 JSON Schema `format` 检查所需的
底层字符与字符串工具：ASCII 字符分类、十六进制与十进制解析、字符串切分。

这些函数被 `format-uri`、`format-email`、`format-datetime` 等模块复用。

## 常见用法示例

```scheme
(import (liii json schema format-helpers))

(json-schema-ascii-digit? #\7)          ; => #t
(json-schema-split-on-char "a/b/c" #\/) ; => ("a" "b" "c")
(json-schema-decimal->number "0042")    ; => 42
(json-schema-hex-string? "dead")        ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/format-helpers "函数名"
```

## 函数分类索引

### 一、字符分类
- `json-schema-ascii-digit?` - 判断字符是否为 ASCII 数字
- `json-schema-ascii-letter?` - 判断字符是否为 ASCII 字母
- `json-schema-ascii-alnum?` - 判断字符是否为 ASCII 字母或数字
- `json-schema-hex-digit?` - 判断字符是否为十六进制数字
- `json-schema-all-ascii?` - 判断字符串是否只包含 ASCII 字符

### 二、字符串工具
- `json-schema-split-on-char` - 按指定字符把字符串切分成子串列表

### 三、数字解析
- `json-schema-decimal-string?` - 判断字符串是否只包含十进制数字
- `json-schema-decimal->number` - 把十进制数字串转换为整数
- `json-schema-hex-string?` - 判断字符串是否为十六进制数字串

## 错误处理约定

- 字符分类谓词只覆盖 ASCII 范围，不接受其他 Unicode 字符
- `json-schema-decimal->number` 不做输入校验，非法字符由调用方先行拦截
- `json-schema-hex-string?` 限制长度不超过 4
