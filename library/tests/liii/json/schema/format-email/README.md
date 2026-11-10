# (liii json schema format-email) 模块函数分类索引

`(liii json schema format-email)` 实现 JSON Schema `format` 关键字中
`email` 与 `idn-email` 两个格式的检查。

`email-address?` 只接受 ASCII 地址，本地部分可以是点原子或带引号的字符串；
`idn-email-address?` 进一步允许非 ASCII 字符。

## 常见用法示例

```scheme
(import (liii json schema format-email))

(email-address? "user@example.com")      ; => #t
(email-address? "user@@example.com")     ; => #f
(idn-email-address? "用户@example.com")   ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/format-email "函数名"
```

## 函数分类索引

### 一、邮箱地址
- `email-address?` - 判断字符串是否为合法的电子邮件地址
- `idn-email-address?` - 判断字符串是否为合法的国际化电子邮件地址

## 错误处理约定

- 两个谓词都返回布尔值，不会抛出异常
- `email-address?` 拒绝包含非 ASCII 字符的地址
- `idn-email-address?` 拒绝以点结尾的本地部分或域名
