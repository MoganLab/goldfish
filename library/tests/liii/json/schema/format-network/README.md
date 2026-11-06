# (liii json schema format-network) 模块函数分类索引

`(liii json schema format-network)` 提供 JSON Schema format
关键字的网络类格式检查器：IPv4、IPv6 与 hostname，
供校验器在 format-assertion 词汇表下按字符串格式约束实例。

## 常见用法示例

```scheme
(import (liii json schema))

(json-schema-valid? "192.168.0.1"
  '(("type" . "string") ("format" . "ipv4"))
  '((formats ipv4)))
; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/format-network "函数名"
```

## 函数分类索引

### 一、网络格式检查器
- `ipv4-address?` - 判断是否为点分十进制 IPv4 地址
- `ipv6-address?` - 判断是否为 IPv6 地址（支持 :: 缩写）
- `hostname?` - 判断是否为合法主机名

## 错误处理约定

- 各检查器仅接受字符串输入，对非字符串输入行为未定义
