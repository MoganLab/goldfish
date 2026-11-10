# (liii json schema pointer) 模块函数分类索引

`(liii json schema pointer)` 实现 JSON Pointer 解析与 URI 片段处理：
按指针取出资源中的值，拆解 URI 的 fragment 与目录部分，
解码百分号转义序列。

这些函数被 `(liii json schema reference)` 与
`(liii json schema embedded)` 复用。

## 常见用法示例

```scheme
(import (liii json schema pointer))

(resolve-pointer '(("a" . ("b" . 1))) "/a/b")   ; => 1
(uri-fragment "https://example.com/s#frag")    ; => "frag"
(percent-decode "%E4%B8%AD")                   ; => "中"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/pointer "函数名"
```

## 函数分类索引

### 一、JSON Pointer
- `resolve-pointer` - 按 JSON Pointer 解析出根资源中的值

### 二、URI 片段
- `uri-fragment` - 取出 URI 中的 fragment 部分
- `uri-without-fragment` - 去掉 URI 中的 fragment 部分
- `uri-directory` - 取出 URI 的目录部分
- `uri-suffix?` - 判断 URI 是否以指定后缀结尾

### 三、转义
- `percent-decode` - 解码 URI 中的百分号转义序列

### 四、绝对性
- `absolute-uri?` - 判断字符串是否为绝对 URI

## 错误处理约定

- `resolve-pointer` 在指针非法、目标缺失或下标越界时抛出 `json-schema-unresolvable`
- `percent-decode` 在转义序列不完整或含非十六进制字符时抛出 `json-schema-unresolvable`
