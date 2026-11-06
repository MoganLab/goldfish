# (liii json schema format-uri) 模块函数分类索引

`(liii json schema format-uri)` 实现 JSON Schema `format` 关键字中
`uri`、`uri-reference`、`json-pointer` 与 `relative-json-pointer`
四个格式的检查。

URI 部分遵循 RFC 3986，JSON Pointer 部分遵循 RFC 6901。

## 常见用法示例

```scheme
(import (liii json schema format-uri))

(uri? "https://example.com")             ; => #t
(uri-reference? "/relative")             ; => #t
(json-pointer? "/a~1b")                  ; => #t
(relative-json-pointer? "1/foo")         ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/format-uri "函数名"
```

## 函数分类索引

### 一、URI
- `uri?` - 判断字符串是否为绝对 URI
- `uri-reference?` - 判断字符串是否为 URI 引用

### 二、JSON Pointer
- `json-pointer?` - 判断字符串是否为 JSON Pointer
- `relative-json-pointer?` - 判断字符串是否为相对 JSON Pointer

## 错误处理约定

- 四个谓词都返回布尔值，不会抛出异常
- `uri?` 要求存在合法的 scheme
- JSON Pointer 中的 `~` 只能写成 `~0` 或 `~1`
