# (liii json schema dialect) 模块函数分类索引

`(liii json schema dialect)` 负责 JSON Schema 方言识别与关键字表：
确认 Schema 使用的草稿版本，并判断关键字的位置类别。

目前只支持 2020-12 方言，其他草稿声明会报 `json-schema-unknown-dialect`。

## 常见用法示例

```scheme
(import (liii json schema dialect))

(draft-for '(("type" . "integer")))   ; => draft2020-12
(known-keyword? "type")                ; => 真值
(subschema-location? "properties")     ; => 真值
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/dialect "函数名"
```

## 函数分类索引

### 一、方言识别
- `draft-for` - 返回 Schema 使用的 JSON Schema 方言

### 二、关键字表
- `known-keyword?` - 判断关键字是否为 2020-12 的已知关键字
- `subschema-location?` - 判断关键字的值是否存放子 Schema
- `schema-map-keyword?` - 判断关键字的值是否是名称到 Schema 的映射
- `unsupported-modern-keyword!` - 检查尚未支持的新版关键字

## 错误处理约定

- `$schema` 声明了非 2020-12 草稿时抛出 `json-schema-unknown-dialect`
- `$schema` 不是字符串时抛出 `json-schema-schema-error`
- `known-keyword?` 等谓词返回 `member` 的结果，真值但不严格等于 `#t`
