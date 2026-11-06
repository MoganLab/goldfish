# (liii json schema meta) 模块函数分类索引

`(liii json schema meta)` 内嵌 JSON Schema 2020-12 元 Schema
及其各词汇表资源，使 `check-schema` 可以在完全离线的环境中
校验 Schema 本身。

## 常见用法示例

```scheme
(import (liii json schema meta))

;; 内嵌资源包括主元 Schema 与 core、applicator、validation、
;; unevaluated、format-annotation、format-assertion、content、
;; meta-data 八个词汇表
(length (json-schema-meta-uris))   ; => 9

(json-schema-meta-resource "https://json-schema.org/draft/2020-12/schema")
; => 主元 Schema 资源
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/meta "函数名"
```

## 函数分类索引

### 一、内嵌元资源
- `json-schema-meta-uris` - 内嵌元 Schema 资源的 URI 列表
- `json-schema-meta-resource` - 按 URI 读取内嵌元 Schema 资源

## 错误处理约定

- URI 不在内嵌资源列表中时抛出值错误
