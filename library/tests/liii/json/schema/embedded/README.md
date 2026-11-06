# (liii json schema embedded) 模块函数分类索引

`(liii json schema embedded)` 实现 JSON Schema 资源定位：
把相对引用解析成绝对 URI，查找内嵌资源、普通锚点与动态锚点。

查找只沿着子 Schema 位置的关键字向下进行，
名称到 Schema 的映射关键字按每个值单独查找。

## 常见用法示例

```scheme
(import (liii json schema embedded))

(resolve-resource-name "meta/core" '(("$id" . "https://example.com/root.json")))
   ; => "https://example.com/meta/core"
(anchor-resource document "x")   ; => 带 $anchor 的 Schema
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/embedded "函数名"
```

## 函数分类索引

### 一、引用解析
- `resolve-resource-name` - 把相对引用解析成绝对 URI
- `absolute-resource` - 计算带绝对 $id 的资源

### 二、资源查找
- `embedded-resource` - 在根资源中查找内嵌的资源
- `anchor-resource` - 在根资源中查找带 $anchor 的 Schema
- `dynamic-anchor-resource` - 在根资源中查找带 $dynamicAnchor 的 Schema
- `walk-dynamic-anchor` - 在 Schema 树中查找带 $dynamicAnchor 的节点

### 三、辅助
- `uri-suffix?` - 判断 URI 是否以指定后缀结尾

## 错误处理约定

- `anchor-resource` 与 `dynamic-anchor-resource` 找不到锚点时抛出 `json-schema-unresolvable`
- `embedded-resource` 找不到资源时返回 `#f`
