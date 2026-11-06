# (liii json schema cache) 模块函数分类索引

`(liii json schema cache)` 实现 JSON Schema 引用解析缓存：
按「引用 + 根资源 + 动态作用域」三元组命中，
避免重复解析同一引用，同时保证不同资源之间互不污染。

## 常见用法示例

```scheme
(import (liii json schema cache))

(define cache (make-reference-cache))
(reference-cache-put! cache "child.json" root '() 'VALUE)
(reference-cache-lookup cache "child.json" root '())   ; => 命中条目
(reference-cache-lookup cache "child.json" other '())  ; => #f（根资源不同）
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/cache "函数名"
```

## 函数分类索引

### 一、创建与判断
- `make-reference-cache` - 创建引用缓存

### 二、缓存操作
- `reference-cache-key-matches?` - 条目是否与引用/根资源/作用域匹配
- `reference-cache-lookup` - 查找缓存条目
- `reference-cache-put!` - 写入缓存条目

## 错误处理约定

- 缓存的值本身可能是 `#f`（假 Schema），需用条目本身判断是否命中
