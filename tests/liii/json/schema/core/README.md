# (liii json schema core) 模块函数分类索引

`(liii json schema core)` 提供单节点求值入口：在组合关键字内部继续
下钻到子 Schema 时，用它携带当前的实例路径与 Schema 路径。

节点的遍历逻辑由 `(liii json schema evaluator)` 实现，
关键字语义由 `(liii json schema keywords)` 及其子模块实现。

## 常见用法示例

```scheme
(import (liii json schema core))

;; 判断单个节点是否合法
(json-schema-node-valid? validator 1 '(("type" . "integer")) #t)

;; 下钻到子 Schema 并收集错误
(json-schema-descend-errors validator "x" '(("type" . "integer"))
                             '("rows" 0) '("items") #t)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/core "函数名"
```

## 函数分类索引

### 一、节点求值
- `json-schema-node-valid?` - 以指定根资源校验单个节点，返回布尔结果
- `json-schema-descend-errors` - 以指定根资源校验单个节点，返回错误列表

## 错误处理约定

- 两个函数都不捕获校验过程中的异常
- `json-schema-descend-errors` 返回的错误列表由 `(liii json schema error)` 构造
