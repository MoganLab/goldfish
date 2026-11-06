# (liii json schema evaluator) 模块函数分类索引

`(liii json schema evaluator)` 是 JSON Schema 校验的核心评估器：
对单个 Schema 节点执行关键字评估，并通过资源预算记录
统计单次校验的深度、步数与引用次数。

## 常见用法示例

```scheme
(import (liii json schema)
        (liii json schema evaluator))

(define validator (make-json-schema-validator '(("type" . "integer"))
                    '((check-schema? . #f))))

;; 直接评估根节点，返回错误列表
(validate-node validator 1 '(("type" . "integer")) '() '()
  '(("type" . "integer")))   ; => ()

;; 读取校验器自动创建的预算记录
(validator-budget validator)  ; => resource-budget 记录
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/evaluator "函数名"
```

## 函数分类索引

### 一、节点评估
- `validate-node` - 对单个 Schema 节点执行关键字评估，返回错误列表

### 二、资源预算
- `validator-budget` - 从校验器选项中读取资源预算记录

## 错误处理约定

- Schema 既不是对象也不是布尔值时抛出 `json-schema-schema-error`
- 超出预算限制时抛出预算超限错误
