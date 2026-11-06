# (liii json schema equal) 模块函数分类索引

`(liii json schema equal)` 实现 JSON 语义下的相等判定，
供 `const`、`enum` 与 `uniqueItems` 等关键字复用。

相等判定遵循 JSON 而非 Scheme：数字按数值比较，
布尔与数字严格区分，对象按无序映射比较，数组按有序序列比较。

## 常见用法示例

```scheme
(import (liii json schema equal))

(json-equal? 1 1.0)                                          ; => #t
(json-equal? #t 1)                                          ; => #f
(json-equal? '(("a" . 1) ("b" . 2)) '(("b" . 2) ("a" . 1)))  ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/equal "函数名"
```

## 函数分类索引

### 一、相等判定
- `json-equal?` - 按 JSON 语义比较两个值是否相等

## 错误处理约定

- `json-equal?` 不抛出异常，任何值都可以比较
- 布尔与数字严格区分，`#t` 与 `1` 不相等
- 对象按无序映射比较，键的顺序不影响结果
