# (liii json schema keywords-combo) 模块函数分类索引

`(liii json schema keywords-combo)` 实现 JSON Schema 的组合器
关键字评估：allOf、anyOf、oneOf、not 与 if/then/else，
是校验器评估流程的一部分。

## 常见用法示例

```scheme
(import (liii json schema))

;; 通过公开 API 间接使用组合器评估
(json-schema-valid? 1 '(("anyOf" (("type" . "integer")) (("type" . "string")))))
; => #t

(json-schema-valid? 1 '(("not" ("type" . "string"))))   ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/keywords-combo "函数名"
```

## 函数分类索引

### 一、组合器关键字
- `combo-keyword?` - 判断关键字是否为组合器关键字
- `validate-combo` - 评估组合器关键字，返回错误列表

## 错误处理约定

- oneOf 匹配数不是 1、anyOf 全部失败等产生 validation 错误记录
- 未知关键字不产生错误
