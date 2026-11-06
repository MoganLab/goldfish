# (liii json schema error) 模块函数分类索引

`(liii json schema error)` 定义 JSON Schema 校验错误对象 `<json-schema-error>`，
以及创建错误与读取错误各字段的访问器。

错误对象由校验入口产生，也可手工构造用于测试或自定义报告。

## 常见用法示例

```scheme
(import (liii json schema error))

;; 从校验入口取得错误对象并读取字段
(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(json-schema-error? err)        ; => #t
(json-schema-error-kind err)     ; => validation
(json-schema-error-keyword err)  ; => "type"

;; 手工构造错误记录
(define custom (validation-error "msg" "type" "string" 1 #t '() '() '() #f))
(json-schema-error-kind custom)  ; => validation
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/error "函数名"
```

## 函数分类索引

### 一、谓词与构造器
- `json-schema-error?` - 判断是否为错误对象
- `make-validation-error` - 显式构造错误记录
- `validation-error` - 构造类别为 validation 的错误
- `value->display` - 将任意值转为 write 风格的显示字符串

### 二、字段访问器
- `json-schema-error-kind` - 错误类别（validation / schema）
- `json-schema-error-message` - 人类可读消息
- `json-schema-error-keyword` - 触发错误的关键字
- `json-schema-error-keyword-value` - 关键字的取值
- `json-schema-error-instance` - 触发位置的实例
- `json-schema-error-schema` - 触发位置的 Schema
- `json-schema-error-path` - 实例路径（倒序段列表）
- `json-schema-error-schema-path` - Schema 路径（倒序段列表）
- `json-schema-error-context` - 组合关键字子错误
- `json-schema-error-cause` - 底层原因

### 三、路径显示
- `json-schema-error-absolute-path` - 从根开始的完整实例路径
- `json-schema-error-absolute-schema-path` - 从根开始的完整 Schema 路径
- `json-schema-error-json-path` - JSONPath 风格显示路径

## 错误处理约定

- `validation-error` 会把关键字追加到 Schema 路径末尾
- `value->display` 使用 write 风格输出，字符串带引号
- 错误对象是不可变的 record
