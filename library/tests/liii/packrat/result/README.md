# (liii packrat result) 模块函数分类索引

`(liii packrat result)` 提供 Packrat 解析结果记录与解析结果流（惰性流与记忆化查找表）。

使用时导入：

```scheme
(import (liii packrat result))
```

## 常见用法示例

```scheme
(define r (make-result 42 #f))
(parse-result-successful? r)     ; => #t
(parse-result-semantic-value r)  ; => 42
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/packrat/result "函数名"
```

## 函数分类索引

### 一、解析结果记录
- `make-parse-result` - 构造解析结果记录
- `parse-result?` - 判断对象是否为解析结果记录
- `parse-result-successful?` - 读取解析结果是否成功
- `parse-result-semantic-value` - 读取解析结果的语义值
- `parse-result-next` - 读取解析结果的后续解析状态
- `parse-result-error` - 读取解析结果携带的错误
- `make-result` - 构造成功的解析结果记录
- `make-expected-result` - 构造「期望某记号」类型的失败结果
- `make-message-result` - 构造带错误消息的失败结果
- `merge-result-errors` - 把额外错误合并进解析结果
- `parse-error->parse-result` - 把解析错误转换为失败的解析结果

### 二、解析结果流
- `make-parse-results` - 构造解析结果流记录
- `parse-results?` - 判断对象是否为解析结果流记录
- `parse-results-position` - 读取解析结果流的位置
- `parse-results-next` - 读取解析结果流的后续状态
- `parse-results-token-kind` - 读取当前记号的类型
- `parse-results-token-value` - 读取当前记号的值
- `parse-results-base` - 读取当前基础记号
- `base-generator->results` - 把取词生成器转换为解析结果流
- `prepend-base` - 构造带基础记号的解析结果流
- `prepend-semantic-value` - 构造带记忆化语义值的解析结果流
- `results->result` - 按非终结符名查询或登记记忆化结果
