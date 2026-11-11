# (liii packrat error) 模块函数分类索引

`(liii packrat error)` 提供 Packrat 解析过程中的错误记录与合并能力。

使用时导入：

```scheme
(import (liii packrat error))
```

## 常见用法示例

```scheme
(define pos (top-parse-position "test.txt"))
(define err (make-error-expected pos "identifier"))
(parse-error-position err) ; => pos
(parse-error-expected err) ; => '("identifier")
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/packrat/error "函数名"
```

## 函数分类索引

### 一、解析错误记录
- `parse-error?` - 判断对象是否为解析错误记录
- `parse-error-position` - 读取解析错误的位置
- `parse-error-expected` - 读取解析错误中期望的记号集合
- `parse-error-messages` - 读取解析错误中的消息列表

### 二、错误构造与合并
- `make-error-expected` - 构造「期望某记号」类型的解析错误
- `make-error-message` - 构造带错误消息的解析错误
- `parse-error-empty?` - 判断解析错误是否为空
- `merge-parse-errors` - 合并两个解析错误
