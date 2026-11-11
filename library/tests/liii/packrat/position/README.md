# (liii packrat position) 模块函数分类索引

`(liii packrat position)` 提供 Packrat 解析过程中的位置跟踪与比较能力。

使用时导入：

```scheme
(import (liii packrat position))
```

## 常见用法示例

```scheme
(define pos (top-parse-position "test.txt"))
(parse-position-line pos)    ; => 1
(parse-position-column pos)  ; => 0
(define next-pos (update-parse-position pos #\a))
(parse-position->string next-pos) ; => "test.txt:1:1"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/packrat/position "函数名"
```

## 函数分类索引

### 一、解析位置记录
- `make-parse-position` - 构造解析位置记录
- `parse-position?` - 判断对象是否为解析位置记录
- `parse-position-file` - 读取解析位置的文件名
- `parse-position-line` - 读取解析位置的行号
- `parse-position-column` - 读取解析位置的列号

### 二、位置更新与运算
- `top-parse-position` - 构造文件起始位置
- `update-parse-position` - 按读入的字符推进解析位置
- `parse-position->string` - 把解析位置格式化为字符串
- `parse-position>?` - 比较两个解析位置的先后
