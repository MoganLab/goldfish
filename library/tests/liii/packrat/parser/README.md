# (liii packrat parser) 模块函数分类索引

`(liii packrat parser)` 提供定义带记忆化的 Packrat 规则的语法宏 `packrat-parser`。

使用时导入：

```scheme
(import (liii packrat parser))
```

## 常见用法示例

```scheme
(define simple-parser
  (packrat-parser expr
    (expr ((a <- 'num) a)
          ((a <- 'id) a))))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/packrat/parser "函数名"
```

## 函数分类索引

### 宏
- `packrat-parser` - 定义 packrat 解析规则
