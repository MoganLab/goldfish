# (liii packrat combinator) 模块函数分类索引

`(liii packrat combinator)` 提供 Packrat 解析组合子（记号检查、解析后置、择一、分支排除）。

使用时导入：

```scheme
(import (liii packrat combinator))
```

## 常见用法示例

```scheme
(define parse-num
  (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc "函数名"
```

或者指定库名：

```bash
bin/gf doc liii/packrat/combinator "函数名"
```

## 函数分类索引

### 组合子
- `packrat-check-base` - 检查当前基础记号类型的组合子
- `packrat-check` - 对子解析结果做后置处理的组合子
- `packrat-or` - 择一解析的组合子
- `packrat-unless` - 排除某个分支的组合子
