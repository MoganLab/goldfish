# (liii goldfix repair) 模块函数索引

`(liii goldfix repair)` 提供括号修正的核心能力：按可信缩进推断括号结构，生成缺失或多余的右括号修正结果与诊断报告。

使用时导入：

```scheme
(import (liii goldfix repair))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix/repair "函数名"
```

## 函数分类索引

### 括号修正
- `repair-parentheses` - 修正括号并返回修正后的源码与 repair-report 记录
- `fix-string` - 修正括号并只返回修正后的源码

### 括号检查
- `parentheses-balanced?` - 判断结构括号是否平衡
