# (liii bigint arithmetic) 模块函数分类索引

bigint arithmetic 提供 bigint 的基本算术运算，包括加、减、乘、除、取模、取余、绝对值、相反数与整数幂。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/bigint/arithmetic "函数名"
```

## 函数分类索引

### 一、基本算术
- `bigint-add` - 两数相加
- `bigint-subtract` - 两数相减
- `bigint-multiply` - 两数相乘
- `bigint-divide` - 整数商（向零取整）
- `bigint-quotient` - 整数商（与 bigint-divide 相同）
- `bigint-remainder` - 整数余数（向零取整）
- `bigint-modulo` - 模运算（商向下取整，余数符号与除数一致）
- `bigint-divide-and-remainder` - 同时返回商和余数

### 二、符号与绝对值
- `bigint-negate` - 取相反数
- `bigint-abs` - 取绝对值

### 三、幂运算
- `bigint-expt` - 整数幂运算
