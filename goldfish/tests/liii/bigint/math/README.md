# (liii bigint math) 模块函数分类索引

bigint math 提供 bigint 的高级数学运算，包括最大公约数、模运算、模幂、模逆、平方根与 n 次方根。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/bigint/math "函数名"
```

## 函数分类索引

### 一、数论运算
- `bigint-gcd` - 计算两个 bigint 的最大公约数
- `bigint-mod` - 模运算（结果始终非负）
- `bigint-mod-expt` - 模幂运算
- `bigint-mod-inverse` - 模逆元运算

### 二、开方运算
- `bigint-sqrt` - 整数平方根
- `bigint-sqrt-and-remainder` - 整数平方根及余数
- `bigint-rootn` - 整数 n 次方根
- `bigint-rootn-and-remainder` - 整数 n 次方根及余数
