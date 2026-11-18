# (liii decimal cast) 模块函数分类索引

decimal cast 负责 decimal 与 bigint、整数、浮点数、有理数之间的类型转换。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal/cast "函数名"
```

## 函数分类索引

### 一、bigint 转换
- `decimal-to-bigint` - 转换为 bigint，截断小数部分
- `decimal-to-bigint-exact` - 精确转换为 bigint，有小数部分则报错
- `bigint->decimal` - 从 bigint 构造 decimal

### 二、数值类型转换为 decimal
- `integer->decimal` - 从整数构造 decimal
- `long->decimal` - 从长整数构造 decimal
- `double->decimal` - 从双精度浮点数构造 decimal
- `rational->decimal` - 从有理数（分子/分母）构造 decimal

### 三、decimal 转换为浮点数
- `decimal->float` - 转换为单精度浮点数
