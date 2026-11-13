# (liii decimal conversion) 模块函数分类索引

decimal conversion 负责 decimal 与字符串、数值类型、bigint 之间的转换。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal/conversion "函数名"
```

## 函数分类索引

### 一、构造函数
- `string->decimal` - 从字符串构造 decimal
- `bigint->decimal` - 从 bigint 构造 decimal
- `integer->decimal` - 从整数构造 decimal
- `double->decimal` - 从双精度浮点数构造 decimal
- `rational->decimal` - 从有理数构造 decimal
- `long->decimal` - 从长整数构造 decimal

### 二、数值转换
- `decimal->long` - 转换为长整数
- `decimal->long-exact` - 精确转换为长整数
- `decimal->int` - 转换为整数
- `decimal->int-exact` - 精确转换为整数
- `decimal->short` - 转换为短整数
- `decimal->short-exact` - 精确转换为短整数
- `decimal->byte` - 转换为字节
- `decimal->byte-exact` - 精确转换为字节
- `decimal->double` - 转换为双精度浮点数
- `decimal->float` - 转换为单精度浮点数
- `decimal->rational` - 转换为有理数
- `decimal->bigint` - 转换为 bigint
- `decimal->bigint-exact` - 精确转换为 bigint

### 三、字符串格式化
- `decimal->string` - 转换为字符串
- `decimal->plain-string` - 无指数表示
- `decimal->engineering-string` - 工程记数法
- `decimal->scientific-string` - 科学记数法
