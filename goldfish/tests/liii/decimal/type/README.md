# (liii decimal type) 模块函数分类索引

decimal type 负责 decimal 转换为各种数值类型：整数（long/int/short/byte）、浮点数与有理数。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal/type "函数名"
```

## 函数分类索引

### 一、转换为整数
- `decimal->long` - 转换为长整数（带舍入）
- `decimal->long-exact` - 精确转换为长整数
- `decimal->int` - 转换为整数（带舍入）
- `decimal->int-exact` - 精确转换为整数
- `decimal->short` - 转换为短整数（带舍入）
- `decimal->short-exact` - 精确转换为短整数
- `decimal->byte` - 转换为字节（带舍入）
- `decimal->byte-exact` - 精确转换为字节

### 二、转换为浮点数与有理数
- `decimal->double` - 转换为双精度浮点数
- `decimal->float` - 转换为单精度浮点数
- `decimal->rational` - 转换为有理数（分子/分母对）
