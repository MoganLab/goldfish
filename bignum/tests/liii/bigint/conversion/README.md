# (liii bigint conversion) 模块函数分类索引

bigint conversion 负责 bigint 与字符串、整数、字节向量之间的转换。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/bigint/conversion "函数名"
```

## 函数分类索引

### 一、构造函数
- `string->bigint` - 从十进制字符串构造 bigint
- `integer->bigint` - 从普通整数构造 bigint
- `bytes->bigint` - 从二进制补码字节向量构造 bigint

### 二、格式化输出
- `bigint->string` - 转换为十进制字符串
- `bigint->bytes` - 转换为二进制补码字节向量
