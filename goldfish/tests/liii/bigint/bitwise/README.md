# (liii bigint bitwise) 模块函数分类索引

bigint bitwise 提供 bigint 的位运算、位测试与位查询过程。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/bigint/bitwise "函数名"
```

## 函数分类索引

### 一、按位运算
- `bigint-and` - 按位与
- `bigint-or` - 按位或
- `bigint-xor` - 按位异或
- `bigint-not` - 按位取反

### 二、移位运算
- `bigint-arithmetic-shift` - 算术移位

### 三、位测试
- `bigint-bit-set?` - 测试指定位是否为 1
- `bigint-bit-set` - 置位指定位
- `bigint-bit-clear` - 清零指定位
- `bigint-bit-flip` - 翻转指定位

### 四、位查询
- `bigint-bit-count` - 统计值为 1 的位数
- `bigint-bit-length` - 最小二进制补码位数
- `bigint-lowest-bit-set` - 最低非零位下标
