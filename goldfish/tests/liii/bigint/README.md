# (liii bigint) 模块函数分类索引

bigint 提供不可变的任意精度整数接口，聚合各子模块的导出。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/bigint "函数名"
```

## 函数分类索引

### 一、构造函数
- `string->bigint` - 从十进制字符串构造（由 conversion 提供）
- `integer->bigint` - 从普通整数构造（由 conversion 提供）
- `make-random-bigint` - 构造随机 bigint

### 二、算术运算
- `bigint-add` - 加法
- `bigint-subtract` - 减法
- `bigint-multiply` - 乘法
- `bigint-divide` - 除法
- `bigint-remainder` - 余数
- `bigint-divide-and-remainder` - 商与余数
- `bigint-quotient` - 商
- `bigint-modulo` - 模
- `bigint-mod` - 模（mod-expt 的别名风格）
- `bigint-mod-expt` - 模幂
- `bigint-mod-inverse` - 模逆
- `bigint-negate` - 取负
- `bigint-abs` - 绝对值
- `bigint-gcd` - 最大公约数
- `bigint-expt` - 幂

### 三、开方与根
- `bigint-sqrt` - 平方根
- `bigint-sqrt-and-remainder` - 平方根与余数
- `bigint-rootn` - n 次方根
- `bigint-rootn-and-remainder` - n 次方根与余数

### 四、素数
- `bigint-prime?` - 素数判断
- `bigint-next-prime` - 下一个素数
