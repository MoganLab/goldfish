# (liii decimal arithmetic) 模块函数分类索引

decimal arithmetic 提供 decimal 的算术运算，包括四则运算、余数、符号判断与数值比较。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal/arithmetic "函数名"
```

## 函数分类索引

### 一、基本信息
- `decimal-unscaled-value` - 获取非标度值
- `decimal-signum` - 获取符号
- `decimal-ulp` - 获取最后一位单位

### 二、四则运算
- `decimal-add` - 加法
- `decimal-subtract` - 减法
- `decimal-multiply` - 乘法
- `decimal-divide` - 除法
- `decimal-remainder` - 余数
- `decimal-divide-and-remainder` - 商与余数
- `decimal-divide-to-integral-value` - 整数商
- `decimal-plus` - 恒等运算

### 三、缩放
- `decimal-move-point-left` - 小数点左移
- `decimal-move-point-right` - 小数点右移
- `decimal-scale-by-power-of-ten` - 缩放 10 的幂

### 四、符号与判断
- `decimal-negate` - 取负
- `decimal-abs` - 绝对值
- `decimal-max` - 最大值
- `decimal-min` - 最小值
- `decimal-exact-integer?` - 是否为精确整数
- `decimal-zero?` - 是否为零
- `decimal-positive?` - 是否为正
- `decimal-negative?` - 是否为负
- `decimal-equals?` - 数值与 scale 全等比较
