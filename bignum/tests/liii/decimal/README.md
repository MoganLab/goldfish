# (liii decimal) 模块函数分类索引

decimal 提供不可变的十进制定点数，基于 (liii bigint) 实现任意精度，聚合各子模块的导出。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal "函数名"
```

## 函数分类索引

### 一、类型与访问器
- `decimal?` - 判断是否为 decimal
- `make-decimal` - 创建 decimal（由 record 提供）
- `decimal-unscaled-value` - 获取非标度值
- `decimal-scale` - 获取 scale
- `decimal-precision` - 获取精度
- `decimal-signum` - 获取符号
- `decimal-ulp` - 获取最后一位单位

### 二、算术运算
- `decimal-add` - 加法
- `decimal-subtract` - 减法
- `decimal-multiply` - 乘法
- `decimal-divide` - 除法
- `decimal-remainder` - 余数
- `decimal-divide-and-remainder` - 商与余数
- `decimal-divide-to-integral-value` - 整数商
- `decimal-negate` - 取负
- `decimal-abs` - 绝对值
- `decimal-pow` - 幂
- `decimal-sqrt` - 平方根
- `decimal-plus` - 恒等运算

### 三、舍入
- `decimal-set-scale` - 设置 scale
- `decimal-round` - 按 scale 与模式舍入
- `decimal-scale-by-power-of-ten` - 缩放 10 的幂
- `decimal-strip-trailing-zeros` - 去除尾随零
- `decimal-floor` - 向下取整
- `decimal-ceiling` - 向上取整
- `decimal-truncate` - 截断取整
- `decimal-round-up` 等舍入模式常量

### 四、比较
- `decimal=?` `decimal<?` `decimal>?` `decimal<=?` `decimal>=?`（由 compare 提供）
- `decimal-max` - 最大值
- `decimal-min` - 最小值
- `decimal-equals?` - 数值与 scale 全等比较
- `decimal-hash-code` - 哈希值（由 compare 提供）
- `decimal-zero?` `decimal-positive?` `decimal-negative?`

### 五、MathContext 运算
- `decimal-add-with-context` 等带上下文运算
- `make-math-context` 等（由 math-context 提供）
