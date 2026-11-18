# (liii decimal rounding) 模块函数分类索引

decimal rounding 提供 decimal 的舍入模式常量、精度计算与各种舍入操作。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal/rounding "函数名"
```

## 函数分类索引

### 一、舍入模式常量
- `decimal-round-up` - 远离零方向舍入模式
- `decimal-round-down` - 向零方向舍入模式
- `decimal-round-ceiling` - 向正无穷方向舍入模式
- `decimal-round-floor` - 向负无穷方向舍入模式
- `decimal-round-half-up` - 四舍五入模式
- `decimal-round-half-down` - 五舍六入模式
- `decimal-round-half-even` - 银行家舍入模式
- `decimal-round-unnecessary` - 无需舍入模式

### 二、精度
- `decimal-precision` - 获取精度（非标度值的有效位数）

### 三、舍入操作
- `decimal-set-scale` - 设置 scale 并按模式舍入
- `decimal-round` - 按 scale 与模式舍入
- `decimal-strip-trailing-zeros` - 去除尾随零
- `decimal-floor` - 向下取整
- `decimal-ceiling` - 向上取整
- `decimal-truncate` - 截断取整
