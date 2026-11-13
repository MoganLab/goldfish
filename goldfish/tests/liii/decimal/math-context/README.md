# (liii decimal math-context) 模块函数分类索引

math-context 提供精度与舍入模式的上下文记录，用于约束算术运算结果。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal/math-context "函数名"
```

## 函数分类索引

### 一、类型与构造
- `math-context?` - 判断是否为 math-context
- `make-math-context` - 创建 math-context

### 二、访问器
- `math-context-precision` - 获取精度
- `math-context-rounding-mode` - 获取舍入模式

### 三、标准常量
- `math-context-decimal32` - 7 位精度，half-even
- `math-context-decimal64` - 16 位精度，half-even
- `math-context-decimal128` - 34 位精度，half-even
- `math-context-unlimited` - 无限精度，unnecessary

### 四、辅助函数
- `math-context-valid-rounding-mode?` - 判断舍入模式是否有效
