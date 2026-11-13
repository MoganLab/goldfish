# (liii decimal record) 模块函数分类索引

decimal record 提供 decimal 的记录类型定义与规范化。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal/record "函数名"
```

## 函数分类索引

### 一、类型与构造
- `decimal?` - 判断是否为 decimal
- `make-decimal` - 创建 decimal
- `make-decimal-with-scale` - 按指定 scale 创建 decimal

### 二、访问器
- `decimal-unscaled` - 获取非标度值
- `decimal-scale` - 获取 scale
