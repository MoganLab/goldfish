# (liii decimal string) 模块函数分类索引

decimal string 负责 decimal 与字符串之间的转换，包括解析与多种格式化输出。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/decimal/string "函数名"
```

## 函数分类索引

### 一、解析
- `string->decimal` - 从字符串解析构造 decimal

### 二、格式化输出
- `decimal->string` - 转换为字符串（自动选择表示法）
- `decimal->plain-string` - 无指数表示
- `decimal->engineering-string` - 工程记数法
- `decimal->scientific-string` - 科学记数法
