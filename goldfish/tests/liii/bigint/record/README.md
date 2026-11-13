# (liii bigint record) 模块函数分类索引

bigint record 提供 bigint 的基础记录类型定义，是 (liii bigint) 的底层实现基础。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/bigint/record "函数名"
```

## 函数分类索引

### 一、类型判断
- `bigint?` - 判断是否为 bigint

### 二、构造函数
- `make-bigint` - 创建 bigint

### 三、访问器
- `bigint-signum` - 获取符号 (-1, 0, 1)
- `bigint-mag` - 获取 magnitude 数组

### 四、工具函数
- `bigint-normalize` - 规范化 bigint
- `strip-leading-zeros` - 去除前导零

### 五、常量
- `BIGINT-BASE` - 基数 (2^32)
- `BIGINT-BITS` - 每个数字的位数 (32)
