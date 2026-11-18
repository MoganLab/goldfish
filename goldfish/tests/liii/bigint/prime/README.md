# (liii bigint prime) 模块函数分类索引

bigint prime 提供 bigint 的素数检测、下一素数寻找与随机 bigint 生成。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc liii/bigint/prime "函数名"
```

## 函数分类索引

### 一、素数操作
- `bigint-prime?` - Miller-Rabin 概率性素数检测
- `bigint-next-prime` - 寻找大于给定 bigint 的下一个素数

### 二、随机生成
- `make-random-bigint` - 生成指定位数的随机正 bigint
