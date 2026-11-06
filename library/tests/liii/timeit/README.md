# (liii timeit) 模块函数索引

`(liii timeit)` 是 Goldfish Scheme 的性能测试工具库：循环执行给定语句并测量其耗时（秒），常用于宏基准与微性能测试。

使用时导入：

```scheme
(import (liii timeit))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/timeit "函数名"
```

## 函数分类索引

### 性能测量
- `timeit` - 循环执行给定语句 number 次（默认 1000000 次），返回耗时秒数；可选 setup 过程在计时前执行一次
