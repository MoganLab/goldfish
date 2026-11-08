# (liii bitwise) 模块函数分类索引

`(liii bitwise)` 提供按位逻辑、移位、位字段和位查询等操作。
适合处理标志位、权限掩码、压缩状态和底层协议编码。
本库基于 `(srfi srfi-151)`，提供了更简洁的 `bitwise-or` 别名，并重导出了 SRFI-151 的全部位运算函数及 S7 基础位运算接口。

SRFI-151 相关函数的文档与单元测试位于 [../../srfi/srfi-151/README.md](../../srfi/srfi-151/README.md)。

## 常见用法示例

```scheme
(import (liii bitwise))

;; 示例1：组合多个位掩码
(bitwise-or 1 2 4) ; => 7

;; 示例2：通过移位快速调整数值
(arithmetic-shift 3 2) ; => 12

;; 示例3：检查某一位是否被设置
(bit-set? 3 10) ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/bitwise "函数名"
```

## 函数分类索引

### 一、按位逻辑
- `bitwise-or` - 多参数按位或操作
