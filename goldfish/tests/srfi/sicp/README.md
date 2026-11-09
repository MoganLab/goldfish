# (srfi sicp) 模块函数分类索引

`(srfi sicp)` 是《Structure and Interpretation of Computer Programs》中使用的经典常量与运行时长查询的快捷入口，底层实现来自 `(srfi srfi-216)`。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/sicp "函数名"
```

## 函数分类索引

### 一、SICP 风格常量
- `true` - 真值常量，等价于 `#t`
- `false` - 假值常量，等价于 `#f`
- `nil` - 空列表常量，等价于 `'()`

### 二、运行时长
- `runtime` - 返回解释器运行时长（毫秒）

## 说明

本库为纯重导出封装，按函数拆分的单元测试统一放在其实现库 `tests/srfi/srfi-216/` 目录。
