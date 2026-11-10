# (srfi srfi-78) 模块函数分类索引

`(srfi srfi-78)` 提供了轻量级测试框架（Lightweight testing），支持即开即用的断言、多种报告模式与测试状态统计。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-78 "函数名"
```

## 函数分类索引

### 一、断言与检查
- `check` - 对表达式求值结果进行等值或自定义比较断言

### 二、模式与状态控制
- `check-set-mode!` - 设置测试运行模式（off/summary/report-failed/report）
- `check-reset!` - 重置断言成功计数与失败记录
- `check-passed?` - 检查是否全胜且断言总数等于期望值
- `check-failed?` - 检查当前是否存在失败断言
- `check-report` - 输出断言汇总结果
