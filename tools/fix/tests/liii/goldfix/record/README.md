# (liii goldfix record) 模块函数索引

`(liii goldfix record)` 提供括号修正过程中的记录类型定义：token（带位置信息的记号）、按物理行组织的行记录、左括号环境、待确认的右括号、源码改动 edit 以及修复结果 report。

使用时导入：

```scheme
(import (liii goldfix record))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix/record "函数名"
```

## 函数分类索引

### token 记录
- `make-fix-token` - 创建带位置信息的 token 记录
- `fix-token?` - 判断一个值是否为 fix-token 记录
- `fix-token-type` - 读取 token 的类型符号
- `fix-token-offset` - 读取 token 在源码中的起始偏移量
- `fix-token-end` - 读取 token 在源码中的结束偏移量
- `fix-token-line` - 读取 token 所在的行号
- `fix-token-column` - 读取 token 所在的列号
- `fix-token-text` - 读取 token 对应的源码文本

### 行记录
- `make-fix-line` - 创建按物理行组织后的 token 行记录
- `fix-line?` - 判断一个值是否为 fix-line 记录
- `fix-line-number` - 读取行记录的行号
- `fix-line-start-offset` - 读取行记录在源码中的起始偏移量
- `fix-line-first-code-token` - 读取行记录中第一枚 code token
- `fix-line-tokens` - 读取行记录中的 token 列表

### 左括号环境
- `make-open-frame` - 创建一个仍然打开的左括号环境记录
- `open-frame?` - 判断一个值是否为 open-frame 记录
- `open-frame-offset` - 读取左括号在源码中的偏移量
- `open-frame-line` - 读取左括号所在的行号
- `open-frame-column` - 读取左括号所在的列号
- `open-frame-tag-name` - 读取左括号对应的标记名称

### 待确认的右括号
- `make-pending-close` - 创建一个等待下一行缩进确认的右括号记录
- `pending-close?` - 判断一个值是否为 pending-close 记录
- `pending-close-frame` - 读取右括号所属的 open-frame 记录
- `pending-close-offset` - 读取右括号在源码中的起始偏移量
- `pending-close-end` - 读取右括号在源码中的结束偏移量
- `pending-close-line` - 读取右括号所在的行号
- `pending-close-column` - 读取右括号所在的列号

### 源码改动
- `make-fix-edit` - 创建一条描述源码改动的 edit 记录
- `fix-edit?` - 判断一个值是否为 fix-edit 记录
- `fix-edit-kind` - 读取 edit 的类型符号
- `fix-edit-offset` - 读取 edit 的插入位置偏移量
- `fix-edit-start` - 读取 edit 删除区间的起始偏移量
- `fix-edit-end` - 读取 edit 删除区间的结束偏移量
- `fix-edit-text` - 读取 edit 插入的文本
- `fix-edit-reason` - 读取 edit 的修改原因
- `fix-edit-open-offset` - 读取 edit 对应左括号的偏移量

### 修复结果
- `make-repair-report` - 创建一条汇总括号修复结果的 report 记录
- `repair-report?` - 判断一个值是否为 repair-report 记录
- `repair-report-ok?` - 读取 report 中括号是否平衡的标志
- `repair-report-edits` - 读取 report 中修复产生的 edit 列表
- `repair-report-diagnostics` - 读取 report 中的诊断信息列表
