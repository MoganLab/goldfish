# (liii pi code all-in-one boundary) 模块函数分类索引

`(liii pi code all-in-one boundary)` 在跨模型交接前清洗会话尾部：回滚报错、中止、截断、空内容或悬空调用的 assistant 消息，保留用户消息、完整回复和已经执行完毕的 tool result。

使用时导入：

```scheme
(import (liii pi code all-in-one boundary))
```

## 查看函数文档

```bash
gf doc pi-boundary-recover-messages
```

## 函数分类索引

- `pi-boundary-recover-messages` - 回滚会话尾部不完整 assistant 消息

## 导出边界

本库只公开 `pi-boundary-recover-messages`。消息角色读取、stopReason 读取、内容归一
（字符串包装为单元素列表、向量转列表）与「是否可交接」判断都是实现细节，由
`pi-boundary-recover-messages-test.scm` 的回滚断言间接覆盖。

## 交接不变量

尾部 assistant 满足下列任一条件时不可交给下一个节点，必须回滚到前一条可用消息：

- `errorMessage` 非空；
- `stopReason` 为 `error`、`aborted`、`length` 或 `toolUse`；
- 内容为空（空内容或没有内容块）。

回滚逐条进行，直到尾部是用户消息、已完成的 `toolResult`，或内容完整的 assistant。
工具本身的失败结果（`isError` 为真）仍视为已完成，保留在上下文中。
