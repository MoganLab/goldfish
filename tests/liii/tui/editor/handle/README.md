# (liii tui editor handle) 模块函数索引

`(liii tui editor handle)` 提供编辑器的键盘事件分发处理原语，支持字符键入、导航键（左右/上下/首尾/单词跳转）、行内删除操作、撤销恢复与提交。

使用时导入：

```scheme
(import (liii tui editor handle))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc editor-handle-input
```

## 函数分类索引

### 输入处理
- `editor-handle-input` - 处理按键输入并返回消费状态
