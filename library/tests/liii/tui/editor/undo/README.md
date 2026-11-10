# (liii tui editor undo) 模块函数索引

`(liii tui editor undo)` 提供行编辑器的撤销机制，支持状态快照捕获、连续打字合并单元与单步回滚。

使用时导入：

```scheme
(import (liii tui editor undo))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc %editor-snapshot
```

## 函数分类索引

### 撤销操作原语
- `%editor-snapshot` - 捕获当前状态快照
- `%editor-undo-type!` - 打字输入合并记录
- `%editor-undo!` - 执行单步撤销
- `%editor-undo-clear!` - 清空撤销历史
