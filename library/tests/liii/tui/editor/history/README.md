# (liii tui editor history) 模块函数索引

`(liii tui editor history)` 提供命令行/编辑器输入历史管理，支持历史条目去重、容量修剪、草稿暂存以及双向导航（prev/next）。

使用时导入：

```scheme
(import (liii tui editor history))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-history
```

## 函数分类索引

### 历史记录创建与状态
- `make-history` - 创建历史管理器实例
- `history?` - 判断是否为历史管理器实例
- `history-entries` - 获取历史条目列表
- `history-max-size` - 获取最大条目限制
- `history-index` - 获取当前浏览位置索引
- `history-draft` - 获取暂存草稿

### 记录变动与导航
- `history-push!` - 追加新记录
- `history-prev` - 回溯更早记录
- `history-next` - 回退较新记录
- `history-reset!` - 复位浏览状态
