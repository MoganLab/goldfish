# (liii tui editor viewport) 模块函数索引

`(liii tui editor viewport)` 提供编辑器多行文本在有限高度内的视口计算与滚动同步，支持光标居中跟随与隐藏行数提示。

使用时导入：

```scheme
(import (liii tui editor viewport))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc %ed-viewport
```

## 函数分类索引

### 视口计算与同步
- `%ed-sync-viewport!` - 同步视口起始行以保持光标可见
- `%ed-viewport` - 获取当前视口参数（起始行/可见行/上下隐藏行）
