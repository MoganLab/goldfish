# (liii tui editor view) 模块函数索引

`(liii tui editor view)` 提供行编辑器的文本行渲染、光标视觉位置折行换算、软件光标（反显/竖线/方块）绘制与有效排版宽度计算。

使用时导入：

```scheme
(import (liii tui editor view))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc render-editor-lines
```

## 函数分类索引

### 视觉排版与渲染
- `render-editor-lines` - 渲染编辑器文本行（带光标样式与占位符）
- `render-line-with-cursor` - 绘制单行文本上的软件光标
- `editor-visual-cursor` - 将逻辑光标坐标换算为折行后的视觉行与显示列
- `%editor-layout-width` - 计算扣除光标预留列后的有效排版宽度
