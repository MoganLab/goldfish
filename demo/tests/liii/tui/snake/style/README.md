# (liii tui snake style) 模块函数索引

`(liii tui snake style)` 提供贪吃蛇 TUI 渲染的 ANSI 配色与样式开关，支持 NO_COLOR 降级。

使用时导入：

```scheme
(import (liii tui snake style))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc style-head
```

## 函数分类索引

### 开关控制
- `style-enabled?` - 颜色开关当前状态
- `style-enable!` / `style-disable!` - 打开/关闭颜色
- `with-color-mode` - 指定颜色模式执行过程并自动恢复

### 游戏元素配色
- `style-head` / `style-body` / `style-food` - 蛇头/蛇身/食物配色

### 文本配色
- `style-title` / `style-accent` / `style-dim` / `style-heartbeat` / `style-game-over` - 标题/强调/弱化/心跳/结束配色
- `style-reset` - ANSI 重置序列
