# (liii tui snake view) 模块函数索引

`(liii tui snake view)` 提供贪吃蛇的视图渲染：网格渲染、侧栏组件、整体布局与游戏结束视图，并按终端尺寸自适应。

使用时导入：

```scheme
(import (liii tui snake view))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-game-view
```

## 函数分类索引

### 渲染
- `render-grid-lines` - 游戏状态渲染为网格行

### 组件构造
- `make-stats-box` - 分数/状态侧栏组件
- `make-help-box` - 操作说明侧栏组件
- `make-game-view` - 整体视图组件（自适应布局）
- `make-game-over-view` - 游戏结束视图组件

### 布局计算
- `snake-compute-layout` - 按终端尺寸计算布局参数
- `snake-compute-dimensions` - 按终端尺寸计算游戏区尺寸
