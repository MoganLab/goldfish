# (liii tui snake) 模块函数索引

贪吃蛇 TUI 演示：游戏逻辑、视图渲染、布局与交互。

使用时导入：

```scheme
(import (liii tui snake logic))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc snake-tick
```

## 子模块导览

| 子模块 | 说明 |
|---|---|
| `(liii tui snake logic)` | 游戏状态记录与核心逻辑（移动、吃食、碰撞、转向、穿墙） |
| `(liii tui snake view)` | 视图渲染与自适应布局 |
| `(liii tui snake style)` | ANSI 配色与样式开关 |
| `(liii tui snake mouse)` | 鼠标点击转向 |

## 函数分类索引

### 游戏状态与逻辑（logic）
- `make-snake-game` / `snake-game?` - 游戏状态构造与谓词
- `snake-width` / `snake-height` - 游戏区域尺寸
- `snake-body` / `snake-head` - 蛇身与蛇头坐标
- `snake-dir` / `snake-turn` - 前进方向与转向
- `snake-tick` - 推进一帧（移动、吃食、碰撞判定）
- `snake-food` / `snake-spawn-food` - 食物位置与生成
- `snake-score` / `snake-high-score` - 分数与最高分
- `snake-status` / `snake-tick-count` - 游戏状态与帧计数
- `snake-pass-wall` - 穿墙坐标环绕
- `snake-normalize-key` / `snake-dir-opposite?` - 按键归一化与反向判定
- `snake-resize` - 调整游戏区域尺寸

### 视图与布局（view）
- `render-grid-lines` - 游戏状态渲染为网格行
- `make-stats-box` / `make-help-box` - 侧栏组件
- `make-game-view` / `make-game-over-view` - 整体视图组件
- `snake-compute-layout` / `snake-compute-dimensions` - 自适应布局计算

### 样式（style）
- `style-enabled?` / `style-enable!` / `style-disable!` / `with-color-mode` - 颜色开关
- `style-head` / `style-body` / `style-food` - 游戏元素配色
- `style-title` / `style-accent` / `style-dim` / `style-heartbeat` / `style-game-over` - 文本配色
- `style-reset` - ANSI 重置序列

### 鼠标交互（mouse）
- `snake-click->dir` - 点击坐标转向
- `snake-mouse-press->dir` - 鼠标事件转向
