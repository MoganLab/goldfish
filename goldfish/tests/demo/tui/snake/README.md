# (demo tui snake) 模块函数分类索引

贪吃蛇 TUI 演示：游戏逻辑、视图渲染、布局与交互。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、游戏逻辑（demo/tui/snake/logic.scm）
- `make-snake-game` - 创建游戏状态
- `snake-game?` - 游戏状态谓词
- `snake-width` / `snake-height` - 游戏区域尺寸
- `snake-body` / `snake-head` - 蛇身与蛇头坐标
- `snake-dir` / `snake-turn` - 前进方向与转向
- `snake-tick` - 推进一帧（移动、吃食、碰撞判定）
- `snake-food` / `snake-spawn-food` - 食物位置与生成
- `snake-score` / `snake-high-score` - 分数与最高分
- `snake-status` - 游戏状态（running / game-over）
- `snake-tick-count` - 已推进帧数
- `snake-pass-wall` - 穿墙模式开关
- `snake-normalize-key` / `snake-dir-opposite?` - 按键归一化与反向判定
- `snake-resize` - 调整游戏区域尺寸

### 二、视图与样式（demo/tui/snake/view.scm、style.scm）
- `snake-game->view` - 游戏状态渲染为 TUI 组件
- `snake-style` - 配色与样式定义
