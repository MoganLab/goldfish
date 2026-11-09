# (liii tui snake logic) 模块函数索引

`(liii tui snake logic)` 提供贪吃蛇游戏状态记录与核心逻辑：移动推进、吃食增长、碰撞判定、转向保护、穿墙环绕与区域调整。

使用时导入：

```scheme
(import (liii tui snake logic))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc snake-tick
```

## 函数分类索引

### 游戏状态
- `make-snake-game` - 创建游戏状态
- `snake-game?` - 游戏状态谓词
- `snake-width` / `snake-height` - 游戏区域尺寸
- `snake-body` / `snake-head` - 蛇身与蛇头坐标
- `snake-dir` - 当前前进方向
- `snake-food` - 食物坐标
- `snake-score` / `snake-high-score` - 分数与最高分
- `snake-status` - 游戏状态（running / paused / game-over）
- `snake-tick-count` - 已推进帧数

### 核心逻辑
- `snake-tick` - 推进一帧（移动、吃食、碰撞判定）
- `snake-turn` - 转向（禁止 180 度掉头）
- `snake-spawn-food` - 在避开蛇身处生成食物
- `snake-resize` - 调整游戏区域尺寸
- `snake-pass-wall` - 穿墙坐标环绕
- `snake-normalize-key` - 按键归一化
- `snake-dir-opposite?` - 反向方向判定
