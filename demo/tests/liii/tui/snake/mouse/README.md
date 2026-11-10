# (liii tui snake mouse) 模块函数索引

`(liii tui snake mouse)` 提供贪吃蛇的鼠标交互：把点击屏幕坐标换算为转向方向。

使用时导入：

```scheme
(import (liii tui snake mouse))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc snake-click->dir
```

## 函数分类索引

### 鼠标转向
- `snake-click->dir` - 点击屏幕坐标 -> 转向方向
- `snake-mouse-press->dir` - 鼠标事件（左键 press）-> 转向方向
