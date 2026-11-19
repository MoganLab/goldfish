# (liii tui mouse) 模块函数索引

`(liii tui mouse)` 提供终端 SGR 模式鼠标事件的建模与解析，包括按键（按下/释放/单击/拖动）、滚轮方向、光标坐标（0-based）以及修饰键（Shift/Ctrl/Alt）状态的提取。

使用时导入：

```scheme
(import (liii tui mouse))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-mouse-event
```

## 函数分类索引

### 鼠标事件对象创建与谓词
- `make-mouse-event` - 构造鼠标事件对象
- `mouse-event?` - 判断是否为鼠标事件对象
- `mouse-event-click?` - 判断是否为单击事件

### 鼠标转义序列解析
- `parse-mouse-event` - 解析 SGR 鼠标 ANSI 序列
- `mouse-sequence?` - 检验是否为 SGR 鼠标 ANSI 序列

### 属性访问器
- `mouse-event-type` - 获取事件类型（'press, 'release, 'click, 'drag, 'wheel-up, 'wheel-down 等）
- `mouse-event-button` - 获取按钮名称（'left, 'middle, 'right, 'none 等）
- `mouse-event-x` - 获取 0-based 列坐标
- `mouse-event-y` - 获取 0-based 行坐标
- `mouse-event-shift?` - 是否按下 Shift 键
- `mouse-event-ctrl?` - 是否按下 Ctrl 键
- `mouse-event-alt?` - 是否按下 Alt 键
- `mouse-event-time` - 获取事件时间戳
