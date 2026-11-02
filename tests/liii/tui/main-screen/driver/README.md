# (liii tui main-screen driver) 模块函数索引

`(liii tui main-screen driver)` 提供主屏幕（inline 场景）终端表面的驱动实现，负责差分渲染、终端同步刷新（sync block）、光标定位与可见性管理、尺寸自适应以及内容收缩时的全量重绘策略。

使用时导入：

```scheme
(import (liii tui main-screen driver))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-main-screen
```

## 函数分类索引

### 屏幕创建与属性访问
- `make-main-screen` - 创建主屏幕驱动实例
- `main-screen?` - 判断是否为主屏幕驱动实例
- `screen-terminal` - 获取底层关联终端
- `screen-buffer` - 获取内部行缓冲区
- `screen-lines` - 获取当前帧文本行列表

### 渲染与重绘控制
- `screen-render!` - 差分渲染帧内容并定位光标
- `screen-refresh!` - 清屏并全量重绘当前帧
- `screen-invalidate!` - 使内部缓存失效以强制重绘
- `screen-resize!` - 调整终端几何尺寸并全量重绘
- `screen-clear!` - 清空终端表面与帧缓存

### 光标可见性控制
- `screen-auto-hide-cursor?` - 查询是否自动隐藏光标
- `screen-auto-hide-cursor-set!` - 设置是否自动隐藏光标
