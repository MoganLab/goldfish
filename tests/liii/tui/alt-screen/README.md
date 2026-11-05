# (liii tui alt-screen) 模块函数索引

`(liii tui alt-screen)` 提供全屏终端表面的整帧渲染驱动，支持行差分增量重绘、终端同步刷新（sync block）、光标定位与全量重绘机制。

使用时导入：

```scheme
(import (liii tui alt-screen))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-alt-screen
```

## 函数分类索引

### 备用屏创建与属性
- `make-alt-screen` - 创建备用屏驱动实例
- `alt-screen?` - 判断是否为备用屏驱动实例
- `alt-screen-terminal` - 获取底层关联终端
- `alt-screen-lines` - 获取当前帧文本行列表

### 渲染与重绘控制
- `alt-screen-render!` - 渲染文本帧并更新光标
- `alt-screen-normalize` - 归一化帧内容（按宽度裁剪、按高度补齐）
- `alt-screen-clear!` - 清除终端屏幕与帧缓存
- `alt-screen-invalidate!` - 使缓存失效以强制重绘
- `alt-screen-refresh!` - 全量重绘当前帧
