# (liii tui main-screen) 模块函数索引

`(liii tui main-screen)` 统一导出主屏幕相关的驱动、行缓冲与差分算法接口，聚合自 `(liii tui main-screen driver)`、`(liii tui main-screen buffer)` 和 `(liii tui main-screen diff)` 子模块。

主屏幕面向 inline 场景：内容行数可以超过终端高度，由终端自身滚动；差分渲染只重写变化的行，并通过终端同步输出（sync block）避免闪烁。

使用时导入：

```scheme
(import (liii tui main-screen))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-main-screen
```

## 子模块导览

| 子模块 | 说明 |
|---|---|
| `(liii tui main-screen driver)` | 主屏幕终端表面驱动：差分渲染、刷新、缩放、清屏 |
| `(liii tui main-screen buffer)` | 帧行缓冲区：当前帧/上一帧内容与脏标记管理 |
| `(liii tui main-screen diff)` | 行级差分算法：keep/update/append/clear 操作生成 |

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

### 帧行缓冲区
- `make-screen-buffer` - 创建屏幕行缓冲区实例
- `screen-buffer?` - 判断是否为屏幕行缓冲区实例
- `screen-buffer-lines` - 获取当前帧文本行列表
- `screen-buffer-lines-set!` - 设置当前帧文本行列表
- `screen-buffer-prev-lines` - 获取上一帧文本行列表
- `screen-buffer-prev-lines-set!` - 设置上一帧文本行列表
- `screen-buffer-dirty?` - 判断缓冲区是否为脏状态
- `screen-buffer-dirty-set!` - 设置缓冲区的脏标记
- `screen-buffer-invalidate!` - 将缓冲区标记为脏状态
- `screen-buffer-commit!` - 提交新的一帧内容
- `screen-buffer-clear!` - 清空当前帧内容

### 差分算法
- `screen-diff` - 比较两帧文本行并生成差分操作
- `screen-dirty-diff` - 脏状态下的全量更新差分
- `diff-op-type` - 获取差分操作类型
- `diff-op-index` - 获取差分操作作用的行索引
- `diff-op-text` - 获取差分操作携带的文本内容
- `screen-diff-has-changes?` - 判断差分序列是否存在内容变化
