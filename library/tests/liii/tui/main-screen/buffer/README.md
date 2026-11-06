# (liii tui main-screen buffer) 模块函数索引

`(liii tui main-screen buffer)` 提供主屏幕的行缓冲数据结构，记录当前帧与上一帧的文本行内容，并维护脏标记以支持全量重绘与增量差分。

使用时导入：

```scheme
(import (liii tui main-screen buffer))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-screen-buffer
```

## 函数分类索引

### 缓冲区创建
- `make-screen-buffer` - 创建屏幕行缓冲区实例
- `screen-buffer?` - 判断是否为屏幕行缓冲区实例

### 帧内容访问
- `screen-buffer-lines` - 获取当前帧文本行列表
- `screen-buffer-lines-set!` - 设置当前帧文本行列表
- `screen-buffer-prev-lines` - 获取上一帧文本行列表
- `screen-buffer-prev-lines-set!` - 设置上一帧文本行列表

### 脏标记与帧提交
- `screen-buffer-dirty?` - 判断缓冲区是否为脏状态
- `screen-buffer-dirty-set!` - 设置缓冲区的脏标记
- `screen-buffer-invalidate!` - 将缓冲区标记为脏状态
- `screen-buffer-commit!` - 提交新的一帧内容
- `screen-buffer-clear!` - 清空当前帧内容

### 渲染目标规范化
- `screen-normalize-lines` - 将渲染目标规范化为文本行列表
