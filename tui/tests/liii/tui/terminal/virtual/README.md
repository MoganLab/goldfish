# (liii tui terminal virtual) 模块函数索引

`(liii tui terminal virtual)` 提供纯内存的虚拟终端实现，用于脱机测试与 TUI 组件的确定性验证，支持屏幕内容快照、原始输出记录、输入按键队列模拟以及动态尺寸调整。

使用时导入：

```scheme
(import (liii tui terminal virtual))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-virtual-terminal
```

## 函数分类索引

### 虚拟终端创建与谓词
- `make-virtual-terminal` - 创建内存虚拟终端实例
- `virtual-terminal?` - 判断是否为虚拟终端实例

### 屏幕与原始输出
- `virtual-terminal-screen` - 获取当前屏幕内容行列表
- `virtual-terminal-raw` - 获取原始输出片段列表
- `virtual-terminal-resize!` - 动态调整虚拟终端宽高

### 输入队列模拟
- `virtual-terminal-push-key!` - 向输入队列尾部追加按键
- `virtual-terminal-pop-key!` - 从输入队列头部取出按键
- `virtual-terminal-unshift-key!` - 把按键插回输入队列头部
