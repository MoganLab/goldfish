# (liii tui terminal protocol) 模块函数索引

`(liii tui terminal protocol)` 定义终端抽象协议与基础协议实现，覆盖尺寸查询、文本写入、光标定位与显隐、清屏、同步输出块（sync block）、raw 模式切换、备用屏切换以及鼠标上报模式开关。

使用时导入：

```scheme
(import (liii tui terminal protocol))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-terminal-record
```

## 函数分类索引

### 终端记录构建与属性
- `make-terminal-record` - 创建终端协议记录实例
- `terminal?` - 判断是否为终端实例
- `terminal-type` - 获取终端类型标识
- `terminal-data` - 获取终端实例的私有数据
- `terminal-data-set!` - 设置终端实例的私有数据

### 尺寸与文本输出
- `terminal-size` - 查询终端尺寸
- `terminal-write` - 向终端写入文本
- `terminal-flush` - 刷新终端输出缓冲

### 光标控制
- `terminal-cursor` - 查询光标位置
- `terminal-set-cursor!` - 设置光标位置
- `terminal-cursor-visible?` - 查询光标可见性
- `terminal-hide-cursor!` - 隐藏光标
- `terminal-show-cursor!` - 显示光标
- `terminal-max-y` - 查询曾渲染到达的最大行索引

### 清屏与同步输出
- `terminal-clear` - 清空整个终端表面
- `terminal-clear-line` - 清空当前行
- `terminal-clear-line-to-end` - 清除光标到行尾
- `terminal-begin-sync!` - 开启终端同步输出
- `terminal-end-sync!` - 结束终端同步输出

### 终端模式切换
- `terminal-enter-raw-mode!` - 进入 raw 模式
- `terminal-exit-raw-mode!` - 退出 raw 模式
- `terminal-enter-alt-screen!` - 进入备用屏
- `terminal-exit-alt-screen!` - 退出备用屏
- `terminal-mouse-mode!` - 开启/关闭鼠标事件上报
