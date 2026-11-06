# (liii tui terminal) 模块函数索引

`(liii tui terminal)` 统一导出终端抽象接口，聚合自 `(liii tui terminal protocol)`、`(liii tui terminal size)`、`(liii tui terminal standard)`、`(liii tui terminal virtual)` 与 `(liii tui terminal poll)` 子模块。

使用时导入：

```scheme
(import (liii tui terminal))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc terminal-write
```

## 子模块导览

| 子模块 | 说明 |
|---|---|
| `(liii tui terminal protocol)` | 终端抽象协议：尺寸、写入、光标、清屏、同步输出、模式切换 |
| `(liii tui terminal size)` | 终端尺寸探测 |
| `(liii tui terminal standard)` | 标准（真实）终端实现（ANSI 转义序列） |
| `(liii tui terminal virtual)` | 内存虚拟终端实现（脱机测试） |
| `(liii tui terminal virtual state)` | 虚拟终端内部状态与字符绘制原语 |
| `(liii tui terminal poll)` | 输入轮询、ANSI/UTF-8 解析与鼠标事件合成 |
| `(liii tui terminal key-map)` | 控制台按键码到按键符号的映射表 |

## 函数分类索引

### 终端记录与属性
- `make-terminal-record` - 创建终端协议记录实例
- `terminal?` - 判断是否为终端实例

### 尺寸与输出
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

### 尺寸探测
- `terminal-detect-size` - 探测终端窗口尺寸

### 标准终端
- `make-standard-terminal` - 创建标准终端实例
- `standard-terminal?` - 判断是否为标准终端实例

### 虚拟终端
- `make-virtual-terminal` - 创建内存虚拟终端实例
- `virtual-terminal?` - 判断是否为虚拟终端实例
- `virtual-terminal-screen` - 获取当前屏幕内容行列表
- `virtual-terminal-raw` - 获取原始输出片段列表
- `virtual-terminal-resize!` - 动态调整虚拟终端宽高
- `virtual-terminal-push-key!` - 向输入队列尾部追加按键
- `virtual-terminal-pop-key!` - 从输入队列头部取出按键
- `virtual-terminal-unshift-key!` - 把按键插回输入队列头部

### 输入轮询与粘贴
- `terminal-poll-key` - 非阻塞读取一个按键或鼠标事件
- `terminal-paste-mode?` - 查询粘贴模式开关状态
- `terminal-set-paste-mode!` - 开启或关闭粘贴模式
- `terminal-drain-mouse-events` - 一次性取走所有已到达的鼠标事件
