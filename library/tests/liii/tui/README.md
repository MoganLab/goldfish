# (liii tui) 模块导览

`(liii tui)` 是 Goldfish Scheme 终端用户界面（TUI）的顶层聚合库，整合了组件模型、文本测量、终端抽象、屏幕驱动、容器布局、滚动视口、动画加载器、多行编辑器、鼠标交互、模糊匹配、下拉选择与系统剪贴板等核心模块。

使用时导入：

```scheme
(import (liii tui))
```

## 子模块导览

| 子模块 | 说明 |
|---|---|
| `(liii tui component)` | TUI 核心组件协议与基础文本/空白占位组件 |
| `(liii tui text-width)` | ANSI 控制序列过滤与视觉字符宽度计算 |
| `(liii tui terminal)` | 终端抽象层（标准终端、虚拟终端与 ANSI/VT 协议支持） |
| `(liii tui main-screen)` | 主屏双缓冲差异比对（Diff）与增量渲染驱动 |
| `(liii tui alt-screen)` | 备用屏（Alt Screen）独立全屏驱动 |
| `(liii tui box)` | 高级排版容器（边框 Box、VStack、HStack、动态 Container 与 MouseRegion） |
| `(liii tui scroll-view)` | 文本/组件滚动视口引擎 |
| `(liii tui loader)` | 转轮动画（Spinner）与状态指示组件 |
| `(liii tui editor)` | 单行/多行文本编辑框、撤销重做与粘贴处理 |
| `(liii tui mouse)` | 鼠标事件（按下/释放/拖拽/滚轮/点击）协议与解析 |
| `(liii tui fuzzy)` | 模糊搜索匹配与排序过滤 |
| `(liii tui select-list)` | 交互式单选列表组件 |
| `(liii tui clipboard)` | OSC 52 与系统原生剪贴板跨终端复制能力 |

## 查看函数文档

各导出函数的具体用法与测试用例参见各子模块目录下的测试文件。可通过如下命令查看指定函数文档：

```bash
gf doc <函数名>
```
