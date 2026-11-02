# (liii tui component) 模块函数索引

`(liii tui component)` 提供终端用户界面基础组件抽象契约协议，包含渲染、按键输入处理、鼠标事件处理与脏区刷新，并内置文本自动折行、单行截断文本与空白占位等常用基础组件。

使用时导入：

```scheme
(import (liii tui component))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-component
```

## 函数分类索引

### 核心契约与组件构建
- `make-component` - 创建基础组件对象
- `component?` - 判断是否为合法组件
- `component-render` - 将组件渲染为文本行列表
- `component-handle-input` - 向组件分发键盘输入
- `component-handle-mouse!` - 向组件分发鼠标事件
- `component-invalidate!` - 标记组件脏区或刷新内部状态
- `component-data` - 获取组件的内部私有数据
- `component-data-set!` - 设置组件的内部私有数据

### 内置通用基础组件
- `make-text` - 自动折行文本组件
- `make-truncated-text` - 单行截断文本组件
- `make-spacer` - 空白占位组件
