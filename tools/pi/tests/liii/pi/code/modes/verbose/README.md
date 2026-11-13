# (liii pi code modes verbose) 模块函数分类索引

`(liii pi code modes verbose)` 提供人类友好的 verbose 流式输出模式（带 emoji 与轮次/工具标记），面向终端与脚本运行。

使用时导入：

```scheme
(import (liii pi code modes verbose))
```

## 查看函数文档

```bash
gf doc display-agent-event
gf doc display-verbose-event
gf doc make-verbose-emitter
gf doc handle-verbose-result
```

## 函数分类索引

### 事件显示
- `display-agent-event` - 格式化输出带 emoji 和轮次标记的 Agent 事件
- `display-verbose-event` - 便捷调用 display-agent-event 格式化 Agent 事件

### 流程集成
- `make-verbose-emitter` - 构造 verbose 事件发射器过程
- `handle-verbose-result` - 处理 verbose 模式最终结果并返回退出状态码
