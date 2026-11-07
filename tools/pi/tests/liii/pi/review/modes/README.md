# (liii pi review modes) 模块函数分类索引

`(liii pi review modes)` 提供代码审查工作流的编排与调度逻辑。

使用时导入：

```scheme
(import (liii pi review modes))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc run-code-review-flow
```

## 函数分类索引

### 工作流入口
- `run-code-review-flow` - 执行代码审查流程，支持终端展示与 JSON 事件两种模式
